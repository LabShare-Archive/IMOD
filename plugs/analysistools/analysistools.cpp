/*
 *  analysistools.cpp -- Special plugin for dealing with sections and slices
 *
 */

/*****************************************************************************
 *   Copyright (C) 2007 by Andrew Noske from the Institute for Molecular     *
 *   Bioscience at the University of Queensland (Australia)                  *
 *****************************************************************************/

/*  $Author$

    $Date$

    $Revision$

    $Log: analysistools.cpp,v $
    Revision 1.4  2011/01/08 01:03:37  tempuser
    *** empty log message ***

    Revision 1.3  2010/10/18 22:41:56  tempuser
    Minor changes only

    Revision 1.2  2010/10/18 22:28:37  tempuser
    Improved and renamed qt_dialog_customizable to customdialog

    Revision 1.1  2009/11/02 08:45:41  tempuser
    first upload of AnalysisTools

    
    Revision 0.0  2008/07/20 15:45:41  noske
    Made special module to be used in IMOD

*/

//############################################################

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <qvariant.h>
#include <qaction.h>
#include <qapplication.h>
#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qlabel.h>
#include <qcombobox.h>
#include <QButtonGroup>
#include <qradiobutton.h>
#include <qdialog.h>
#include <qspinbox.h>
#include <qlayout.h>
#include <qgroupbox.h>
#include <qtooltip.h>
#include <qstringlist.h>
#include <qmessagebox.h>
#include <qinputdialog.h>
//Added by qt3to4:
#include <QVBoxLayout>
#include <QWheelEvent>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QGridLayout>
#include <QKeyEvent>
#include <QEvent>
#include <QHBoxLayout>

#include <QMenu>

#include "_common_functions.h"
#include "customdialog.h"
#include "imodplugin.h"
#include "dia_qtutils.h"
#include "analysistools.h"

#include <qfiledialog.h>

#include <sstream>      // for formatting string output
#include <fstream>      // for input/output of binary files

//############################################################

static AnalysisToolsData plug = { 0, 0 };

//############################################################



//----------------------------------------------------------------------------
//
//          MAPPED FUNCTIONS:
//
//----------------------------------------------------------------------------



//------------------------
//-- MAPPED FUNCTION: Called by the imod plugin load function

const char *imodPlugInfo(int *type)
{
  if (type)
    *type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS + IMOD_PLUG_MESSAGE + 
      IMOD_PLUG_MOUSE + IMOD_PLUG_EVENT;
    
  return("Analysis Tools");
}

//------------------------
//-- MAPPED FUNCTION: Grab hotkey input. return 1 if we handle the key.
 
int imodPlugKeys(ImodView *vw, QKeyEvent *event)
{
  int keyhandled = 1;
  
  if (!plug.view)          // if plugin window isn't open: don't grab keys
    return 0;
  
  int keysym  = event->key();            // key value (Key_A, Key_Space... etc)
  int ctrl    = event->modifiers() & Qt::ControlModifier;   // ctrl modifier
  int shift   = event->modifiers() & Qt::ShiftModifier;     // shift modifier
  
  
  switch(keysym)
  {
    case Qt::Key_Up:
      if(shift)
        return 0;
      else
        edit_changeSelectedSlice( 1, true );
      break;
      
    case Qt::Key_Down:
      if(shift)
        return 0;
      else
        edit_changeSelectedSlice( -1, true );
      break;
    
    case Qt::Key_T:
      if(ctrl)
        plug.window->test();
      else
        return 0;
      break;
      
    default:
      keyhandled = 0;
      break;
  }
  
  return keyhandled;
}

//------------------------
//-- MAPPED FUNCTION: Called when plugin window is started.
//-- Opens the plugin window for user interaction and initilizes data.
//-- See imodplug.h for a list of support functions.

void imodPlugExecute(ImodView *inImodView)
{

  if (plug.window) {      // if already open: bring window to front
    plug.window->raise();
    return;
  }
  
  ivwSetMovieModelMode( plug.view, IMOD_MMODEL );
  
  //## INITIALIZE DATA:
  
  if( !plug.initialized )
  {
    plug.window->initValues();
    
    plug.window->loadSettings();
    plug.initialized = true;
  }
  plug.view = inImodView;
  ivwTrackMouseForPlugs(plug.view, 1);
  ivwEnableStipple( plug.view, 1 );     // enables the display of stippled lines
  ivwGetImageSize(inImodView, &plug.xsize, &plug.ysize, &plug.zsize);
  
  //## INITIALIZE EXTRA OBJECT:
  
  plug.extraObjGG = ivwGetFreeExtraObjectNumber(plug.view);
  Iobj *xobjGG = ivwGetAnExtraObject(plug.view, plug.extraObjGG);
  imodObjectSetColor(xobjGG, 0.0, 0.3, 0.0);               // dark green
  imodObjectSetValue(xobjGG, IobjFlagClosed, 0);
  
  plug.extraObjG = ivwGetFreeExtraObjectNumber(plug.view);
  Iobj *xobjG = ivwGetAnExtraObject(plug.view, plug.extraObjG);
  imodObjectSetColor(xobjG, 1.0, 0.25, 0.25);              // pink
  imodObjectSetValue(xobjG, IobjFlagClosed, 0);
  
  plug.extraObjGB = ivwGetFreeExtraObjectNumber(plug.view);
  Iobj *xobjGB = ivwGetAnExtraObject(plug.view, plug.extraObjGB);
  imodObjectSetColor(xobjGB, 1.0, 0.1, 0.1);              // red
  imodObjectSetValue(xobjGB, IobjFlagClosed, 1);
  
  
  //## CREATE THE PLUGIN WINDOW:
  
  plug.window  = new AnalysisTools(imodDialogManager.parent(IMOD_DIALOG),"Analysis Tools");
  
  imodDialogManager.add((QWidget *)plug.window, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)plug.window, IMOD_DIALOG );
}


//------------------------
//-- MAPPED FUNCTION: Process wheel events

int imodPlugEvent(ImodView *vw, QEvent *event, float imx, float imy)
{
  if( plug.window == NULL )
    return (0);
  
  return 0;
}


//------------------------
//-- MAPPED FUNCTION: Process a mouse event: An example of a circular cursor  
//-- with radius specified in image coordinates

/*
     Mouse event callback function to be defined by plugins with the
     IMOD_PLUG_MOUSE bit set.  ^
     This function can be used to override 3dmod mouse actions in the Zap 
     window.  [imx] and [imy] will contain the image position, and [but1], 
     [but2], and [but3] will indicate the state of the 3 buttons as mapped by 
     user preferences.  The return value should be the sum of two values: 
     ^  1 if the plugin handled the mouse event, indicating that no other action
     should be taken with the event itself by the 3dmod program.
     ^  2 if the specific calling window should draw itself, without issuing a
     general program redraw.  If this is not sufficient, the plugin should call
     ivwRedraw instead of setting this bit.
     ^  A zero return value indicates that 3dmod should process the event as usual.
     ^This function is called only when a mouse button is down, unless mouse
     tracking is enabled with ivwTrackMouseForPlugs.
    
    BUTTON KEY: (using my setup)
        LEFT   = but2 ********
        MIDDLE = but3
        RIGHT  = but1
*/

int imodPlugMouse(ImodView *vw, QMouseEvent *event, float imx, float imy,
                  int but1, int but2, int but3)
{
                      // if plugin is not open or imod isn't in "model mode": do nothing
  if( !plug.window || !ivwGetMovieModelMode(plug.view) )
    return (0);
  
  return (0);
    
  /*
//## UPDATE MOUSE VALUES:
  
  plug.mousePrev = plug.mouse;
  
  int noZap = ivwGetTopZapMouse(plug.view, &plug.mouse); // returns 1 if no Zap window
  if(noZap == 1)
    return (2);
  
  plug.changeX = plug.mouse.x - plug.mousePrev.x;
  plug.changeY = plug.mouse.y - plug.mousePrev.y;
  
//## REGENERATE DEFORM CIRCLE:
  
  plug.window->drawExtraObject(false);
  if( plug.showMouseInModelView )
    ivwDraw( plug.view, IMOD_DRAW_ALL );
  
//## UPDATE BUTTON PRESSED VALUES:
  
  plug.but1Pressed  = (but1 == 1) && (plug.but1Down == 0);
  plug.but2Pressed  = (but2 == 1) && (plug.but2Down == 0);
  plug.but3Pressed  = (but3 == 1) && (plug.but3Down == 0);
  
  plug.but1Released = (but1 == 0) && (plug.but1Down == 1);
  plug.but2Released = (but2 == 0) && (plug.but2Down == 1);
  plug.but3Released = (but3 == 0) && (plug.but3Down == 1);
  
  plug.but1Down = but1;      // right mouse   (using my preferred settings)
  plug.but2Down = but2;      // left mouse    (using my preferred settings)
  plug.but3Down = but3;      // middle mosue  (using my preferred settings)
  
  plug.shiftDown = (event->state() & Qt::ShiftButton);
  
//## PERFORM ACTION:
  

  
      // NOTE if we get to here we have dealt with the action, so re redraw and return 1
  
  ivwDraw( plug.view, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC );    
  return (1);
   
   */
}


//############################################################

//----------------------------------------------------------------------------
//
//          AnalysisTools METHODS:
//
//----------------------------------------------------------------------------


//------------------------
//-- Convenience method, allowing you to create a new QAction. The new action
//-- triggers the method "member" (when clicked), is called "text", has the 
//-- status tip "tip" and is added to the specified "menu".
//-- Returns the newly created QAction

QAction *AnalysisTools::addAction( QMenu *menu, const char *member,
                                  QString text, QString tip )
{
  QAction *newAction = new QAction(text, this);
  newAction->setToolTip( tip );
  newAction->setStatusTip( tip );
  connect( newAction, SIGNAL(triggered()), this, member );
  menu->addAction( newAction );
  
  return newAction;
}


//## WINDOW CLASS CONSTRUCTOR:

static const char *buttonLabels[] = {(char*)"Done", (char*)"Help"};
static const char *buttonTips[] = {(char*)"Close Analysis Tools", (char*)"Open help window"};

AnalysisTools::AnalysisTools(QWidget *parent, const char *name) :
  DialogFrame(parent, 2, buttonLabels, buttonTips, true, "Analysis Tools", "", name)
{
  const int LAY_MARGIN   = 4;
  const int LAY_SPACING  = 4;
  const int GROUP_MARGIN    = 1;
  const int SPACER_HEIGHT   = 15;
  
  //FIX UP ??? - CALL "ANALYSIS TOOLS"

  
  
  //## CREATE "ACTIONS" CONTEXT MENU AND CONNECTIONS:
  
  actionMenu   = new QMenu(this);
  QMenu *analysisMenu = actionMenu->addMenu                ("Analysis Tools");
  addAction( analysisMenu, SLOT(estimateZScale()),            "Estimate Z Scale",
             "Estimates the zScale of your section based on an closed contour object \n"
             "which contains multiple roughly sphereical and/or randomly oriented \n"
             "compartments. The basic premise of this function is that a spherical \n"
             "compartment should span the same distance in Z that it does in X and Y. \n"
             "In reality however, many compartments are truncated, and thus more \n"
             "sophisticated alrorithms are used here to analyze the surfaces." );
  addAction( analysisMenu, SLOT(outputConcavePtsAnalysis()),  "Analyze Convex Points",
             "  Allows you to specify a range of objects and outputs an 'analysis \n"
             "of concave points' on each closed contour in these objects. This  \n"
             "analysis includes a count of the total number of concave points, \n"
             "the fraction of contour length which is convex, the area of the \n"
             "convex hull, the contour's 'compactness' and so on. This information \n"
             "can give insight into  the relative 'complexity' of contours in \n"
             "each object." );
  addAction( analysisMenu, SLOT(outputTubeSizeAnalysis()),    "Analyze Tubes",
             "Designed to be used in situations where open contours have been used  \n"
             "to describe tubes of variable thickness, such that the radius of each \n"
             "point reflects the tube's radius at that point. After specifying a \n"
             "range of objects (with open contours) it outputs a mean width, \n"
             "max width, estimated total volume and so on, by analyzing the size of \n"
             "points and distance between them. " );
  addAction( analysisMenu, SLOT(outputBranchingAnalysis()),   "Analyze Branching",
             "Designed to be used in situations where open contours have been used to \n"
             "describe tubes, and some of these tubes 'branch' off others. The output \n"
             "shows the length of each tube, and their organization hierarchy (i.e \n"
             "which contour branches off which other contour) based on the proximity \n"
             "of the contours first point to other contours." );
  addAction( analysisMenu, SLOT(outputVolumeWithinXAnalysis()),   "Volume-Distance Analysis",
             "This very crude function allows you to estimate how much volume is \n"
             "within a certain distance of an object, but not in another object. \n"
             "An example: 'How much volume is within 100 nm of my Golgi AND inside \n"
             "the cell membrane AND not inside the Nucleus'. Note that all analyzed \n"
             "objects must be segmented with closed contours on every slice. To \n"
             "estimate volume random points are generated within a boxed region and \n" 
             "tested wether or not they are inside and/or within a specified distance \n"
             "of a closed contour, thus the more points generated, the more accurate  \n"
             "results are likely to be." );
  addAction( analysisMenu, SLOT(findClosestDistanceSurfsTwoObjects()),   "Distance Between Surfaces In Two Objects Analysis",
             "Monica is a poo-poo head  :-) \n"
             "I sure hope I remember to fix this up. \n"
             " \n"
             " \n"
             " \n"
             " \n" 
             " \n"
             " \n"
             "." );
  
  QMenu *gridMenu = actionMenu->addMenu                    ("Deformation Grid");
  addAction( gridMenu, SLOT(generateDefGrid()),            "Generate",
             "Designed to generate a 'deformation grid' - a uniform grid of \n"
             "(non-uniform) displacement vectors - between the 'section boundaries' \n"
             "of joined tomograms. To work, this requires an object called \n"
             "'deformation_points' containing a series of two-point open contour \n"
             "which span across each section  boundary and map the displacement \n"
             "of some recognizable structure. Once a deformation grid is generated \n"
             "it can be used to 'Deform Objects'." );
  addAction( gridMenu, SLOT(deformObjectsUsingDefGrid()),     "Deform Objects",
             "Uses a deformation grid (see above) to deform all points over a \n"
             "range of objects specified by the user." );
  addAction( gridMenu, SLOT(analyzeDefPoints()),     "Analyze Deformation Points",
             "Analyzes the XY offset of points in the 'deformation grid' object. " );
  addAction( gridMenu, SLOT(gridDisplayOptions()),                "Display Options #",
             "Presents a number of options to change the appearance of the \n"
             "deformation grid." );
  /*addAction( gridMenu, SLOT(notImplemented()),                "Open #",
             "... \n"
             "..." );
  addAction( gridMenu, SLOT(notImplemented()),                "Save #",
             "... \n"
             "..." );
  */
  
  QMenu *sectionMenu = actionMenu->addMenu                 ("Section Tools");
  addAction( sectionMenu, SLOT(delimitSectionBoundaries()),   "Delimit Tomogram Boundaries",
             "Constructions contours around the boundaries of the material \n"
             "over a range of Z values specified by the user. \n"
             "Note that the 'background pixel' is considered any pixel \n"
             "outside the section material, and these typically have a value of 174." );
  addAction( sectionMenu, SLOT(fillInRangeOfPixelValues()),   "Delimit Grey Values",
             "Allows the user to specify a range of pixel values, and either: \n"
             "(a) generates scattered point on each of these values, or \n"
             "(b) generates a series of new closed contours either around \n"
             "these regions." );
  addAction( sectionMenu, SLOT(deleteSlice()),   "Delete Slice",
             "Deletes the a range of slices from the model by removing contours \n"
             "from those slices and shifting down all points above. \n"
             "If specified, it will shifting open contour or scattered points \n"
             "up or down so they won't be lost." );
  
  //## Extra Buttons
   
  widget1 = new QWidget(this);
    
  gridLayout2 = new QGridLayout(widget1);
  gridLayout2->setSpacing(LAY_SPACING);
  gridLayout2->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  
  moreActionsButton = new QPushButton("Actions", widget1);
  connect(moreActionsButton, SIGNAL(clicked()), this, SLOT(moreActions()));
  moreActionsButton->setToolTip( "Contains several actions I didn't want to sqeeze "
                                 "into this window");
  gridLayout2->addWidget(moreActionsButton, 0, 0);
  
  moreSettingsButton = new QPushButton("Settings", widget1);
  connect(moreSettingsButton, SIGNAL(clicked()), this, SLOT(moreSettings()));
  moreSettingsButton->setToolTip( "Contains several settings I didn't want to sqeeze "
                                  "into this window");
  gridLayout2->addWidget(moreSettingsButton, 0, 1); 
  
  mLayout->addWidget(widget1);
  
  
  mLayout->addStretch();
  this->adjustSize();
  
  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
}


//## SLOTS:


//------------------------
//-- Accesses the extra object and draw a red deform circle and/or other
//-- reference contour at the last recorded position of the mouse. What is
//-- drawn depends on what drawing mode is selected.

bool AnalysisTools::drawExtraObject( bool redraw )
{
  if ( !plug.window )
    return false;
  
  //## CLEAR EXTRA OBJECT:
  
  Iobj *xobjGG = ivwGetAnExtraObject(plug.view, plug.extraObjGG);
  ivwClearAnExtraObject(plug.view, plug.extraObjGG);
  
  Iobj *xobjG = ivwGetAnExtraObject(plug.view, plug.extraObjG);
  imodObjectSetValue(xobjG, IobjFlagExtraInModv, (plug.grid_showModelView)?1:0);
  imodObjectSetValue(xobjG, IobjLineWidth, plug.grid_displayLineWDot);
  imodObjectSetValue(xobjG, IobjLineWidth2, plug.grid_displayLineWDot);
  ivwClearAnExtraObject(plug.view, plug.extraObjG);
  
  Iobj *xobjGB = ivwGetAnExtraObject(plug.view, plug.extraObjGB);
  imodObjectSetValue(xobjGB, IobjFlagExtraInModv, (plug.grid_showModelView)?1:0);
  imodObjectSetValue(xobjGB, IobjLineWidth, plug.grid_displayLineW);
  imodObjectSetValue(xobjGB, IobjLineWidth2, plug.grid_displayLineW);
  imodObjectSetValue(xobjGB, IobjPointSize, plug.grid_displaySpheres);
  ivwClearAnExtraObject(plug.view, plug.extraObjGB);
  
  
  if ( !xobjG )
    return false;
  
  
  //## GET Z VALUE:
  
  int ix, iy,iz;
  ivwGetLocation(plug.view, &ix, &iy, &iz);
  plug.mouse.z = iz;
  
  float x = plug.mouse.x;
  float y = plug.mouse.y;
  float z = plug.mouse.z;
  
  
  //## DRAW DEFORMATION GRID:
  
  bool dsExists = z < (int)plug.dgrid.slices.size();
  
  if( plug.grid_showGrid && dsExists )
  {
    DeformGridSlice &gs = plug.dgrid.slices[z];
    
    for(int i=0; i<(int)gs.vect.size(); i++)  // for each vector:
    {
      Ivector &vect = gs.vect[i];        // current vector
      
      Icont *contV = imodContourNew();
      imodPointAppendXYZ( contV, vect.ptS.x, vect.ptS.y, z);
      imodPointAppendXYZ( contV, vect.ptE.x, vect.ptE.y, z);
      setInterpolated( contV, 1 );
      imodObjectAddContour(xobjG, contV);
    }
  }
  
  if( plug.grid_showGridBounds && dsExists )  // if wanted and exists: boundary around grid on current slice
  {
    DeformGridSlice &gs = plug.dgrid.slices[z];
    
    Icont *contB = imodContourNew();
    imodPointAppendXYZ( contB, gs.getVectBL().ptE.x, gs.getVectBL().ptE.y, z);
    imodPointAppendXYZ( contB, gs.getVectBR().ptE.x, gs.getVectBR().ptE.y, z);
    imodPointAppendXYZ( contB, gs.getVectTR().ptE.x, gs.getVectTR().ptE.y, z);
    imodPointAppendXYZ( contB, gs.getVectTL().ptE.x, gs.getVectTL().ptE.y, z);
    imodObjectAddContour(xobjGB, contB);
    
    for(int y=0; y<gs.yPts; y++)
    {
      for(int x=0; x<gs.xPts; x++)
      {
        Ipoint *ptCurr  = &gs.getVectSafe(x,y).ptE;
        Ipoint *ptDown  = &gs.getVectSafe(x,y+1).ptE;
        Ipoint *ptRight = &gs.getVectSafe(x+1,y).ptE;
        
        Icont *contVert = imodContourNew();
        imodPointAppendXYZ( contVert, ptCurr->x, ptCurr->y, z);
        imodPointAppendXYZ( contVert, ptDown->x, ptDown->y, z);
        imodObjectAddContour(xobjGB, contVert);
        
        Icont *contHorz = imodContourNew();
        imodPointAppendXYZ( contHorz, ptCurr->x,  ptCurr->y, z);
        imodPointAppendXYZ( contHorz, ptRight->x, ptRight->y, z);
        imodObjectAddContour(xobjGB, contHorz);
      }
    }
  }
  
  if( plug.grid_showGridGreen && dsExists )
  {
    DeformGridSlice &gs = plug.dgrid.slices[z];
    for(int y=0; y<gs.yPts; y++)
    {
      for(int x=0; x<gs.xPts; x++)
      {
        Ipoint *ptCurr  = &gs.getVectSafe(x,y).ptS;
        Ipoint *ptDown  = &gs.getVectSafe(x,y+1).ptS;
        Ipoint *ptRight = &gs.getVectSafe(x+1,y).ptS;
        
        Icont *contVert = imodContourNew();
        imodPointAppendXYZ( contVert, ptCurr->x, ptCurr->y, z);
        imodPointAppendXYZ( contVert, ptDown->x, ptDown->y, z);
        imodObjectAddContour(xobjGG, contVert);
        
        Icont *contHorz = imodContourNew();
        imodPointAppendXYZ( contHorz, ptCurr->x,  ptCurr->y, z);
        imodPointAppendXYZ( contHorz, ptRight->x, ptRight->y, z);
        imodObjectAddContour(xobjGG, contHorz);
      }
    }
  }
                     
  if( plug.grid_showModelView && dsExists )
  {

    for(int z=0; z<(int)plug.dgrid.slices.size(); z++)
    {
      DeformGridSlice &gs = plug.dgrid.slices[z];
      if( gs.sectionBoundary || z == 0 || z == plug.zsize-1 )
      {  
        Icont *contEdge = imodContourNew();
        gs.getContourAroundEdge( contEdge );
        imodObjectAddContour(xobjGB, contEdge);
      }
    }
    
    
    Icont *contLL = imodContourNew();
    Icont *contLR = imodContourNew();
    Icont *contUL = imodContourNew();
    Icont *contUR = imodContourNew();
    
    for(int z=0; z<(int)plug.dgrid.slices.size(); z++)
    {
      imodPointAppend( contLL, &plug.dgrid.slices[z].getVectBL().ptE );
      imodPointAppend( contLR, &plug.dgrid.slices[z].getVectBR().ptE );
      imodPointAppend( contUL, &plug.dgrid.slices[z].getVectTL().ptE );
      imodPointAppend( contUR, &plug.dgrid.slices[z].getVectTR().ptE );
    }
    
    imodObjectAddContour(xobjG, contLL);
    imodObjectAddContour(xobjG, contLR);
    imodObjectAddContour(xobjG, contUL);
    imodObjectAddContour(xobjG, contUR);
    
    
    /*
    for(int y=0; y<=plug.dgrid.rowsY; y++)
    {
      for(int x=0; x<=plug.dgrid.colsX; x++)
      {
        Icont *contB = imodContourNew();
        for(int z=0; z<(int)plug.dgrid.slices.size(); z++)
        {
          Ipoint *pt = &plug.dgrid.slices[z].getVectSafe(x,y).ptE;
          imodPointAppendXYZ( contB, pt->x,pt->y,z);
        }
        imodObjectAddContour(xobjG, contB);
      }
    }
     */
  }
  
  
  
  if( redraw )
    ivwDraw( plug.view, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC );
  
  return true;
}


//------------------------
//-- Clears all the contents of the extra object.

void AnalysisTools::clearExtraObj()
{
  Iobj *obj = ivwGetExtraObject(plug.view);
  int ncont = csize(obj);
  if (!ncont)
    return;
  
  Icont *cont = getCont(obj, 0);
  for (int co = ncont - 1; co >= 0; co--)   // remove contours from the end
    imodObjectRemoveContour(obj, co);
  imodContoursDelete(cont, ncont);          // free the contour data
}


//------------------------
//-- Used to initialize default values into DrawingToolsData.

void AnalysisTools::initValues()
{
  plug.arrowKeysSections     = true;
  plug.selectedAction        = 0;
  
  plug.grid_showGrid         = true;
  plug.grid_showGridBounds   = true;
  plug.grid_showGridGreen    = false;
  plug.grid_showModelView    = true;
  plug.grid_displayLineW     = 1;
  plug.grid_displaySpheres   = 0;
  plug.grid_displayLineWDot  = 1;
  
  plug.grid_colsX            = 20;
  plug.grid_rowsY            = 20;
  plug.grid_power            = 3.0;
}


//------------------------
//-- Loads most of the settings for AnalysisTools from user preferences

void AnalysisTools::loadSettings()
{
  
  double savedValues[NUM_SAVED_VALS];
  
  int nvals = prefGetGenericSettings("AnalysisTools", savedValues, NUM_SAVED_VALS);
  
  if(nvals!=NUM_SAVED_VALS)
  {
    wprint("AnalysisTools: Error loading saved values");
    return;
  }
  
  plug.arrowKeysSections      = savedValues[0];
  plug.grid_showGrid          = savedValues[1];
  plug.selectedAction         = savedValues[2];
}


//------------------------
//-- Saves most of the settings within AnalysisToolsData in user preferences
//-- so they will load next time Bead Helper is started

void AnalysisTools::saveSettings()
{
  double saveValues[NUM_SAVED_VALS];
  
  saveValues[0]   = plug.arrowKeysSections;
  saveValues[1]   = plug.grid_showGrid;
  saveValues[2]   = plug.selectedAction;
  
  prefSaveGenericSettings("AnalysisTools",NUM_SAVED_VALS,saveValues);
}



//------------------------
//-- Reduces ALL contours in the current object.

void AnalysisTools::test()
{
  /*
  Imod *imod = ivwGetModel(plug.view);
  float modelZScale    = imodGetZScale(imod);
  Iobj *obj = getCurrObj();
  Icont *cont1 = getCont( obj, 0 );
  Icont *cont2 = getCont( obj, 1 );
  Ipoint p1, p2;
  Ipoint ll1, ur1;
  Ipoint ll2, ur2;
  
  cont_getMBR(cont1, &ll1, &ur1);
  cont_getMBR(cont2, &ll2, &ur2);
  
  cout << "MIN DIST MBRS: " << mbr_distBetweenBBoxes3D( &ll1, &ur1,
                                                                &ll2, &ur2, modelZScale
                                                                   ) << endl;
  cout << "MIN DIST BETWEEN CONTS: " << cont_minDistBetweenContPts3D( cont1, cont2, modelZScale,
                                                                  &p1, &p2 ) << endl;
  flush(cout);
  printPt( &p1);
  printPt( &p2);
  */
  
  findClosestDistanceSurfsTwoObjects();
  
  /*
  int numRandPts = 100*1000;
  
  cout << analysis_outputVolumeWithinXAnalysis( 0, 3,4,-1, 900, numRandPts, true, true );  // golgi
  cout << analysis_outputVolumeWithinXAnalysis( 0, 3,4,-1, 300, numRandPts, true, false );
  cout << analysis_outputVolumeWithinXAnalysis( 0, 3,4,-1, 100, numRandPts, true, false );
  
  cout << analysis_outputVolumeWithinXAnalysis( 4, 3,4,-1, 900, numRandPts, true, true ); // nucleus
  cout << analysis_outputVolumeWithinXAnalysis( 4, 3,4,-1, 300, numRandPts, true, false );
  cout << analysis_outputVolumeWithinXAnalysis( 4, 3,4,-1, 100, numRandPts, true, false );
  
  cout << analysis_outputVolumeWithinXAnalysis( 3, 3,4,-1, 900, numRandPts, true, true ); // membrane
  cout << analysis_outputVolumeWithinXAnalysis( 3, 3,4,-1, 300, numRandPts, true, false );
  cout << analysis_outputVolumeWithinXAnalysis( 3, 3,4,-1, 100, numRandPts, true, false );
  cout << analysis_outputVolumeWithinXAnalysis( 3, 3,4,-1, 50, numRandPts, true, false );
  cout << analysis_outputVolumeWithinXAnalysis( 3, 3,4,-1, 25, numRandPts, true, false );
  */
  
  /*deleteSlice();*/
  /*int x, y, z;
  ivwGetLocation( plug.view, &x, &y, &z );
  
  
  //## TEST "img_createContourAroundEdge" FUNCTION:
  
  Icont *cont = getCurrCont();
  if( !cont || !plug.data )
  {
    MsgBox("Select contour and initialize plug.data first");
    return;
  }
  const unsigned char PIX_ON  = 1;       // used for pixels outside desired grey range
  img_createContourAroundEdge( cont, PIX_ON, x,y,z, plug.data, plug.xsize, plug.ysize, 1000 );
  */
  
  //## TEST RETRIEVAL OF GREY VALUES:
  /*
  float greyVal = ivwGetFileValue( plug.view, x,y,z );
  cout << "greyVal = " << greyVal << endl;
  
  unsigned char **slice = ivwGetCurrentZSection( plug.view );
  int idx = y*plug.xsize + x;
  unsigned char *d = slice[y];
  unsigned char greyValC = d[x];
  int val = (int)greyValC;
  
  cout << "greyValC = " << val << endl;
   */
}

//------------------------
//-- Reduces ALL contours in the current object.

void AnalysisTools::notImplemented()
{
  MsgBox("This function has not been implemented yet");
}



//------------------------
//-- Outputs the string "bigOutputString" to either:
//-- (a) the main imod window     - if outputOption is 0
//-- (b) the console              - if outputOption is 1
//-- (c) saved to a cvs file      - if outputOption is 2

void AnalysisTools::outputString( string bigOutputString, int outputOption )
{
  if( bigOutputString.length() <= 1 )
    return;
  
  switch(outputOption)
  {
    case(0):
    {
      wprint( bigOutputString.c_str() );
      break;
    }
    case(1):
    {
      cout << bigOutputString;
      break;
    }
    case(2):
    {
      QString qname = QFileDialog::getSaveFileName(plug.window,"Save File",
                                                   "","CSV file (*.csv)");
      if ( qname==NULL )
        return;
      
      string filePath = qStringToString(qname);
      
      ofstream out_file( filePath.c_str() );                //open/create text file
      if (out_file.fail())
      {
        MsgBox("ERROR: Could not create and/or open file");
        return;
      }
      out_file << bigOutputString;
      out_file.close();                    //close text file
      break;
    }
  }
}




//------------------------
//-- Estimates the zScale of your section based on an closed contour object
//-- which contains multiple roughly spherical and/or randomly oriented
//-- compartments. The basic premise of this function is that a spherical
//-- compartment should span the same distance in Z that it does in X and Y.
//-- In reality however, many compartments are truncated, and thus more
//-- sophisticated algorithms are used here to analyze the surfaces.
//-- Details of this algorithm have been submitted for publication at PLoS 
//-- computational biology as "Improved Isotropic Reconstruction for 3D 
//-- Surface Models of Cellular Compartments by Mathematical Analysis of 
//-- Spherical Organelles by Electron Tomography".

void AnalysisTools::estimateZScale()
{
  if( !isCurrObjValidAndShown() )
  {
    MsgBox("You must select a valid object containing multiple \n"
           "'approximately spherical' surfaces");
    return;
  }
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = getCurrObj();
  float modelZScale    = imodGetZScale(imod);
  int objIdx, cIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &cIdx, &ptIdx);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  float        currZScale          = modelZScale;
  static bool  useHemispheres      = true;
  static bool  calcSpheres         = false;//true;    // sphere options
  static float maxDeviation        = 0.25;
  static float startChangeZ        = 0.05;
  static float accuracyZScale      = 0.00001;
  static int   maxIterationsCenter = 1000;
  static float maxZChange          = 1.0;
  static bool  showSurfClassif     = true;//false;
  static bool  appendToCSV         = false;//true;
  static bool  summaryCSV          = false;//true;
  static bool  printOnlyAccepted   = false;
  static bool  showCenters         = true;
  
  CustomDialog ds( "Estimate Z Scale", this );
  ds.addLabel      ( "NOTE: Before running this you should first run \n"
                     "'Drawing Tools > Reduce All' (to ensure evenly spaced \n"
                     "points) then run 'imodmesh -p 10' and 'imodsortsurf' \n"
                     "to sort contours into surfaces for your object \n"
                     "with 'approximately spherical' compartments.\n" );
  ds.addDblSpinBoxF( "best (starting) z scale estimate:    ", 0.1, 6.0, &currZScale, 2, 0.01 );
  ds.addCheckBox   ( "use hemispheres (touching section boundary)", &useHemispheres, "will include spherical profiles truncated at the section boundary, if it appears >1/2 has been segmented" );
  ds.addLabel      ( "--- SPHERE OPTIONS: ---" );
  ds.addCheckBox   ( "calculate spheres of best fit", &calcSpheres, "WARNING: this can be time consuming" );
  ds.addDblSpinBoxF( "maximum deviation from center (D):    ", 0.0, 0.9, &maxDeviation, 3, 0.005, "if the distance from the furthest point minus the distance to the closest point divide the distance from the furthest point is > than this, it won't be counted" );
  ds.addDblSpinBoxF( "start change in Z:                    ", 0.001, 0.5, &startChangeZ, 3, 0.001, "how far the first guesses will fall from the current Z" );
  ds.addDblSpinBoxF( "accuracy of Z scale to caculate:      ", 0.000001, 0.2, &accuracyZScale, 6, 0.000001, "how accurately the best z scale for each surface will be guessed/calculated" );
  ds.addSpinBox    ( "maximum iterations to find center:    ", 10, 10000, &maxIterationsCenter, 10, "the max number of times the center point will be refined trying to find the best center" );
  ds.addDblSpinBoxF( "maximum change in Z allowed:          ", 0.1, 2.0, &maxZChange, 3, 0.005, "any surface outside of this will be assumed an error and discounted" );
  ds.addLabel      ( "--- OUTPUT OPTIONS: ---" );
  ds.addCheckBox   ( "show surface classifications", &showSurfClassif, "will change the surfNum of rejected surfaces to -2, so they can be identified in the MODELVIEW window" );
  ds.addCheckBox   ( "append info to csv file", &appendToCSV, "prints information about surfaces to the file '_z_scale_output.csv' " );
  ds.addCheckBox   ( "append summary to csv file", &summaryCSV, "prints a summary to the file '_z_scale_summary.csv' " );
  ds.addCheckBox   ( "print accepted surfaces only", &printOnlyAccepted, "does not outputs information for rejected surfaces" );
  ds.addCheckBox   ( "show center of surfaces", &showCenters, "shows center of each surface" );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  
  //## PERFORM CALCULATION:
  float zScaleEstimate = 
    analysis_estimateZScale( objIdx, currZScale, useHemispheres, calcSpheres,
                             maxDeviation, startChangeZ, accuracyZScale,
                             maxIterationsCenter, maxZChange, 
                             showSurfClassif, appendToCSV, summaryCSV,
                             printOnlyAccepted, showCenters );
  
  string message = "Estimated z scale = " + toString( zScaleEstimate, 3 ) + "\n"
    + "Old z scale = " + toString( modelZScale, 3 ) + "\n" + "\n"
    + "... Do you want to change the Z scale?";
  
  if( MsgBoxYesNo( this, message ) )
    ;
    //imodSetZScale( imod, zScaleEstimate );
  
}



//------------------------
//-- Allows you to specify a range of objects and outputs an 'analysis 
//-- of concave points' on each closed contour in these objects. This 
//-- analysis includes a count of the total number of concave points,
//-- the fraction of contour length which is convex, the area of the
//-- convex hull, the contour's 'compactness' and so on. This information
//-- can give insight into  the relative 'complexity' of contours in
//-- each object.

void AnalysisTools::outputConcavePtsAnalysis()
{
  Imod *imod = ivwGetModel(plug.view);
 
  //## GET USER INPUT FROM CUSTOM DIALOG:
  static bool printAllConts     = true;
  int minObj                    = 1;
  int maxObj                    = osize(imod);
  static int  outputOption      = 0;
  
  CustomDialog ds("Analysis of Concave Points", this );
  ds.addSpinBox ( "min object:",1,osize(imod),&minObj,1 );
  ds.addSpinBox ( "max object:",1,osize(imod),&maxObj,1 );
  ds.addCheckBox( "print all contours", &printAllConts );
  ds.addRadioGrp( "output to:", "imod window|console|csv file", &outputOption );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  minObj--;
  maxObj--;
  
  
  //## PERFORM ANALYSIS AND OUTPUT RESULTS:
  
  string bigOutputString =
    analysis_outputConcavePtsAnalysis(minObj,maxObj,printAllConts);
  
  outputString( bigOutputString, outputOption );
}


//------------------------
//-- Designed to be used in situations where open contours have been used 
//-- to describe tubes of variable thickness, such that the radius of each
//-- point reflects the tube's radius at that point. After specifying a  
//-- range of objects (with open contours) it outputs a mean width,  
//-- max width, estimated total volume and so on, by analyzing the size of
//-- points and distance between them. 

void AnalysisTools::outputTubeSizeAnalysis()
{
  Imod *imod = ivwGetModel(plug.view);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  int minObj                    = 9;
  int maxObj                    = 11;
  static bool printAllConts     = true;
  static bool convertUnits      = true;
  static int  outputOption      = 0;
  
  CustomDialog ds( "Tube Analysis", this );
  ds.addSpinBox ( "min object:",1,osize(imod),&minObj,1 );
  ds.addSpinBox ( "max object:",1,osize(imod),&maxObj,1 );
   ds.addCheckBox( "print all open contours", &printAllConts );
  ds.addCheckBox( "convert to appripriate units", &convertUnits );
  ds.addRadioGrp( "output to:", "imod window|console|csv file", &outputOption );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  minObj--;
  maxObj--;
  
  
  //## PERFORM ANALYSIS AND OUTPUT RESULTS:
  
  string bigOutputString =
    analysis_outputTubeSizeAnalysis(minObj,maxObj,printAllConts,convertUnits);
  
  outputString( bigOutputString, outputOption );
}

//------------------------
//-- Designed to be used in situations where open contours have been used to
//-- describe tubes, and some of these tubes 'branch' off others. The output
//-- shows the length of each tube, and their organization hierarchy (i.e
//-- which contour branches off which other contour) based on the proximity
//-- of the contours first point to other contours. 

void AnalysisTools::outputBranchingAnalysis()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  int objMainLen         = 10;
  int objBranches        = 11;
  static int  branchDepth       = 2;
  static int  maxDistBranch     = 10;
  static bool addRadiusToLen    = true;
  static bool convertUnits      = true;
  static bool showMatches       = true;
  static int  outputOption      = 0;
  
  CustomDialog ds("Branch Analysis", this);
  ds.addSpinBox ( "object to analyze:",1,osize(imod),&objMainLen,1,
                 "The object containing 'root contours' - which we want to \n"
                 "find branches off" );
  ds.addSpinBox ( "object containing branches:",1,osize(imod),&objBranches,1,
                 "The contours which we will test to see if they branch off \n"
                 "'root contours' or other branches \n"
                 "NOTE: This object can be the same object as above" );
  ds.addLabel   ( "--- BRANCHING OPTIONS ---" );
  ds.addSpinBox ( "maximum branch depth:",1,100,&maxDistBranch,1,
                 "The maximum level or 'depth' of branching to search for \n"
                 "whereby a branch coming off a root contour has depth 1, \n"
                 "a branch off a branch off a root has depth 3, etc." );
  ds.addSpinBox ( "max dist to search for branches:",1,100,&maxDistBranch,1,
                 "A contour is considered a branch if it's FIRST point is \n"
                 "within this many pixels of any point within the contour \n"
                 "we are testing (ie. a root contour or existing branch)." );
  ds.addLabel   ( "--- OUTPUT OPTIONS ---" );
  ds.addCheckBox( "add radius at ends to length", &addRadiusToLen,
                 "The length of root contours will be their open length \n"
                 "PLUS the radius of their first and last point \n"
                 "and the length of all branches will be their open length \n"
                 "PLUS the radius of their last point ONLY." );
  ds.addCheckBox( "convert to appripriate units", &convertUnits,
                 "Will show results in nm (or whatever you've entered \n"
                 "in the model header) instead of pixels" );
  ds.addCheckBox( "show matches", &showMatches,
                 "Prints a list of all contours in 'object containing branches' \n"
                 "showing how many times each was detected as a branch" );
  ds.addRadioGrp( "output to:", "imod window|console|csv file", &outputOption );
  ds.exec();
  if( ds.wasCancelled() )
  return;
  
  objMainLen--;
  objBranches--;
  
  //## PERFORM ANALYSIS AND OUTPUT RESULTS:
  
  string bigOutputString =
    analysis_outputBranchingAnalysis(objMainLen, objBranches, branchDepth, maxDistBranch,
                                     addRadiusToLen, convertUnits, showMatches);
  
  outputString( bigOutputString, outputOption );
}

//------------------------
//-- This very crude function allows you to estimate how much volume is
//-- within a certain distance of an object, but not in another object.
//-- An example: 'How much volume is within 100 nm of my Golgi AND inside
//-- the cell membrane AND not inside the Nucleus'. Note that all analyzed
//-- objects must be segmented with closed contours on every slice. To
//-- estimate volume random points are generated within a boxed region and 
//-- tested wether or not they are inside and/or within a specified distance
//-- of a closed contour, thus the more points generated, the more accurate 
//-- results are likely to be.

void AnalysisTools::outputVolumeWithinXAnalysis()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  static int objToAnalyze  = 1;
  static int objInIdx      = 4;
  static int objOut1Idx    = 5;
  static int objOut2Idx    = 0;
  
  static int distThres     = 100;
  static int numRandomPts  = 100000;
  static bool useMbr       = true;
  static bool addPtsNewObj = true;
  static int  outputOption = 1;
    
  CustomDialog ds( "Volume within X distance analysis", this);
  ds.addLabel   ( "Objects to use:" );
  ds.addSpinBox ( "> object for analysis:",1,osize(imod),&objToAnalyze,1,
                  "The object containing contours we want to compare against" );
  
  ds.addSpinBox ( "> must be in object:",-1,osize(imod),&objInIdx,1,
                  "Points will be dismissed if NOT inside a (closed) contour from this object \n"
                  "NOTE: If 0 then not used" );
  ds.addSpinBox ( "> must not be in object:",-1,osize(imod),&objOut1Idx,1,
                  "Points will be dismissed if inside a (closed) contour from this object \n" 
                  "NOTE: If 0 then not used" );
  ds.addSpinBox ( "> must not be in object:",-1,osize(imod),&objOut2Idx,1,
                  "Points will be dismissed if inside a (closed) contour from this object \n" 
                  "NOTE: If 0 then not used" );
  ds.addLabel   ( "----------" );
  ds.addSpinBox ( "distance from object to use (nm):",1,99999999,&distThres,10,
                  "The distance away from the analysis object to use as threshold" );
  ds.addSpinBox ( "# random points:",1,10000000,&numRandomPts,1000,
                  "Number of points to randomly generate and test for distance "
                  "to analysis object (used to estimate volume)" );
  ds.addCheckBox( "use MBR around analysis object", &useMbr,
                  "Generates and minimum boundring rectangle around the analysis object "
                  "expanded to include a gutter equal to the distance threshold" );
  ds.addCheckBox( "add points to new object", &addPtsNewObj,
                  "Allows you to see the result of random point distance analysis" );
  ds.addRadioGrp( "output to:", "imod window|console|csv file", &outputOption );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  //## PERFORM ANALYSIS AND OUTPUT RESULTS:
  
  string bigOutputString =
    analysis_outputVolumeWithinXAnalysis( objToAnalyze-1,
                                          objInIdx-1, objOut1Idx-1, objOut2Idx-1,
                                          (float) distThres, numRandomPts,
                                          useMbr, addPtsNewObj );
  
  outputString( bigOutputString, outputOption );
}


//------------------------
//-- 

void AnalysisTools::findClosestDistanceSurfsTwoObjects()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  static int objAIdx = 1;
  static int objBIdx = 2;
  
  static bool addPtsNewObj = true;
  static bool applyDistLimit = false;
  static float  maxDistLimitInNm = 1000;
  static int  outputOption = 1;
  
  CustomDialog ds( "Closest Distance Between Surfaces", this);
  ds.addLabel   ( "Objects to use:" );
  ds.addSpinBox ( "> object A (from):",1,osize(imod),&objAIdx,1,
                  "For EACH surface in this object, you want to find the closest \n"
                  "surface in object B." );
  ds.addSpinBox ( "> object B (to):",-1,osize(imod),&objBIdx,1,
                  "For each surface in object A, you want to find the closest surface \n"
                  "in this object. When closest distances are fun, every surface \n"
                  "in object A should have one line extending from it, but surfaces \n"
                  "in object B can have zero or more lines." );
  ds.addCheckBox( "limit distances (in nm) to ignore anything over:", &applyDistLimit,
                  "Generates and minimum boundring rectangle around the analysis object "
                  "expanded to include a gutter equal to the distance threshold" );
  ds.addDblSpinBoxF( "..... ",1.0,100000000.0,&maxDistLimitInNm,2,100.0,
                     "The max limit distance. If any surface in object A is further \n"
                     "than this distance (in the units you've specified under \n"
                     "'Model > Header') to the nearest surface in object B it won't "
                     "be shown" );
  ds.addRadioGrp( "output to:", "imod window|console|csv file", &outputOption );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  //## PERFORM ANALYSIS AND OUTPUT RESULTS:
  
  float maxDistLimitInPixels = fDiv( maxDistLimitInNm, imodGetPixelSize(imod) );
  if(!applyDistLimit)
    maxDistLimitInPixels = FLOAT_MAX;
  
  string bigOutputString =
    analysis_closestDistanceSurfsTwoObjects( objAIdx-1, objBIdx-1, addPtsNewObj,
                                             maxDistLimitInPixels );
  
  outputString( bigOutputString, outputOption );
}


//------------------------
//-- Designed to generate a 'deformation grid' - a uniform grid of 
//-- (non-uniform) displacement vectors - between the 'section boundaries' 
//-- of joined tomograms. To work, this requires an object called
//-- 'deformation_points' containing a series of two-point open contour
//-- which span across each section  boundary and map the displacement
//-- of some recognizable structure. Once a deformation grid is generated
//-- it can be used to 'Deform Objects'.

void AnalysisTools::generateDefGrid()
{
  Imod *imod  = ivwGetModel(plug.view);
  int defObjIdx = -1;
  
  //## TRY TO FIND "DEFORMATION POINTS" OBJECT
  for(int o=0; o<osize(imod); o++)
  {
    QString nameStr = (QString)imodObjectGetName( getObj(imod,o) );
    if( nameStr == "deformation_points" )
      defObjIdx = o;
  }
  
  //## IF "DEFORMATION POINTS" OBJECT NOT FOUND: OFFER TO CREATE ONE
  if(defObjIdx == -1)
  {
    if( !MsgBoxYesNo( plug.window,
                      "To create a deformation grid, you must create an open object "
                      "called 'deformation_points' and draw several lines "
                      "(i.e. contours with 2 points) between corresponding features "
                      "either side of each section boundary \n\n"
                      "Create this object?" ) )
        return;
    
    if( imodNewObject(imod) ) return;
    defObjIdx = osize(imod)-1;
    Iobj *newObj = getObj( imod, defObjIdx );
    imodObjectSetName (newObj,"deformation_points");
    imodObjectSetColor(newObj, 1.0, 0.5, 0.0);     // orange
    imodObjectSetValue(newObj, IobjFlagClosed, 0);
    imodObjectSetValue(newObj, IobjPointSize, 10);
    return;
  }
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool applyCumulativeTransforms = true;
  
  CustomDialog ds( "Generate Deformation Grid", this);
  ds.addSpinBox  ( "columns (x):  ", 1, 1000, &plug.grid_colsX, 1 );
  ds.addSpinBox  ( "rows    (y):  ", 1, 1000, &plug.grid_rowsY, 1 );
  ds.addCheckBox( "show grid",  &plug.grid_showGrid );
  ds.addCheckBox( "applyCumulativeTransforms",  &applyCumulativeTransforms );
  ds.addDblSpinBoxF( "power used in deform:  ", 0.05, 25.0, &plug.grid_power, 1, 0.1,
                    "Used in formula 1/(distance^power) used to weight vectors" );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  //## CREATE DEFORMATION GRID:
  def_generateDefGrid( defObjIdx, plug.grid_colsX, plug.grid_rowsY, plug.grid_power, applyCumulativeTransforms );
}




//------------------------
//-- Uses a deformation grid (see above) to deform all points over a 
//-- range of objects specified by the user.

void AnalysisTools::deformObjectsUsingDefGrid()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  int nObjects = osize(imod);
  
  if( plug.dgrid.isEmpty() )
  {
    MsgBox( "You must generate or load a deformation grid before "
            "you can deform objects" );
    return;
  }
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int         objMin         = 1;
  int         objMax         = nObjects;
  static bool useInverse     = false;
  static bool skipDefPtsObj  = false;
  
  CustomDialog ds( "Deform Objects Using Deformation Grid", this);
  ds.addLabel   ( "object range:" );
  ds.addSpinBox ( "min object:", 1, nObjects, &objMin, 1 );
  ds.addSpinBox ( "max object:", 1, nObjects, &objMax, 1 );
  ds.addLabel   ( "-----\n"
                  "point-wise options:" );
  ds.addCheckBox( "skip 'deformation_points' object",  &skipDefPtsObj );
  ds.addCheckBox( "use inverse of grid (not recommended)",  &useInverse );  
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  objMin--;
  objMax--;
  
  if( !MsgBoxYesNo( plug.window,
                    "WARNING: You cannot undo this operation, so it is "
                    "IMPERATIVE you save a copy of your model BEFORE "
                    "you continue! \n\n"
                    "Are you sure you want to continue?" ) )
    return;
  
  
  //## DEFORM OBJECTS:
  
  wprint("DEFORMING OBJECTS:");
  
  for(int o=objMin; o<objMax && o<osize(imod); o++)
  {
    Iobj *obj = getObj(imod,o);
    QString nameStr = (QString)imodObjectGetName( obj );
    if( skipDefPtsObj && nameStr == "deformation_points" )
    {
      wprint("Skipped 'deformation_points' object %d", o+1);
      continue;
    }
    def_deformObjectsUsingDefGrid( o, useInverse, false );
  }
}




//------------------------
//-- Analyzes the XY offset of points in the 'deformation grid' object.

void AnalysisTools::analyzeDefPoints()
{
  Imod *imod  = ivwGetModel(plug.view);
  int objIdx, cIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &cIdx, &ptIdx);
  Iobj *obj = getObj(imod,objIdx);
  if( !obj || csize(obj)<=0 )
    return;
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  static bool convertUnits      = true;
  static int  outputOption      = 0;
  
  CustomDialog ds("Tube Analysis", this);
  ds.addCheckBox( "convert to appripriate units", &convertUnits );
  ds.addRadioGrp( "output to:", "imod window|console|csv file", &outputOption );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  
  
  //## PERFORM ANALYSIS AND OUTPUT RESULTS:
  
  string bigOutputString = def_outputDefPointsAnalysis( objIdx,convertUnits );
  
  outputString( bigOutputString, outputOption );
}


//------------------------
//-- Presents a number of options to change the appearance of the
//-- deformation grid.

void AnalysisTools::gridDisplayOptions()
{
  moreSettings();
}



/*
inline unsigned char &pix(unsigned char *data, int x, int y)
{
  return data[y*plug.xsize + x];
}

inline int isPixEqual(unsigned char val, unsigned char *data, int x, int y,
                      int maxX, int maxY, bool outOfBoundsReturnsTrue=false)
{
  if( x < 0 || x > maxX || y < 0 || y > maxY )
    return ( outOfBoundsReturnsTrue );
  return (data[y*maxX + x] == val) ? 1 : 0;
}
*/
/*
inline int pixIntVal( unsigned char **slice, int x, int y )
{
  return (int)slice[y][x];
}*/



//------------------------
//-- Constructions contours around the boundaries of the material
//-- over a range of Z values specified by the user.
//-- Note that the "background pixel" is considered any pixel
//-- outside the section material, and these typically have a value of 174.

void AnalysisTools::delimitSectionBoundaries()
{
  Imod *imod  = ivwGetModel(plug.view);
  int secObjIdx = -1;
  
  //## TRY TO FIND "TOMOGRAM BOUNDARIES" OBJECT
  for(int o=0; o<osize(imod); o++)
  {
    QString nameStr = (QString)imodObjectGetName( getObj(imod,o) );
    if( nameStr == "tomogram_boundaries" )
      secObjIdx = o;
  }
  
  //## IF "TOMOGRAM BOUNDARIES" OBJECT NOT FOUND: OFFER TO CREATE ONE
  if(secObjIdx == -1)
  {
    if( !MsgBoxYesNo( plug.window,
                      "This function will create a new object called "
                      "'tomogram_boundaries'... do you wish to continue?" ) )
      return;
    
    if( imodNewObject(imod) ) return;
    secObjIdx = osize(imod)-1;
    Iobj *newObj = getObj( imod, secObjIdx );
    imodObjectSetName (newObj,"tomogram_boundaries");
    imodObjectSetColor(newObj, 0.0, 0.0, 1.0);     // blue
  }
  
  const int BG_GREYVAL = 174;
  Iobj *obj  = getObj(imod,secObjIdx);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int          minZ = 1;
  int          maxZ = plug.zsize;
  static bool  useMBR     = true;
  static bool  makeConvex = false;
  static bool  reducePts  = false;
  static float minOverlap = 0.999;
  static bool  printProgress = true;
  
  CustomDialog ds("Generate Deformation Grid", this);
  ds.addSpinBox  ( "min z:  ", 1, plug.zsize, &minZ, 1 );
  ds.addSpinBox  ( "max z:  ", 1, plug.zsize, &maxZ, 1 );
  ds.addLineEditF( "min overlap to \nfor section boundaries:  ",
                   0.000, 1.000, &minOverlap, 8,
                   "If two adjacent section boundary contours overlap by more \n"
                   "than this amount they will be classified as being part of \n"
                   "the same section - otherwise they form a section boundary!" );
  ds.addLabel    ( "-----" );
  ds.addLabel    ( "Methods to reduce points:" );
  ds.addCheckBox ( "use minimum bounding rectangle",   &useMBR,
                   "Assumes the material boundary is a rectangle" );
  ds.addCheckBox ( "make convex",   &makeConvex, "Ensures contours are convex");
  ds.addCheckBox ( "reduce points", &reducePts,
                   "Uses a polygon generalization tolerance method to reduce "
                   "the number of points" );
  ds.addCheckBox ( "print progress", &printProgress,
                   "Will output a line for every slice.... but also slows down "
                   "progress a little" );
  ds.addLabel    ( "-----" );
  ds.addLabel    ( "This function generates contours around \n"
                   "material on every selected slice using \n "
                   "the 'tomogram_boundaries' object, and \n "
                   "uses these to determine section boundaries" );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  minZ--;
  
  //## PROCESS SLICES:
  
  wprint("PROCESSING SLICES: \n");
  
  float totAreaMat = 0.0f;
  float totArea    = 0.0f;
  int   numPixels  = plug.xsize*plug.ysize;
  
  for(int z=minZ; z<maxZ && z<plug.zsize; z++)
  {
    unsigned char **slice = ivwGetZSection( plug.view, z );
    
    if(!slice)
    {
      wprint( " section %d - ERROR PROCESSING" );
      continue;
    }
      
    Icont *contB = imodContourNew();      // bounding contour around the material
    
    //## ADD POINTS ALONG LEFT SIDE FROM BOTTOM - PUSHING EACH RIGHT UNTIL IT
    //## HITS A FG PIXEL
    
    for(int y=0; y<plug.ysize; y++)
    {
      for(int x=0; x<plug.xsize; x++)
      {
        if( (int)slice[y][x] != BG_GREYVAL )
        {
          imodPointAppendXYZ( contB, x, y, z);
          imodPointAppendXYZ( contB, x, y+1, z);
          break;
        }
      }
    }
      
    //## ADD POINTS ALONG RIGHT SIDE FROM TOP - PUSHING EACH RIGHT UNTIL IT
    //## HITS A FG PIXEL
    
    for(int y=plug.ysize-1; y>=0; y--)
    {
      for(int x=plug.xsize-1; x>0; x--)
      {
        if( (int)slice[y][x] != BG_GREYVAL )
        {
          imodPointAppendXYZ( contB, x+1, y+1, z);
          imodPointAppendXYZ( contB, x+1, y, z);
          break;
        }
      }
    }
      
    //## ADD CONTOUR TO OBJECT AND PRINT RESULTS:
    
    if( psize(contB)<=2 )
    {
      wprint( " section %d - COULD NOT FIND MATERIAL!" );
      continue;
    }
    
    if( useMBR )
    {
      Ipoint ll, ur;
      imodContourGetBBox(contB, &ll, &ur);
      for(int p=psize(contB)-1; p>=0; p--)
      {
        Ipoint *pt = getPt(contB,p);
        bool ptTouchesBB = (pt->x <= ll.x) || (pt->x >= ur.x)
                        || (pt->y <= ll.y) || (pt->y >= ur.y);
        if(!ptTouchesBB)
          imodPointDelete(contB,p);
      }
      imodContourStrip    ( contB );
    }
    if( makeConvex )
    {
      imodContourStrip    ( contB );
      cont_makeConvexCrude( contB );
      imodContourStrip    ( contB );
    }
    if( reducePts )
    {
      cont_reducePtsTol( contB, 1.0f );
    }
    
    float areaMat = imodContourArea( contB );
    totAreaMat += areaMat;
    totArea    += numPixels;
    
    int percentMat = calcPercentInt( areaMat, numPixels );
    wprint( " > section %d - %d%% material \n", z+1, percentMat );
    if( printProgress )
      QApplication::flush();
    
    edit_addContourToObj( obj, contB, true);
    
    imodContourDelete(contB);
  }
  
  int percentMatTot = calcPercentInt( totAreaMat, totArea );
  wprint("\n%d%% of the tomogram is occupied by material over "
         "these %d slices\n", percentMatTot, maxZ - minZ);
  
  //## GO THROUGH MATERIAL BOUNDARY CONTOURS AND IDENTIFY CONSEQUTIVE
  //## SLICES ON EITHER SIDE OF A SECTION BOUNDARY:
  
  int numBoundaries = 0;
  for(int c=1; c<csize(obj)-1; c++)
  {
    Icont *cont     = getCont(obj, c);
    Icont *contPrev = getCont(obj, c-1);
    Icont *contNext = getCont(obj, c+1);
    
    bool prevContSameSection = img_fractionOverlap( cont, contPrev ) > minOverlap;
    bool nextContSameSection = img_fractionOverlap( cont, contNext ) > minOverlap;
    
    if( prevContSameSection && nextContSameSection )
    {
      setInterpolated(cont,1);
    }
    else if( !prevContSameSection )
    {
      setInterpolated(cont,0);
    }
    if( !nextContSameSection )
    {
      setInterpolated(cont,0);
      numBoundaries++;
      wprint("section boundary slices: %d - %d\n", c+1, c+2);
    }
  }
  wprint("\n%d section boundaries (%d sections) were found\n", numBoundaries,
         numBoundaries+1 );
  
  undoFinishUnit( plug.view );              // FINISH UNDO
}


//------------------------
//-- Allows the user to specify a range of pixel values, and either
//-- (a) generates scattered point on each of these values
//-- (b) generates a series of new closed contours either around these regions.

void AnalysisTools::fillInRangeOfPixelValues()
{
  if( !isCurrObjValidAndShown() )
  {
    wprint("You have not selected a valid, displayed object");
    return;
  }
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = getCurrObj();
  
  int currZ = edit_getZOfTopZap();
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int   minVal               = 180;
  static int   maxVal               = 255;
  int          minZ                 = currZ+1;
  int          maxZ                 = currZ+1;
  static int   minNeighborsOn       = 4;
  static int   numRoundsOn          = 1;
  static int   minNeighborsOff      = 4;
  static int   numRoundsOff         = 1;
  static int   outputType           = 0;
  static bool  showResultAtEnd      = false;
  static int   objNumToUse          = 0;
  
  if( !isObjClosed(obj) )
    outputType = 0;
  
  CustomDialog ds("Generate Deformation Grid", this);
  
  ds.addRadioGrp( "output:",
                  "scattered points|"
                  "contours around on regions",
                  &outputType );
  
  ds.addSpinBox  ( "object to use:  ", 0, osize(imod), &objNumToUse, 1,
                   "The object number, inside which values will be calculated \n"
                   "If 0: entire tomogram will be used" );
  ds.addLabel    ( "-----" );
  ds.addLabel    ( "grey value range (for 'ON' pixels):" );
  ds.addSpinBox  ( " > min val:  ", 0, 255, &minVal, 5 );
  ds.addSpinBox  ( " > max val:  ", 0, 255, &maxVal, 5 );
  ds.addLabel    ( "range of slices:" );
  ds.addSpinBox  ( " > min z:  ", 1, plug.zsize, &minZ, 1 );
  ds.addSpinBox  ( " > max z:  ", 1, plug.zsize, &maxZ, 1 );
  ds.addLabel    ( "-----" );
  ds.addLabel    ( "Elimination of isolated values:" );
  ds.addSpinBox  ( " # round to remove ON pix:  ", 0, 10, &numRoundsOn, 1,
                   "The number of times to run the above algorithm" );
  ds.addSpinBox  ( "         min neighbors ON:  ", 0, 8, &minNeighborsOn, 1,
                   "If an on pixel has < this number of neighbors on, \n"
                   "of its 8 surrounding pixels, it will be turned off" );
  
  ds.addSpinBox  ( " # round to remove OFF pix:  ", 0, 10, &numRoundsOff, 1,
                   "The number of times to run the above algorithm" );
  ds.addSpinBox  ( "         min neighbors OFF:  ", 0, 8, &minNeighborsOff, 1,
                   "If an on pixel has < this number of neighbors on, \n"
                   "of its 8 surrounding pixels, it will be turned off" );
  ds.addLabel    ( "-----" );
  ds.addCheckBox ( "show result at end",   &showResultAtEnd,
                   "Will put the pixel values into the current slice" );
  
  
  
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  minZ--;
  maxZ--;
  
  if( minVal > maxVal || minZ > maxZ )
  {
    MsgBox("The 'min' must be less than or equal to 'max'!");
    return;
  }
  
  
  bool useObject = (objNumToUse > 0);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static float threshold    = 1;
  static bool  diagonal     = 1;
  static bool  polarity     = 0;
  static int   maxConts     = 10000;
  static int   minPtsLimit  = 5;
  static int   minAreaLimit = 10;
  
  if( outputType == 1 )
  {
    CustomDialog ds2("Contours From Image", this);;
    ds2.addLabel    ( "-- Processing Options --" );
    ds2.addCheckBox ( "diagonal",   &diagonal, "Will chamfer sharp corners" );
    ds2.addDblSpinBoxF( "threshold", 0, 255, &threshold, 1,1, "No idea" );
    ds2.addCheckBox ( "polarity",   &polarity, "Direction of contours?" );
    ds2.addLabel     ( "-- Limits --" );
    ds2.addSpinBox  ( "max contours", 1, MAX_INT, &maxConts, 10000,
                      "Used as a safety measure\nIf 0: is no limit" );
    ds2.addSpinBox  ( "min points", 1, 999, &minPtsLimit, 1,
                      "Min number of points contour must have to be added" );
    ds2.addSpinBox  ( "min area", 0, MAX_INT, &minAreaLimit, 1,
                      "Min area a contour must have to be added" );
    ds2.exec();
    if( ds2.wasCancelled() )
      return;
  }
  
  //## CREATE ARRAYS 'DATA' AND 'TEMP' THE SAME SIZE AS OUR SLICES: 
  
  const unsigned char PIX_OFF = 0;       // used for pixels inside  desired grey range
  const unsigned char PIX_ON  = 1;       // used for pixels outside desired grey range
  long pixInRange   = 0;                 // total pixels in range
  long pixInObj     = 0;                 // total pixels in object contours
  long pixTurnedOn  = 0;                 // total isolated 'off' pixels turned 'on'
  long pixTurnedOff = 0;                 // total isolated 'on' pixels turned 'off'
  long pixOnAtEnd   = 0;                 // total pixles 'on' at the end
  long contsAdded   = 0;                 // total contours added
  
  int w = plug.xsize;
  int h = plug.ysize;
  int nPixels = w * h;
  
  if(!plug.data) plug.data = (unsigned char *)malloc(nPixels * sizeof(unsigned char));
  if(!plug.temp) plug.temp = (unsigned char *)malloc(nPixels * sizeof(unsigned char));
  
  if( !plug.data || !plug.temp )
  {
    imodError(NULL, "Failed to get enough memory");
    return;
  }
  
  Iobj *objToUse = NULL;
  if( useObject )
  {
    objToUse = getObj( imod, objNumToUse-1 );
    if(!obj) {
      MsgBox("Invalid object ");
      return;
    }
  }
  
  
  //## FOR EACH SLICE: UPDATE 'DATA' FROM SLICE AND CREATE CONTOURS AROUND ON REGIONS:
  
  for(int z=minZ; z<=maxZ && z<plug.zsize; z++ )
  {
    cout << "Processing slice " << z+1 << endl;
    unsigned char **slice = ivwGetZSection( plug.view, z );
    if(!slice)
    {
      wprint("\aError retrieving slice %d", z+1);
      return;
    }
    
    if( useObject )
    {
      img_setAll( plug.data, PIX_OFF, w, h );
      img_changeValuesInsideObject( objToUse, PIX_ON, PIX_ON, false, plug.data, w, h, z);
      pixInObj = img_countOccurances( plug.data, PIX_ON, w, h );
    }
    else
    {
      img_setAll( plug.data, PIX_ON, w, h );
    }
    
    turnOnRangeOfValues( plug.data, PIX_ON, PIX_OFF, minVal, maxVal, true, slice, w, h );
    pixInRange += img_countOccurances( plug.data, PIX_ON, w, h );
    
    //## IF SPECIFIED: REDUCE NOISE
    
    if( numRoundsOn )
    {
      pixTurnedOff += img_noiseReduction( PIX_ON, PIX_OFF, minNeighborsOn,
                                      plug.data, w, h, numRoundsOn, plug.temp );
    }
    
    if( numRoundsOff )
    {
      pixTurnedOn += img_noiseReduction( PIX_OFF, PIX_ON, minNeighborsOff,
                                     plug.data, w, h, numRoundsOff, plug.temp );
    }
    
    pixOnAtEnd += img_countOccurances( plug.data, PIX_ON, w, h );
    
    
    //## USED DATA ARRAY TO CREATE CONTOUR(S):
    
    if( outputType == 0 )    // scattered points
    {
      Icont* newCont = imodContourNew();
      
      for(int y=0; y<plug.ysize; y++)
        for(int x=0; x<plug.xsize; x++)
          if( pix(plug.data,x,y) == PIX_ON )
            imodPointAppendXYZ( newCont, x+0.5f, y+0.5f, z  );
      
      if( psize(newCont) )
      {
        edit_addContourToObj( obj, newCont, true);
        contsAdded++;
      }
      imodContourDelete( newCont );
    }
    
    
    else if( outputType == 1 )    // contours around on regions
    {      
      //## CREATE CONTOURS AROUND "ON" VALUES:
      
      unsigned char mask   = PIX_ON;
      
      vector<IcontPtr> contSegs;
      long nconts = img_contoursFromImagePoints( contSegs, plug.data, slice,
                                                  plug.xsize, plug.ysize, z,
                                                  mask, diagonal,
                                                  threshold, polarity, maxConts );
      
      wprint( "%d conts found on slice %d \n", nconts, z+1 );
      
      for(long i=0;i<contSegs.size();i++)
      {
        Icont *cont = contSegs[i].cont;
        if( (!cont) || psize(cont)           < minPtsLimit
                    || imodContourArea(cont) < minAreaLimit )
          continue;
        
        edit_addContourToObj( obj, cont, true);
        contsAdded++;
      } 
      
      deleteContours( contSegs );
    }
    
    if( showResultAtEnd )
      img_changeSliceData(slice,plug.data,w,h,true,true);
    
  }
  
  if(contsAdded)
    undoFinishUnit( plug.view );              // FINISH UNDO
  
  //## PRINT RESULTS FOR NUMBER OF PIXELS ON:
  
  int numSlices          = maxZ+1 - minZ;
  long candidatePix      = (useObject) ? pixInObj : nPixels;
  int percentIsolatedOff = calcPercentInt( pixTurnedOff, pixInRange );
  int percentIsolatedOn  = calcPercentInt( pixTurnedOn , pixInRange );
  int percentOnInitial   = calcPercentInt( pixInRange,   candidatePix );
  int percentOnAtEnd     = calcPercentInt( pixOnAtEnd,   candidatePix );
  
  wprint( "\nFILL RANGE OF VALUES:\n"  );
  wprint( "\n" );
  wprint( " range of values: %d-%d\n", minVal, maxVal );
  if( numSlices > 1 )
    wprint( " range of slices: %d-%d\n", minZ+1, maxZ+1 );
  wprint( " contours added: %d\n", contsAdded );
  wprint( "\n" );
  wprint( " # pixels in obj: %d\n", pixInObj );
  if( useObject )
    wprint( " # pixels in obj: %d\n", pixInObj );
  else
    wprint( " # pixels in tomogram:  \t%d (%dx%dx%d)\n", nPixels, w, h, numSlices );
  if( numRoundsOn || numRoundsOff )
    wprint( " # in range: \t%d (%d%%)\n", pixInRange, percentOnInitial );
  if( numRoundsOn )
    wprint( " # turned off: %d (-%d%% of on) (%d neig x %d)\n",
            pixTurnedOff, percentIsolatedOff, minNeighborsOn, numRoundsOn );
  if( numRoundsOff )
    wprint( " # turned on: %d (+%d%% of on) (%d neig x %d)\n",
            pixTurnedOn,  percentIsolatedOn,  minNeighborsOff, numRoundsOff );
  wprint( " # on at end : %d (%d%%)\n", pixOnAtEnd, percentOnAtEnd );
}


//------------------------
//-- Deletes the a range of slices from the model by removing contours
//-- from those slices and shifting down all points above.
//-- If specified, it will shifting open contour or scattered points
//-- up or down so they won't be lost.

void AnalysisTools::deleteSlice()
{
  Imod *imod  = ivwGetModel(plug.view);
  int currZ   = edit_getZOfTopZap();
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int          minSliceDel    = currZ+1;
  int          maxSliceDel    = currZ+1;
  static bool  keepOpenPts       = true;
  static bool  keepClosedConts   = false;
  static int   directionToShift  = 1;
  static bool  movePointsAboveDown  = true;
  directionToShift = (directionToShift==1) ? 0 : 1;
  
  
  CustomDialog ds("Delete Slices", this);
  
  ds.addLabel    ( "slices to delete (i.e. remove points):" );
  ds.addSpinBox  ( " > min val:  ", 0, plug.zsize, &minSliceDel, 1 );
  ds.addSpinBox  ( " > max val:  ", 0, plug.zsize, &maxSliceDel, 1 );
  
  ds.addLabel    ( "-----" );
  ds.addCheckBox ( "keep open/scattered points",  &keepOpenPts, "" );
  ds.addCheckBox ( "keep closed points",          &keepClosedConts, "" );
  
  ds.addLabel    ( "-----" );
  ds.addRadioGrp( "direction to shift (kept) points:",
                  "up|"
                  "down",
                  &directionToShift );
  ds.addLabel    ( "-----" );
  ds.addCheckBox ( "move all points above down",  &movePointsAboveDown, "" );
  ds.addLabel    ( "-----" );
  ds.addLabel    ( "WARNING: Save first as you \n"
                   "can't undo this operation" );
  
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  //## ASSESS IMPUT:
  
  minSliceDel--;
  maxSliceDel--;
  
  if( maxSliceDel < minSliceDel )
    return;
  
  int sliceToMoveKeptPtsTo = (directionToShift==0) ? maxSliceDel+1 : minSliceDel-1;
  int slicesToDelete = maxSliceDel-minSliceDel+1;
  
  //## DELETE OR MOVE ALL POINTS IN THIS Z RANGE:
  
  long numPtsDeleted = 0;
  long numPtsKept    = 0;
  
  for(int o=osize(imod)-1; o>=0; o-- )
  {
    Iobj *obj = getObj(imod,o);
    
    for(int c=csize(obj)-1; c>=0; c-- )
    {
      Icont *cont = getCont(obj,c);
      bool closed = isContClosed(obj,cont);
      
      for(int p=psize(cont)-1; p>=0; p-- )
      {
        Ipoint *pt = getPt(cont,p);
        if( pt->z < minSliceDel || pt->z > maxSliceDel )
          continue;
        
        if( ( closed && keepClosedConts ) || ( !closed && keepOpenPts ) )
        {
          pt->z = sliceToMoveKeptPtsTo;
          numPtsKept++;
        }
        else
        {
          imodPointDelete(cont,p);
          numPtsDeleted++;
        }
      }
    }
  }
  
  //## SHIFT ALL POINTS ABOVE THS Z RANGE DOWN:
  
  long numPtsMovedDown = 0;
  
  if(movePointsAboveDown)
  {
    for(int o=osize(imod)-1; o>=0; o-- )
    {
      Iobj *obj = getObj(imod,o);
      for(int c=csize(obj)-1; c>=0; c-- )
      {
        Icont *cont = getCont(obj,c);
        for(int p=psize(cont)-1; p>=0; p-- )
        {
          if( getPt(cont,p)->z > maxSliceDel ) {
            numPtsMovedDown++;
            getPt(cont,p)->z -= slicesToDelete;
          }
        }
      }
    }
  }
  
  //## PRINT OUTPUT:
  
  wprint( "\nDELETE RANGE OF SLICES:\n"  );
  wprint( "\n" );
  wprint( " number of slices deleted: %d (slice %d-%d)\n",
          slicesToDelete, minSliceDel+1, maxSliceDel+1 );
  wprint( " total points deleted:     %d\n", numPtsDeleted );
  wprint( " total points kept:        %d\n", numPtsKept );
  wprint( " total points shited down: %d\n", numPtsMovedDown );
}



//------------------------
//-- Prints information about the sections within the current tomogram

void AnalysisTools::printSectionInfo()
{

}



//------------------------
//-- Gives a choice of several other options for the user.

void AnalysisTools::moreActions()
{
  //## DISPLAY CONTEXT MENU:
  
  int x = this->x() + this->widget1->x() + this->moreActionsButton->x();
  int y = this->y() + this->widget1->y() + this->moreActionsButton->y();
  
  QPoint pos( x, y );
  actionMenu->exec( pos );
  
  /*
  //## GET USER INPUT FROM CUSTOM DIALOG:
   
  CustomDialog ds("Perform Action", this);
  int ID_ACTION = ds.addRadioGrp( "action:",
                                  "generate deformation grid|"
                                  "deform objects using deformation grid|"
                                  "fill range of pixel values|"
                                  "delimit section boundaries|"
                                  "delete contours from slices|"
                                  "duplicate objects|"
                                  "generate deformation grid|"
                                  "print section information",
                                  &plug.selectedAction,
                                  "",
                                  "Generates a deformation grid using current object|"
                                  "Uses the current deformation grid to shift all"
                                    "contours/points over a range of objects|"
                                  "Lets you specify where section boundaries "
                                    "start|"
                                  "Uses the grey values to try and determine the "
                                    "boundary of sections on every slice|"
                                  "Deletes contours from specified slices|"
                                  "Duplicates an entire range of objects|"
                                  "Prints information about the sections within "
                                    "the tomogram" );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  switch(plug.selectedAction)
  {
    case(0):      // generate deformation grid
      generateDefGrid();
      break;
      
    case(1):      // deform objects using deformation grid
      deformObjectsUsingDefGrid();
      break;
      
    case(2):      // fill range of pixel values
      fillInRangeOfPixelValues();
      break;
      
    case(3):      //  delimit section boundaries
      delimitSectionBoundaries();
      break;
      
    case(4):      // 
      ;
      break;
  }
  */
  
  plug.window->drawExtraObject(false);
  ivwRedraw( plug.view );
}

//------------------------
//-- Allows user to change other plugin values/settings.

void AnalysisTools::moreSettings()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  CustomDialog ds( "More Settings", this );
  ds.addLabel   ( "--- SETTINGS ---" );
  ds.addCheckBox( "use arrow keys to change sections", 
                                          &plug.arrowKeysSections );
  ds.addLabel   ( "--- DEFORMATION GRID ---" );
  ds.addCheckBox( "show vectors in ZAP", &plug.grid_showGrid );
  ds.addCheckBox( "show boundaries in ZAP", &plug.grid_showGridBounds );
  ds.addCheckBox( "show (green) starting grid in ZAP", &plug.grid_showGridGreen );
  ds.addCheckBox( "show grid in ModelView", &plug.grid_showModelView );
  ds.addSpinBox ( "grid line display width:",1, 50, &plug.grid_displayLineW, 1,
                  "The thickness of lines used for grid lines" );
  ds.addSpinBox ( "grid spheres:",0, 50, &plug.grid_displaySpheres, 1,
                  "The sphere size for grid lines" );
  ds.addSpinBox ( "grid line display width dotted:",1, 50, &plug.grid_displayLineWDot, 1,
                  "The thickness of lines used for dotted grid lines" );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  drawExtraObject(true);
  //ivwRedraw( plug.view );
}



//## PROTECTED:


//------------------------
//-- Called to display help window.

void AnalysisTools::buttonPressed(int which)
{
  if (!which)
    close();
  else
  {
    imodShowHelpPage("../plughelp/analysistools.html#TOP");
  }
}

//------------------------
//-- Window closing event handler - removes this pluging from the imod dialog manager

void AnalysisTools::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)plug.window);
  clearExtraObj();
  ivwFreeExtraObject(plug.view, plug.extraObjG);
  ivwFreeExtraObject(plug.view, plug.extraObjGB);
  ivwTrackMouseForPlugs(plug.view, 0);
  
  if(plug.data)
    free(plug.data);
  if(plug.temp)
    free(plug.temp);
  
  plug.window->saveSettings();
  
  plug.view = NULL;
  plug.window = NULL;
  e->accept();
}


//------------------------
//-- Key press event handler - closes on escape or passes on event to "ivwControlKey"

void AnalysisTools::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

//------------------------
//-- Key release event hander - passes on event to "ivwControlKey"

void AnalysisTools::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}






//############################################################




//----------------------------------------------------------------------------
//
//          SIMPLE FUNCTIONS:
//
//----------------------------------------------------------------------------



//---------------------------------
//-- Returns a pointer to the currently selected object.

Iobj *getCurrObj()
{
  Imod *imod  = ivwGetModel(plug.view);
  return ( imodObjectGet(imod) );
}


//---------------------------------
//-- Returns a pointer to the currently selected contour.

Icont *getCurrCont()
{
  Imod *imod  = ivwGetModel(plug.view);
  return ( imodContourGet(imod) );
}


//---------------------------------
//-- Returns a pointer to the currently selected point.

Ipoint *getCurrPt()
{
  Imod *imod = ivwGetModel(plug.view);
  return ( imodPointGet(imod) );
}


//---------------------------------
//-- Returns true if the object is valid and has it's draw flag on.

bool isCurrObjValidAndShown()
{
  Iobj *obj = getCurrObj();
  return isObjectValidAndShown(obj);
}


//---------------------------------
//-- Returns true is a valid contour is selected.

bool isCurrContValid()
{
  return ( isContValid( getCurrCont() ) );
}

//---------------------------------
//-- Returns true is a valid point is selected.

bool isCurrPtValid()
{
  Ipoint *pt = getCurrPt();
  return (pt!=NULL);
}




//------------------------
//-- Removes all contours in the object which have their delete flag set to 1

int removeAllDeleteFlaggedContoursFromObj( Iobj *obj )
{
  Icont *cont;
  int numRemoved = 0;
  for( int c=csize(obj)-1; c>=0; c-- )
  {
    cont = getCont(obj, c);
    if( isDeleteFlag( cont ) && isInterpolated( cont ) )
    {
      undoContourRemovalCO( plug.view, c );              // REGISTER UNDO
      imodObjectRemoveContour( obj, c );
      numRemoved++;
    }
  }
  return numRemoved;
}




//----------------------------------------------------------------------------
//
//          EDITING FUNCTIONS:
//
//----------------------------------------------------------------------------



//------------------------
//-- Gets the slice value of the top Zap window or returns -1 if no Zap

int edit_getZOfTopZap()
{
  int currSlice = -1;
  int noZap = ivwGetTopZapZslice(plug.view, &currSlice);   // gets current slice
  if (noZap == 1)   // if no top ZAP window:
    return (-1);
  return (currSlice);
}


//------------------------
//-- Sets the top ZAP window to focus on the selected point and slice.

int edit_setZapLocation( float x, int y, int z, bool redraw )
{
  ivwSetLocation( plug.view, x, y, z );
  if( redraw )
    ivwDraw( plug.view, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC );
  return z;
}


//------------------------
//-- Changes the Z slice by calling page up or page down

int edit_changeSelectedSlice( int changeZ, bool redraw )
{
  int ix, iy, iz;
  ivwGetLocation( plug.view, &ix, &iy, &iz );
  int newZ = iz+changeZ;
  ivwSetLocation( plug.view, ix, iy, newZ);
  plug.window->drawExtraObject(false);
  if( redraw )
    ivwDraw( plug.view, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC );
  return newZ;
}



//------------------------
//-- Adds a new contour to the specified object

int edit_addContourToObj( Iobj *obj, Icont *cont, bool enableUndo )
{
  Icont *newCont = imodContourDup( cont );    // malloc new contour and don't delele it
  int numConts = csize(obj);
  if(enableUndo)
    undoContourAdditionCO( plug.view, numConts );    // REGISTER UNDO
  int newContPos = imodObjectAddContour( obj, newCont );
  free(newCont);
  return newContPos;
}


//------------------------
//-- Removes all contours in the object which have their delete flag set to 1

int edit_removeAllFlaggedContoursFromObj( Iobj *obj )
{
  Icont *cont;
  int numRemoved = 0;
  for( int c=csize(obj)-1; c>=0; c-- )
  {
    cont = getCont(obj, c);
    if( isDeleteFlag( cont ) )
    {
      undoContourRemovalCO( plug.view, c );
      imodObjectRemoveContour( obj, c );
      numRemoved++;
    }
  }
  return numRemoved;
}


//----------------------------------------------------------------------------
//
//          ANALYSIS FUNCTIONS:
//
//----------------------------------------------------------------------------









//---------------------------------

struct ContInSurface          // Used to stores information about a single contour within a surface
{
  int idx;                // the index of the contour within the object
  int z;                  // stores the slice which the contour is on
  Ipoint mbrLL, mbrUR;    // stores a minmum bounding rectangle around the contour
  Ipoint centerOfArea;    // stores the centroid of the contour
  float area;            // stores the area of the contour
  
  float avgDistToCenter;    // stores the average distance from any point in this contour to it's center of area.
  float minDistToCenter;    // stores the minimum distance from any point in this contour to it's center of area.
  float maxDistToCenter;    // stores the maximum distance from any point in this contour to it's center of area.
  
  float avgDistToCenterSurf;  // stores the average distance from any point in this contour to the centroid of the surface (using the "currZScale").
  float minDistToCenterSurf;  // stores the minimum distance from any point in this contour to the centroid of the surface (using the "currZScale").
  float maxDistToCenterSurf;  // stores the maximum distance from any point in this contour to the centroid of the surface (using the "currZScale").
  
  ContInSurface( int _idx, int _z ) {      //-- Default constructor
    idx = _idx;
    z   = _z;
    mbr_reset(&mbrLL,&mbrUR);
  }
  friend bool operator< (const ContInSurface &a, const ContInSurface &b) {    //-- Less than operation based on the contour's "z" value
    return (a.z < b.z);
  }
};

const string CL_OUTOFBOUNDS = "out_of_bounds";
const string CL_DOUBLETRUNC = "double_truncated";
const string CL_TRUNCFRAG   = "truncated_fragment";
const string CL_TRUNCHALF   = "_truncated_half";
const string CL_COMPLETE    = "_complete";
const string CL_MISSING     = " * ";


struct Surface            // Stores all the contours in a single surface, plus information about that object
{
  vector<ContInSurface> co;      // stores a list of indexes to contours within the surface
  
  bool roundEnough;         // set to true if the surface points don't deviate more than "maxDeviation" from the sphere's center.
  bool accepted;            // set to true if the surface is a truncated half or a complete sphere.
  bool truncated;           // set to true if the surface touches the bottom or top of a section boundary        
  string classification;
  
  int slicesSpanned;      // number of slices the surface spans
  int contLargestZ;        // the z value of the contour with the largest area
  int middleSliceZ;        // the z value of the "middle slice" within the "co" vector
  
  Ipoint mbrLL, mbrUR;    // stores a minimum bounding rectangle around the surface
  
  float spanX;            // the length in X of the mbr
  float spanY;            // the length in Y of the mbr
  float spanZ;            // the length in Z of the mbr
  float spanZEst;          // is set to ~spanZ + 1 ... OR  .. twice the radius if the surface is truncated
  
  float avgXY;            // the average of spanX and spanY
  
  Ipoint mbrCenter;        // a point in the center of the minimum bounding rectangle, or, if the surface is truncated... in the middle of the largest contour (which we assume represents the middle of the surface)
  
  Ipoint sphere;           // a point with a radius, represented a "sphere of best fit"     .... as calculated using the point_findOptimalZScaleAndCenterOfPts function.
  float sphereRad;         // 
  float sphereZScale;      // the best radius to fit sphere over the points in this surface .... as calculated using the point_findOptimalZScaleAndCenterOfPts function.
  float sphereAvgResid;    // the average residual for all points as fitted with "sphere"   .... as calculated using the point_findOptimalZScaleAndCenterOfPts function.
  float sphereFractInZ;    // the fraction of the "sphere" represented with contours (eg: will be 0.5 if only a hemisphere is drawn in)
  float sphereMaxDev;      // coefficient representing the difference between the closest and furtherest point ..... (the distance from furthest point - distance to the closest point) / the distance from furthest point
  float sphereStdDev;      // the standard deviation of points from the center showing how much they deviates from a perfect spherical profile
  float sphereWeightSurf;  // the weight used for this surface
  
  float zScaleEst;          // stores the value which represents the best z scale value such that the Z span of the minimum bounding box is ~equal to the span in X and Y.
  
  Surface()  {      //-- Default constructor
    accepted  = false;
    truncated = false;
    classification = "";
    mbr_reset(&mbrLL,&mbrUR);
  }
};




//---------------------------------
//-- Estimates the zScale value by finding all the approximately spherical surfaces,
//-- and comparing their span/diameter in X & Y to their diameter in Z... and
//-- returning the weighted Z scale value what would make the values equal.
//-- 

float analysis_estimateZScale( int objIdx, float currZScale, bool useHemispheres,
                               bool calcSpheres, float maxDeviation,
                               float startChangeZ, float accuracyZScale,
                               int maxItsCenter, float maxZChange,
                               bool showSurfClassif, bool appendToCSV,
                               bool summaryCSV, bool printOnlyAccepted,
                               bool showCenters )
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj   = getObj( imod, objIdx );
  int nconts  = csize(obj);
  
  float modelZScale    = imodGetZScale(imod);
  
  //## POPULATE CONTOURS THE SURFACE VECTOR:
  
  int nSurfaces = imodObjectGetValue( obj, IobjMaxSurface);    // returns the maximum "surfNum" value for any contour in the object + 1
  vector<Surface> surf;              // stores a list of contours in each surface and information about the surface itself
  surf.resize( nSurfaces );            // allow enough room for all surfaces
  
  for (int c=0; c<csize(obj); c++)        // for each contour: if it has a valid surfNum, add it in the appropriate surface
  {
    Icont *cont = getCont(obj,c);
    int surfNum = imodContourGetSurface( cont ) - 1;
    if( surfNum >=0 && surfNum < (int)surf.size()  )
      surf[ surfNum ].co.push_back( ContInSurface(c,getZInt(cont)) );
  }
  
  //## SORT LIST OF CONTOURS WITHIN EACH SURFACE BY Z VALUE:
  
  for(int s=0; s<(int)surf.size(); s++)
    surf[s].co = vector_sort( surf[s].co );
  
  
  //## CALCULATE PROPERTIES (AREA, MBR & CENTROID ETC) FOR EACH CONTOUR (IN EVERY SURFACE):
  
  for(int s=0; s<(int)surf.size(); s++)
  {
    for(int c=0; c<(int)surf[s].co.size(); c++)
    {
      Icont *cont = getCont( obj, surf[s].co[c].idx );
      imodContourGetBBox( cont, &surf[s].co[c].mbrLL, &surf[s].co[c].mbrUR );
      cont_getCentroid( cont, &surf[s].co[c].centerOfArea );
      surf[s].co[c].area = imodContourArea( cont );
      
      cont_findMinMaxAndAvgDistFromPt( &surf[s].co[c].centerOfArea, cont, 1, surf[s].co[c].minDistToCenter, surf[s].co[c].maxDistToCenter, surf[s].co[c].avgDistToCenter );
    }
  }
  
  
  //## CALCULATE PROPERTIES (MBR, LARGEST CONT, SPAN, ZSCALE) FOR EACH SURFACE:
  
  cout <<endl<< "ANALYSING SURFACES:" << endl;
  
  Icont *allPtsInSurf = imodContourNew();   // stores all points in this surface
                                            // ... fed into point_findCenterOfPts()
  
  for(int s=0; s<(int)surf.size(); s++)
  {
    //cout << "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b";        // shows progress
    cout << "   " << s+1 << " / " << (int)surf.size() << "    ("  << calcPercentInt( s+1,(int)surf.size() ) << "%)";
    //cout << endl;
    flush(cout);
    
    imodContourDefault( allPtsInSurf );
    float largestArea = 0.0;
    
    for(int c=0; c<(int)surf[s].co.size(); c++)
    {
      Icont *cont = getCont( obj, surf[s].co[c].idx );
      mbr_addPt( &surf[s].co[c].mbrLL, &surf[s].mbrLL, &surf[s].mbrUR );
      mbr_addPt( &surf[s].co[c].mbrUR, &surf[s].mbrLL, &surf[s].mbrUR );
      
      if( surf[s].co[c].area > largestArea ) {      // finds largest contour in surface
        largestArea = surf[s].co[c].area;
        surf[s].contLargestZ = getZInt( cont );
      }
      for ( int p=0; p<psize(cont); p++ )
        cont_copyPts( cont, allPtsInSurf, false );
    }
    
    
    surf[s].slicesSpanned = (int)(surf[s].mbrUR.z - surf[s].mbrLL.z);
    surf[s].mbrCenter     = line_getPtHalfwayBetween( &surf[s].mbrLL, &surf[s].mbrUR );
    surf[s].middleSliceZ  = (int)surf[s].mbrCenter.z;
    
    surf[s].spanX = surf[s].mbrUR.x - surf[s].mbrLL.x;
    surf[s].spanY = surf[s].mbrUR.y - surf[s].mbrLL.y;
    surf[s].spanZ = surf[s].mbrUR.z - surf[s].mbrLL.z;              // maybe use something bit more sophisticated (+1) ?
    surf[s].avgXY = (surf[s].spanX + surf[s].spanY) / 2.0;
    surf[s].spanZEst  = surf[s].spanZ;
    surf[s].zScaleEst = fDiv( surf[s].avgXY , surf[s].spanZEst );        // estimate Z scale for this surface based on span of MBR
    
    float avgR;
    
    if( !calcSpheres ) {
      surf[s].sphere = point_findCenterOfPts( obj, allPtsInSurf, &surf[s].mbrCenter, avgR,
                                              currZScale, surf[s].sphereAvgResid, maxItsCenter );
      surf[s].sphereZScale = currZScale;
    }
    else {
      surf[s].sphere = point_findOptimalZScaleAndCenterOfPts( obj, allPtsInSurf, &surf[s].mbrCenter, avgR, currZScale, surf[s].sphereZScale, surf[s].sphereAvgResid, startChangeZ, accuracyZScale, maxItsCenter );    // slow process (bottleneck)
    }
    
    surf[s].sphereRad = avgR;
    surf[s].sphereFractInZ = MIN( fDiv( (surf[s].spanZ), (avgR * 2.0f / surf[s].sphereZScale ) ), 1.0f );
    Ipoint scalePt;
    setPt( &scalePt, 1,1,surf[s].sphereZScale );
    surf[s].sphereMaxDev   = point_maxDiffInDistToPoint( allPtsInSurf, &surf[s].sphere, &scalePt );
    surf[s].sphereStdDev   = point_stdDevDistToPoint   ( allPtsInSurf, &surf[s].sphere, &scalePt );
    
    cout << "    \tzSpan=" << surf[s].spanZ << " \tradius=" << surf[s].sphereRad << endl;   //%%%%%%%
    flush(cout);                                                      //%%%%%%%%%%%
  }
  imodContourDelete(allPtsInSurf);

  
  //## CLASSIFY SURFACES:
  
  const float TRUNC_HALF_BUFFER = 5.0;
  const float SAFE_BUFFER = 3.0;
  
  Ipoint mbrSafeLL, mbrSafeUR;
  setPt( &mbrSafeLL, SAFE_BUFFER, SAFE_BUFFER, -20 );
  setPt( &mbrSafeUR, plug.xsize-SAFE_BUFFER, plug.ysize-SAFE_BUFFER, plug.zsize+20 );
  
  //MBRect mbrSafe;              // an MBR generated just inside the walls of the tomogram/model, and used to check if any surfaces touch the side.
  //mbrSafe.setAll( Ipoint( SAFE_BUFFER, SAFE_BUFFER, -20 ), Ipoint( mod.xmax - SAFE_BUFFER, mod.ymax - SAFE_BUFFER, mod.zmax+20 ) );
  
  cout <<endl<<endl<< "POTENTIAL PROBLEMS:" <<endl;
  
  for(int s=0; s<(int)surf.size(); s++)
  {
    if( (int)surf[s].slicesSpanned+1 != (int)surf[s].co.size() )      // if there is not one contour per slice in the surface (if there are missing contours or branches): reject it
    {
      cout << "surface " << s << " has > 1 per slice" << "  span=" << surf[s].slicesSpanned << " conts=" << (int)surf[s].co.size() << endl;    //%%%%
      surf[s].classification = CL_MISSING;
      continue;
    }
    
    bool touchesTop     = surf[s].mbrLL.z <= 0;          // says if the surface touches the top of the section boundary
    bool touchesBottom = surf[s].mbrUR.z >= plug.zsize-1;      // says if the surface touches the bottom of the section boundary
    
    bool touchesSide =  !mbr_isBBoxInsideBBox( &surf[s].mbrLL, &surf[s].mbrUR,
                                               &mbrSafeLL, &mbrSafeUR );
    
    if( touchesSide )                   // if the surface touches the sides of the section boundary: mark it as out of bounds and reject it
    {
      //cout << "surface " << s << " is touching the sides" << endl;  //%%%%%%
      surf[s].classification = CL_OUTOFBOUNDS;
      
      float   sphereFractInZ = surf[s].sphereFractInZ;
      float   sphereFractInX = MIN( fDiv( (surf[s].spanX), (surf[s].sphereRad * 2.0f ) ), 1.0f );
      float   sphereFractInY = MIN( fDiv( (surf[s].spanY), (surf[s].sphereRad * 2.0f ) ), 1.0f );
      surf[s].sphereFractInZ = surf[s].sphereFractInZ * sphereFractInX * sphereFractInY;
      
      //cout << "Out of bounds sphere" << " s=" << s << " sphereFractInX=" << sphereFractInX << " sphereFractInY=" << sphereFractInY << " sphereFractInZ=" << sphereFractInZ << " surf[s].sphereFractInZ=" << surf[s].sphereFractInZ << endl;                  //%%%%%%
      
      surf[s].accepted = false;
      continue;
    }
    else if( touchesTop || touchesBottom )              // if it touches the bottom or top of the tomogram: mark it as truncated, and decide whether to reject it
    {
      surf[s].truncated = true;
      
      if( touchesTop && touchesBottom )  {              // if it touches BOTH bottom and top: mark it as float truncated and reject it
        surf[s].classification = CL_DOUBLETRUNC;
        surf[s].accepted = false;
        continue;
      }
      else                              // else (if this is truncated only on one side):
      {
        int largestContZ = surf[s].contLargestZ;
        if( largestContZ <= TRUNC_HALF_BUFFER || largestContZ >= plug.zsize-TRUNC_HALF_BUFFER )  {    // if largest contour is touching or very near the top or bottom of the tomorgram: mark it as a truncated fragment and reject it
          surf[s].classification = CL_TRUNCFRAG;
          surf[s].accepted = false;
          continue;
        }
        else {                              // else (if the largest contour is in range): mark it as a truncated half and accept it (for now)
          surf[s].mbrCenter.z = largestContZ;                                        // change the "middle" of the surface to the center of the largest contour
          surf[s].spanZEst = MAX( (largestContZ-surf[s].mbrLL.z), (surf[s].mbrUR.z-largestContZ) ) * 2.0;        // here we estimate that the distance from the top/bottom to the largest contour in Z is the radius, and multiply it by two
          surf[s].zScaleEst = fDiv( surf[s].avgXY , surf[s].spanZEst );                        // estimate Z scale
          surf[s].classification = CL_TRUNCHALF;        
          surf[s].accepted = true;
        }
        //cout << "hemisphere found ... z=" << largestContZ << endl;      //%%%%%%
      }
    }
    else {
      surf[s].classification = CL_COMPLETE;
      surf[s].accepted = true;
    }
    
  }
  
  //## DETERMINE WHICH SURFACES ARE ROUND ENOUGH:
  
  for(int s=0; s<(int)surf.size(); s++)
  {
    float diffInZScale  = ABS(currZScale - surf[s].sphereZScale);
    surf[s].roundEnough = ( (surf[s].sphereMaxDev <= maxDeviation)
                            && ( diffInZScale < maxZChange )
                            && ( surf[s].classification != CL_MISSING ) );
    
    if( surf[s].sphereRad > plug.xsize )
    {
      surf[s].roundEnough = false;
      cout << "surface " << s << " failed the optimal zScale algorithm" << endl;  //%%%%%%
    }
  }
  
  
  //## TALLY UP THE DIFFERENT TYPES OF SURFACES
  
  int nMissing = 0, nOutOfBounds = 0, nDoubleTrunc = 0, nTrncFrag = 0, nTrncHalf = 0, nComplete = 0;  // a tally of each surface classification
  int nRoundEnough = 0;    // used to tally number of surfaces which have less than "maxDeviation" deviation in spherical profile
  int nAccepted = 0;      // used to tally number of accepted surfaces
  int nTruncated = 0;      // used to tally number of truncated surfaces
  
  for(int s=0; s<(int)surf.size(); s++)
  {
    {
      if      (surf[s].classification == CL_MISSING)      nMissing++;
      else if (surf[s].classification == CL_OUTOFBOUNDS)  nOutOfBounds++;
      else if (surf[s].classification == CL_DOUBLETRUNC)  nDoubleTrunc++;
      else if (surf[s].classification == CL_TRUNCFRAG)    nTrncFrag++;
      else if (surf[s].classification == CL_TRUNCHALF)    nTrncHalf++;
      else if (surf[s].classification == CL_COMPLETE)     nComplete++;
    }
    
    if( surf[s].roundEnough )   nRoundEnough++;     // tally number of round surfaces
    if( surf[s].accepted )      nAccepted++;        // tally number of accepted surfaces
    if( surf[s].truncated )     nTruncated++;       // tally number of tuncated surfaces
  }
  

  //## CACULATE Z-SCALE ESTIMATE:
  
  float totCompleteNormXY  = 0.0;  // tallies avgXY across all complete surfaces
  float totCompleteNormZ   = 0.0;  // tallies spanZEst across all complete surfaces
  float totTruncatedNormXY = 0.0;  // tallies avgXY across all truncated half surfaces
  float totTruncatedNormZ  = 0.0;  // tallies spanZEst across all truncated half surfaces
  
  float totalSpanInX = 0;
  float totalSpanInY = 0;
  float totalSpanInZ = 0;
  
  for(int s=0; s<(int)surf.size(); s++)
  {
    if( surf[s].accepted && surf[s].roundEnough )
    {
      float totalAvgXYPlusZ = surf[s].avgXY + surf[s].spanZEst;
      float normalXY = fDivCustom( surf[s].avgXY    , totalAvgXYPlusZ, 1 );
      float normalZ  = fDivCustom( surf[s].spanZEst , totalAvgXYPlusZ, 1 );
      
      if( surf[s].classification == CL_COMPLETE ) {
        totCompleteNormXY += normalXY;
        totCompleteNormZ  += normalZ;
        
        totalSpanInX += surf[s].spanX;
        totalSpanInY += surf[s].spanY;
        totalSpanInZ += surf[s].spanZEst;
      }
      else if ( surf[s].classification == CL_TRUNCHALF ) {
        totTruncatedNormXY += normalXY;
        totTruncatedNormZ  += normalZ;
      }
    }
    else
    {
      surf[s].zScaleEst = 0;
    }
  }
  
  float zCrude     = fDivCustom( ((totalSpanInX+totalSpanInY)/2.0), totalSpanInZ, 0 );
  float zComplete  = fDivCustom( totCompleteNormXY,  totCompleteNormZ,  0 );
  float zTruncated = fDivCustom( totTruncatedNormXY, totTruncatedNormZ, 0 );
  
  float TRUNCATED_WEIGHT = 0.5;      // a weighting applied to (accepted) truncated surfaces such that they count less than (accepted) surfaces which are not truncated
  float zWeighted  = fDivCustom( ( TRUNCATED_WEIGHT*(zTruncated*nTrncHalf) + (zComplete*nComplete) ), ( TRUNCATED_WEIGHT*nTrncHalf + nComplete), 0 );
  
  
  //## TALLY UP RESULTS FOR Z-SCALE CUMULATIVE
  
  float totSphereXY = 0.0;
  float totSphereZ  = 0.0;
  int nSpheresCounted = 0;
  float totSphereWeight = 0.0;
  
  for(int s=0; s<(int)surf.size(); s++)
  {
    if( surf[s].roundEnough )
    {
      float sphereXY    = fDivCustom( surf[s].sphereZScale, (surf[s].sphereZScale + 1.0), 1.0 );
      float sphereZ     = 1.0 - sphereXY;
      surf[s].sphereWeightSurf = SQ(surf[s].sphereFractInZ);      // MIGHT USE A SYSTEM OF TAKING THE SQUARE LATER
      
      //cout << " surf[s].sphereZScale=" << surf[s].sphereZScale << " surf[s].sphereFractInZ=" << surf[s].sphereFractInZ << " sphereXY=" << sphereXY << " sphereZ=" << sphereZ << endl;                  //%%%%%%
      
      totSphereXY += (surf[s].sphereWeightSurf * sphereXY);
      totSphereZ  += (surf[s].sphereWeightSurf * sphereZ);
      
      totSphereWeight += surf[s].sphereWeightSurf;
      nSpheresCounted++;
    }
  }
  float zSpheres = fDivCustom( totSphereXY, totSphereZ, 0 );
  
  //## TALLY UP AVERAGES FOR SPHERES:
  
  float totSphereMeanRadius = 0.0;
  float totSphereMaxDev     = 0.0;
  float totSphereAvgResid   = 0.0;
  float totSphereStdDev     = 0.0;
  
  for(int s=0; s<(int)surf.size(); s++)
  {
    if( surf[s].roundEnough )
    {
      totSphereMeanRadius += ((surf[s].sphereRad)      * surf[s].sphereWeightSurf);
      totSphereMaxDev     += ((surf[s].sphereMaxDev)   * surf[s].sphereWeightSurf);
      totSphereAvgResid   += ((surf[s].sphereAvgResid) * surf[s].sphereWeightSurf); 
      totSphereStdDev     += ((surf[s].sphereStdDev)   * surf[s].sphereWeightSurf);
    }
  }
  float avgSphereMeanRadius  = fDivCustom( totSphereMeanRadius, totSphereWeight, 0);
  float avgSphereMaxDev      = fDivCustom( totSphereMaxDev, totSphereWeight, 0);
  float avgSphereAvgResid    = fDivCustom( totSphereAvgResid, totSphereWeight, 0);
  float avgSphereStdDev      = fDivCustom( totSphereStdDev, totSphereWeight, 0);
  
  
  float returnValue = (useHemispheres) ? zWeighted : zComplete;
  
  
  
  //## PRINT SUMMARY OF RESULTS TO COUT
  
  cout << endl << "Z SCALE ESTIMATE RESULTS:"               << endl;
  cout                                                      << endl;
  cout << "crude Z scale estimate:     " << zCrude          << endl;
  cout << "complete Z scale estimate:  " << zComplete        << endl;
  cout << "truncated Z scale estimate: " << zTruncated      << endl;
  cout << "sphere Z scale estimate:    " << zSpheres          << endl;
  cout                                                      << endl;
  cout << "current Z scale:            " << currZScale      << endl;
  cout << "weighted Z scale estimate:  " << zWeighted        << endl;
  cout                                                      << endl;
  cout << "total X span: " << totalSpanInX                  << endl;
  cout << "total Y span: " << totalSpanInY                  << endl;
  cout << "total Z span: " << totalSpanInZ                  << endl;
  cout                                                      << endl;
  cout << "# surfaces:            " << nSurfaces            << endl;
  cout << "#  round enough surfs:  " << nRoundEnough << " (" <<  calcPercent(nRoundEnough,nSurfaces) << "%)"<< endl;
  cout << "#  complete surfaces:   " << nComplete    << " (" <<  calcPercent(nComplete,nSurfaces) << "%)" << endl;
  cout << "#  hemisphere surfaces: " << nTrncHalf    << " (" <<  calcPercent(nTrncHalf,nSurfaces) << "%)"<< endl;
  cout << "#  truncated fragments: " << nTrncFrag    << " (" <<  calcPercent(nTrncFrag,nSurfaces) << "%)"<< endl;
  cout << "#  double truncated:    " << nDoubleTrunc << " (" <<  calcPercent(nDoubleTrunc,nSurfaces) << "%)"<< endl;
  cout << "#  out-of-bounds:       " << nOutOfBounds << " (" <<  calcPercent(nOutOfBounds,nSurfaces) << "%)"<< endl;
  
  string modelFileName = "FILE_NAME ?";   // mod.filePath
  
  
  
  //## PRINT MORE DETAILED RESULTS TO CSV FILE
  
  if( appendToCSV )
  {
    QString qname = QFileDialog::getSaveFileName(plug.window,"Append Info To File","","CSV file (*.csv)");
    if ( qname==NULL )
      return (returnValue);
    string savePath = qStringToString(qname);
    
    ofstream outfile;
    outfile.open(savePath.c_str(), std::ios::app);      // open for appending data
    
    if( outfile.fail() )      // if cannot open or write to file:
    {
      cerr << "ERROR: Could not save csv file" << endl;
    }
    else
    {
      outfile << endl << "Z SCALE ESTIMATE RESULTS:"                  << endl;
      outfile                                                         << endl;
      outfile << "file:  ,,," << modelFileName                        << endl;
      outfile << "time:  ,,," << time_getCurrTimeStampString()        << endl;
      outfile << "objIdx:,,," << objIdx                               << endl;
      outfile                                                         << endl;
      outfile << "crude Z scale estimate:    ,,," << zCrude           << endl;
      outfile << "complete Z scale estimate: ,,," << zComplete        << endl;
      outfile << "truncated Z scale estimate:,,," << zTruncated       << endl;
      outfile << "sphere Z scale estimate:   ,,," << zSpheres          << endl;
      outfile << "current Z scale:           ,,," << currZScale       << endl;
      outfile                                                         << endl;
      outfile << "total X span:,,,"           << totalSpanInX         << endl;
      outfile << "total Y span:,,,"           << totalSpanInY         << endl;
      outfile << "total Z span:,,,"           << totalSpanInZ         << endl;
      outfile                                                         << endl;
      outfile << "# surfaces:            ,,," << nSurfaces            << endl;
      outfile << "# round enough surfs:  ,,," << nRoundEnough  << ",(" << calcPercent(nRoundEnough,nSurfaces)  << "%)"<< endl;
      outfile << "# complete surfaces:   ,,," << nComplete    << ",(" <<  calcPercent(nComplete,nSurfaces)    << "%)"<< endl;
      outfile << "# hemisphere surfaces: ,,," << nTrncHalf    << ",(" <<  calcPercent(nTrncHalf,nSurfaces)    << "%)"<< endl;
      outfile                                  << endl;
      
      outfile  << "FILE_NAME,timescale,,"
          << "modelZScale,currZScale,,"
          << "zCrude,zComplete,zTruncated,zSpheres,zWeighted,,"
          << "totalSpanInX,totalSpanInY,totalSpanInZ,,"
          << "nSurfaces,nRoundEnough,nAccepted,nTruncated,,"
          << "nComplete,nTrncHalf,nTrncFrag,nDoubleTrunc,nOutOfBounds,nMissing,,"
          << "nSpheresCounted,totSphereWeight,avgSphereMeanRadius,avgSphereMaxDev,avgSphereAvgResid,avgSphereStdDev,,"
          << "TRUNCATED_WEIGHT,calcSpheres,maxDeviation,startChangeZ,accuracyZScale,maxItsCenter,maxZChange,,"
          << endl;
          
      outfile  << modelFileName << "," << time_getCurrTimeStampString() << ", ,"
          << modelZScale << "," << currZScale << ",,"
          << zCrude << "," << zComplete << "," << zTruncated << "," << zSpheres << "," << zWeighted << ",,"
          << totalSpanInX << "," << totalSpanInY << "," << totalSpanInZ << ",,"
          << nSurfaces << "," << nRoundEnough << "," << nAccepted << "," << nTruncated << ",,"
          << nComplete << "," << nTrncHalf << "," << nTrncFrag << "," << nDoubleTrunc << "," << nOutOfBounds << "," << nMissing << ",,"
          << nSpheresCounted << "," << totSphereWeight << "," << avgSphereMeanRadius << "," << avgSphereMaxDev << "," << avgSphereAvgResid << "," << avgSphereStdDev << ",,"
          << TRUNCATED_WEIGHT << "," << calcSpheres << "," << maxDeviation << "," << startChangeZ << "," << accuracyZScale << "," << maxItsCenter << "," << maxZChange
          << endl;
          
      outfile << endl << endl;
        
      outfile << "surface idx,accepted,truncated,classification,,zScaleEst,,"
          << "sphereZScale,sphereMeanRadius,sphereFractInZ,sphereMaxDev,sphereAvgResid,sphereStdDev,,"
          << "spanX,spanY,avgXY,spanZEst,,"
          << "slicesSpanned,middleSliceZ,contLargestZ,numConts,,"
          << "minX,maxX,minY,maxY,minZ,maxZ" << endl;
          
      for(int s=0; s<(int)surf.size(); s++)
      {
        if( printOnlyAccepted && !surf[s].accepted )
          continue;
        outfile << s << "," << surf[s].accepted << "," << surf[s].truncated << "," << surf[s].classification << ",," << surf[s].zScaleEst << ",,"
            << surf[s].sphereZScale << "," << surf[s].sphereRad << "," << surf[s].sphereFractInZ << "," << surf[s].sphereMaxDev << "," << surf[s].sphereAvgResid << "," << surf[s].sphereStdDev << ",,"
            << surf[s].spanX << "," << surf[s].spanY << "," << surf[s].avgXY << "," << surf[s].spanZEst << ",,"
            << surf[s].slicesSpanned << "," << surf[s].middleSliceZ << "," << surf[s].contLargestZ << "," << (int)surf[s].co.size() << ",,"
            << surf[s].mbrLL.x << "," << surf[s].mbrUR.x << "," << surf[s].mbrLL.y << "," << surf[s].mbrUR.y << "," << surf[s].mbrLL.z << "," << surf[s].mbrUR.z << "," << endl;
      }
      
      outfile.close();
      
      cout << "... output saved to: " << savePath << endl;
    }
  }
  if( summaryCSV )
  {
    QString qname = QFileDialog::getSaveFileName(plug.window,"Append SUMMARY Info To File","","CSV file (*.csv)");
    if ( qname==NULL )
      return (returnValue);
    string savePathS = qStringToString(qname);
    
    //## CHECK IF SUMMARY FILE IS EMPTY OR ALREADY HAS HEADER:
    string line;
    ifstream infile( savePathS.c_str() );
    bool fileIsEmpty = ( infile.fail() || !getline(infile, line) );
    infile.close();
    
    //## OPEN SUMMARY FILE:
    ofstream outfileS;
    outfileS.open(savePathS.c_str(), std::ios::app);      // open for appending data
    
    if(fileIsEmpty)    // if file is empty: add header
    {
      outfileS<< "FILE_NAME,timescale,,"
          << "modelZScale,currZScale,,"
          << "zCrude,zComplete,zTruncated,zSpheres,zWeighted,,"
          << "totalSpanInX,totalSpanInY,totalSpanInZ,,"
          << "nSurfaces,nRoundEnough,nAccepted,nTruncated,,"
          << "nComplete,nTrncHalf,nTrncFrag,nDoubleTrunc,nOutOfBounds,nMissing,,"
          << "nSpheresCounted,totSphereWeight,avgSphereMeanRadius,avgSphereMaxDev,avgSphereAvgResid,avgSphereStdDev,,"
          << "TRUNCATED_WEIGHT,calcSpheres,maxDeviation,startChangeZ,accuracyZScale,maxItsCenter,maxZChange,,"
          << endl;
    }
    //## WRITE SINGLE LINE SUMMARISING RESULTS
      
      outfileS<< modelFileName << "," << time_getCurrTimeStampString() << ", ,"
          << modelZScale << "," << currZScale << ",,"
          << zCrude << "," << zComplete << "," << zTruncated << "," << zSpheres << "," << zWeighted << ",,"
          << totalSpanInX << "," << totalSpanInY << "," << totalSpanInZ << ",,"
          << nSurfaces << "," << nRoundEnough << "," << nAccepted << "," << nTruncated << ",,"
          << nComplete << "," << nTrncHalf << "," << nTrncFrag << "," << nDoubleTrunc << "," << nOutOfBounds << "," << nMissing << ",,"
          << nSpheresCounted << "," << totSphereWeight << "," << avgSphereMeanRadius << "," << avgSphereMaxDev << "," << avgSphereAvgResid << "," << avgSphereStdDev << ",,"
          << TRUNCATED_WEIGHT << "," << calcSpheres << "," << maxDeviation << "," << startChangeZ << "," << accuracyZScale << "," << maxItsCenter << "," << maxZChange
          << endl;
          
    outfileS.close();
    cout << "... summary saved to: " << savePathS << endl;
  }
  
  
  
  
  
  
  
  
  //## IF DESIRED: CHANGE SURF NUMBERS TO REPRESENT SURFACE CLASSIFICATION
  if( showSurfClassif ) 
  {
    cout << "MAKING NEW OBJECTS" << endl;       //%%%%%%%%
    
    if( imodNewObject(imod) ) return (0);
    Iobj *objMissing = getObj( imod, osize(imod)-1 );
    imodObjectSetName (objMissing,"missing");
    imodObjectSetColor(objMissing, 1.0, 0.0, 1.0);     // cyan         #2
    
    if( imodNewObject(imod) ) return (0);
    Iobj *objOutOfBounds = getObj( imod, osize(imod)-1 );
    imodObjectSetName (objOutOfBounds,"out-of-bounds");
    imodObjectSetColor(objOutOfBounds, 1.0, 0.0, 0.0);     // red         #3
    
    if( imodNewObject(imod) ) return (0);
    Iobj *objDoubleTrun = getObj( imod, osize(imod)-1 );
    imodObjectSetName (objDoubleTrun,"double_trunc");
    imodObjectSetColor(objDoubleTrun, 1.0, 1.0, 0.0);     // yellow         #4
    
    if( imodNewObject(imod) ) return (0);
    Iobj *objTrunFrag = getObj( imod, osize(imod)-1 );
    imodObjectSetName (objTrunFrag,"truncated_frag");
    imodObjectSetColor(objTrunFrag, 1.0, 0.5, 0.0);     // orange         #5
    
    if( imodNewObject(imod) ) return (0);
    Iobj *objTrunHalf = getObj( imod, osize(imod)-1 );
    imodObjectSetName (objTrunHalf,"truncated_half");
    imodObjectSetColor(objTrunHalf, 0.0, 0.0, 0.5);     // dark_blue         #6
    
    if( imodNewObject(imod) ) return (0);
    Iobj *objTrunHalfA = getObj( imod, osize(imod)-1 );
    imodObjectSetName (objTrunHalfA,"truncated_half_accepted");
    imodObjectSetColor(objTrunHalfA, 0.0, 0.0, 0.9);     // blue         #7
    
    if( imodNewObject(imod) ) return (0);
    Iobj *objComplete = getObj( imod, osize(imod)-1 );
    imodObjectSetName (objComplete,"complete");
    imodObjectSetColor(objComplete, 0.0, 1.0, 0.0);     // dark_green         #8
    
    if( imodNewObject(imod) ) return (0);
    Iobj *objCompleteA = getObj( imod, osize(imod)-1 );
    imodObjectSetName (objCompleteA,"complete_accepted");
    imodObjectSetColor(objCompleteA, 0.0, 1.0, 0.0);     // green         #9
    
    //Iobj *objDest = objMissing;
    
    for(int s=0; s<(int)surf.size(); s++)
    {
      bool accepted = surf[s].accepted==true && surf[s].roundEnough==true;
      
      string clfn = surf[s].classification;
      
      int colorVal = 0;
      
      if      (clfn == CL_MISSING)                  colorVal=2;    // cyan
      else if (clfn == CL_OUTOFBOUNDS)              colorVal=3;    // red  
      else if (clfn == CL_DOUBLETRUNC)              colorVal=4;    // yellow
      else if (clfn == CL_TRUNCFRAG)                colorVal=5;    // magenta    
      else if (clfn == CL_TRUNCHALF && !accepted)   colorVal=6;    // blue
      else if (clfn == CL_TRUNCHALF && accepted)    colorVal=7;    // blue
      else if (clfn == CL_COMPLETE && !accepted)    colorVal=8;    // green
      else if (clfn == CL_COMPLETE && accepted)     colorVal=9;    // green
      
      if(colorVal == 0)
        continue;
      
      //int colorVal = 0;
      /*
       if      (clfn == CL_MISSING)      objDest = objMissing; //colorVal=5;    // cyan
       else if (clfn == CL_OUTOFBOUNDS)  objDest = objOutOfBounds; //colorVal=1;    // red  
       else if (clfn == CL_DOUBLETRUNC)  objDest = objDoubleTrun; //colorVal=3;    // yellow
       else if (clfn == CL_TRUNCFRAG)    objDest = objTrunFrag; //colorVal=5;    // magenta    
       else if (clfn == CL_TRUNCHALF)    objDest = objTrunHalf; //colorVal=0;    // blue
       else if (clfn == CL_COMPLETE)     objDest = objComplete; //colorVal=2;    // green
       */
      
      //int classification = (int)surf[s].classification;
      
      for( int i=0; i<(int)surf[s].co.size(); i++ )
      {
        int contIdx = surf[s].co[i].idx;
        cout << "contIdx=" << contIdx << endl;
        
        Iobj *objThis   = getObj( imod, objIdx );
        
        if( contIdx >= csize(objThis)  )
        {
          cerr << "PROBLEM" << endl;
          continue;
        }
        
        Icont *cont = getCont( objThis, contIdx );
        imodContourSetSurface( cont, colorVal );
        
        /*
         if (clfn == CL_MISSING) imodContourSetSurface( cont, 1);
         if (clfn == CL_OUTOFBOUNDS) imodContourSetSurface( cont, 2);
         if (clfn == CL_DOUBLETRUNC) imodContourSetSurface( cont, 3);
         if (clfn == CL_TRUNCFRAG) imodContourSetSurface( cont, 4);
         if      (clfn == CL_TRUNCHALF) imodContourSetSurface( cont, 5);
         if      (clfn == CL_COMPLETE) imodContourSetSurface( cont, 6);
         
         if      (clfn == CL_TRUNCHALF && accepted) imodContourSetSurface( cont, 7);
         if      (clfn == CL_COMPLETE  && accepted)  imodContourSetSurface( cont, 8);
         */
        
        /*
         switch( classification )
         {
           case( CL_MISSING ):     edit_addContourToObj( objMissing, cont, false );   break;
           case( CL_OUTOFBOUNDS ): edit_addContourToObj( objOutOfBounds, cont, false );   break;
           case( CL_DOUBLETRUNC ): edit_addContourToObj( objDoubleTrun, cont, false );   break;
           case( CL_TRUNCFRAG ):   edit_addContourToObj( objTrunFrag, cont, false );   break;
           case( CL_TRUNCHALF ):   edit_addContourToObj( objTrunHalf, cont, false );   break;
           case( CL_COMPLETE ):    edit_addContourToObj( objComplete, cont, false );   break;
         }
         */
        
        /*
         if(surf[s].classification == CL_COMPLETE)
         {
           cout << "....COMPLETE!" << endl;
           //edit_addContourToObj( objComplete, cont, false );
           Icont *newCont = imodContourNew();
           //imodContourDup( cont );    // malloc new contour and don't delele it
           cont_copyPts( cont, newCont, true );
           imodObjectAddContour( objComplete, newCont );
           free(newCont);
         }*/
        
        
      }
      
      //for(int c=0; c<(int)surf[s].co.size(); c++)
      //  obj.cont[ surf[s].co[c].idx ].surfNum = colorVal;
    }
    
    
    //for(int s=0; s<(int)surf.size(); s++)
    //  if( surf[s].accepted == false )
    //    for(int c=0; c<(int)surf[s].co.size(); c++)
    //      obj.cont[ surf[s].co[c].idx ].surfNum = -2;
    
    MsgBox("NOTE: You must move surface #2 to object 'missing' and so on");
  }
  
  
  /*
  if( showCenters )
  {
    demoConts.clear();
    Icont contOnePt;
    for(int s=0; s<(int)surf.size(); s++)
    {
      if( !surf[s].roundEnough )
        continue;
        
      contOnePt.makeCont( surf[s].mbrCenter, surf[s].sphere );
      contOnePt.sortVal = surf[s].sphereZScale;
      demoConts.push_back( contOnePt );
    }
  }*/
  
  return (returnValue);
}





// contains information regenerated for each interpolation event and used
// in determining connected contours etc.


struct ContInfo
{
  int z;
  Ipoint ll;            // lower left  point of the contour's minimum bounding box (mbr)
  Ipoint ur;            // upper right point of the contour's minimum bounding box
  Ipoint center;      // point in the center of the MBR
  
  //int checked;          // can be used to see which contour is checked
  int numPts;
  int surfNum;
};


//------------------------
//-- Sorts contours into surfaces (by changing their "surfNum" value) and returns the number of surfaces

int analysis_sortContsIntoSurfaces( Iobj *obj, int zSpan )
{
  if( !obj || csize(obj)<=5 )
  {
    MsgBox("You must select an object with many contours");
    return 0;
  }
  
  //## CREATE ARRAY TO STORE CONTOUR INFORMATION (BOUNDING BOX AND SURFACE NUMBER):
  
  vector<ContInfo> cinfo;
  cinfo.resize( csize(obj) );
  
  for(int c=0; c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj, c);

    cinfo[c].z   = getZInt(cont);
    cont_getMBR( cont, &cinfo[c].ll, &cinfo[c].ur );
    cinfo[c].center = line_getPtHalfwayBetween( &cinfo[c].ll, &cinfo[c].ur );
    //cinfo[c].checked = false;
    cinfo[c].numPts = psize(cont);
    cinfo[c].surfNum = -1;
  }
  
  
  //vector<int> contChecked;           // stores a idx and float for each contour after
  //                                      // "sort contours" is run
  
  //contChecked.resize( csize(obj) );
  
  
  /*
  vector<ContInfo> 
  
  
	//## RESET SURFACE NUMBER VALUES FOR ALL CONTOURS:
  
  for (int c=0; i<csize(obj); i++)
  {
    Icont *cont = getCont(obj, cont);
    
    imodContourSetSurface( cont, -1 ); 
    contChecked.[c] = 0;				// is set to 1 after contour has been checked / searched
  }
		
  //## MAKE MBRS AROUND EVERY CONTOUR:
  
  vector<ContInfo> cmbr;
  cmbr.resize( csize() );
  for (int i=0; i<csize(); i++)
    cmbr[i] = cont_getMBRAroundContour( cont[i] );
  */
  
  
  
  //## ITERATE THROUGH CONTOURS, FINDING SETS OF MATCHING CONTOURS TO GROUP INTO AN SURFACE
  
  cout << endl << "SORTING CONTOURS INTO SURFACES" << endl << endl;
  
  int numSurfaces = 0;
  int numIsolatedConts = 0;
  
  for (int i=0; i<csize(obj); i++)		// for each cotour:
  {
    if( cinfo[i].surfNum != -1 )			// if contour has already been labelled in a surface: skip it
      continue;
    if( cinfo[i].numPts == 0 )				// if contour is empty: skip it
      continue;
    
    vector<int> idxsInSurf;				// records contours found to be in the same surface
    idxsInSurf.push_back( i );		// add this "base contour" to the list
    
    for(int m=0; m<(int)idxsInSurf.size(); m++)		// for each maching contour found: search and add adjacent matching contours
    {
      int    mIdx = idxsInSurf[m];    // |-- the new contour to search for matches on	
      int    z    = cinfo[mIdx].z;		// |
      
      for(int j=i; j<csize(obj); j++)					// for each contour in the object:
      {
        Icont *contJ    = getCont(obj,j);
        
        if( cinfo[j].surfNum != -1 )			// if contour has already been labelled: skip
          continue;
        if( cinfo[j].numPts == 0 )				// if contour is empty: skip it
          continue;
        
        bool contsAdjacentAndOverlap =
          ( cinfo[j].z != z+1 && cinfo[j].z != z-1 ) &&
          ( mbr_isPtInsideBBox2D( &cinfo[j].center, &cinfo[mIdx].ll, &cinfo[mIdx].ur )
           || mbr_isPtInsideBBox2D( &cinfo[mIdx].center, &cinfo[j].ll, &cinfo[j].ur ) );	
        
        if(contsAdjacentAndOverlap)     // if contours are ajacent and overlap:
        {
          cinfo[j].surfNum = numSurfaces;		// label the contour with it's surface number
          idxsInSurf.push_back( j );				// add this to the indexes in the surface
        }
      }
    }
    
    
    if( (int)idxsInSurf.size() == 1 )		// if no matching contours were found for the starting contour
    {
      cinfo[i].surfNum = -2;            // label the contour with -2 to show that it is isolated
      numIsolatedConts++;               // increment the number of isolated contours
      cout << "Isolated contour at:          " << i+1 << endl;	//%%%%%%%
    }
    else									// else (if maches were found):
    {
      cinfo[i].surfNum = numSurfaces;		// label the contour with it's surface number
      numSurfaces++;                    // increment the number of contours found
      cout << "Surf " << numSurfaces << ": " << (int)idxsInSurf.size() << " conts, base " << i+1 << endl;		//%%%%%%
    }
  }
  
  
  //## CHANGE SURFACE NUMBER FOR EACH CONTOUR:
  
  for(int i=0; i<csize(obj) && i<(int)cinfo.size(); i++ )
  {
    imodContourSetSurface( getCont(obj,i), cinfo[i].surfNum+1 );      // ?????????? + 1
  }
  
  
  //## PRINT SUMMARY OF FINDINGS:
  
  float avContsPerSurf = fDiv( (csize(obj)-numIsolatedConts), numSurfaces );
  
  cout << endl << "SUMMARY OF SURFACES" << endl << endl;
  cout << " # surfaces = " << numSurfaces << endl;
  cout << " # isolated contours = " << numIsolatedConts << endl;
  cout << " avg conts per surface = ~" <<  avContsPerSurf << endl;
  
  return (numSurfaces);
}








//------------------------
//-- Outputs the number of concave points for each contour
//-- over the user-specified range of objects

string analysis_outputConcavePtsAnalysis( int minObj, int maxObj, bool printAllConts )
{  
  Imod *imod = ivwGetModel(plug.view);
  
  //## COUNT NUMBER OF CONCAVE POINTS FOR EACH CONTOUR:
  
  ostringstream out;
  
  out << endl;
  out << ">--------------------" <<endl;
  out << endl << "ASSESSMENT OF CONTOUR COMPLEXITY:" <<endl<<endl;
  
  out << endl << "obj,cont,pts,concave_pts,fract_concave,"
                  "length,convex_length,convex_hull_length,"
                  "fract_concave_length,compactness,"
                  "area,convex_hull_area,fract_concave_area" << endl;
  
  for (int o=MAX(minObj,0); o<MIN(maxObj+1,osize(imod)); o++ )   // for each object
  {
    Iobj *obj = getObj(imod,o);
    
    int totPts             = 0;
    int totConcavePts      = 0;
    float totLength        = 0;
    float totConvexLength  = 0;
    float totHullArea      = 0;
    float totConcaveArea   = 0;
    
    out << endl << "OBJECT " << o+1 << ": " << imodObjectGetName(obj) << endl;
    
    for (int c=0; c<csize(obj); c++)                     // for each contour
    {
      Icont *cont = getCont(obj,c);
      
      bool closed = isContClosed(obj,cont);
      
      int   convexPts;
      float convexLength, hullLength, hullArea;
      
      cont_calcConvexProperties( cont, closed, &convexPts,
                                 &convexLength, &hullLength, &hullArea );
      
      int   pts                = psize(cont);
      float length             = imodContourLength( cont, closed );
      float area               = imodContourArea( cont );
      int   concavePts         = pts - convexPts;
      float concaveArea        = hullArea - area;
      float fractConcavePts    = (float)concavePts / (float)pts;
      float concaveLength      = length - convexLength;
      float fractConcaveLength = fDiv( concaveLength, length );
      float compactness        = fDiv( length*length, area );  
      float fractConcaveArea   = MAX( 0.0f, fDiv( concaveArea, area ) );
      
      if(printAllConts)
        out << o+1 << "," << c+1 << ","
          << pts << "," << concavePts << "," << fractConcavePts << ","
          << length << "," << convexLength << "," << hullLength << ","
          << fractConcaveLength << "," << compactness << ","
          << area << "," << hullArea << "," << fractConcaveArea << endl;
      
      totPts           += pts;
      totConcavePts    += concavePts;
      totLength        += length;
      totConvexLength  += convexLength;
      totHullArea      += area;
      totConcaveArea   += concaveArea;
    }
    
    if( totPts > 0 )
    {
      float totPercentConcavePts  = calcPercent(totConcavePts,totPts);
      float totPercentConcaveLen  = calcPercent(totLength-totConvexLength,totLength);
      float totPercentConcaveArea = calcPercent(totConcaveArea,totHullArea);
      
      totPercentConcavePts  = roundPrec( totPercentConcavePts,  0.01 );
      totPercentConcaveLen  = roundPrec( totPercentConcaveLen,  0.01 );
      totPercentConcaveArea = roundPrec( totPercentConcaveArea, 0.01 );
      
      out << endl;
      out << "TOTAL POINTS:         \t" << totPts << endl;
      out << "TOTAL CONCAVE PTS:    \t" << totConcavePts << endl;
      out << "FRACTION CONCAVE PTS:    \t" << totPercentConcavePts << "%"<< endl;
      out << "FRACTION CONCAVE LENGTH: \t" << totPercentConcaveLen << "%"<< endl;
      out << "FRACTION CONCAVE AREA  : \t" << totPercentConcaveArea << "%"<< endl<< endl;
    }
    else
    {
      out << "EMPTY" << endl;
    }
  }
  out << "TESTS FINISHED" << endl;  
  
  return out.str();
}


//------------------------
//-- Outputs information regarding the length, volume and surface area of
//-- variable width tubes. Be warned that volume and surface area are
//-- only approximate.
//-- 
//-- NOTE: Such tubes can be rendered with:
//--       imodmesh -o 1 -t 1 -d -1 model_file.mop

string analysis_outputTubeSizeAnalysis( int minObj, int maxObj, bool printAllConts,
                                        bool convertUnits )
{  
  Imod *imod = ivwGetModel(plug.view);
  float pixelSize = imodGetPixelSize(imod);
  float zScale    = imodGetZScale(imod);
  float scaleValues = (convertUnits) ? pixelSize : 1;
  
  
  //## WRITE OUT TUBE INFORMATION:
  
  ostringstream out;
  
  out << endl;
  out << ">--------------------" <<endl;
  out << "TUBE INFORMATION:" << endl<<endl;
  out << ",PIXEL SIZE: " << pixelSize << endl;
  out << ",Z-SPACING : " << zScale << endl<<endl;
  out << "obj#,cont#,pts,,openLength,fullLength,,"
    << "avgRadius,firstRadius,lastRadius,minRadius,maxRadius,minMidRadius,,"
    << "openVol,fullVol,surfaceArea"<<endl<<endl;
  
  for (int o=MAX(minObj,0); o<MIN(maxObj+1,osize(imod)); o++ )   // for each object
  {
    Iobj *obj = getObj(imod,o);
    int numConts = csize(obj);
    
    out << ">----------------------"<<endl;
    out << "OBJECT " << o+1 << " - " << imodObjectGetName(obj) << endl<<endl;
    out << "CONTOURS: " << numConts << endl <<endl;
    if(numConts==0)
      continue;
    
    float M = FLOAT_MAX;
    float openL = 0,    OopenL = 0,     openLMin=M,     openLMax=0;
    float fullL = 0,    OfullL = 0,     fullLMin=M,     fullLMax=0;
    float avgR = 0,     OavgR = 0,      avgRMin=M,      avgRMax=0;
    float firstR = 0,   OfirstR = 0,    firstRMin=M,    firstRMax=0;
    float lastR = 0,    OlastR = 0,     lastRMin=M,     lastRMax=0;
    float minR = 0,     OminR = 0,      minRMin=M,      minRMax=0;
    float maxR = 0,     OmaxR = 0,      maxRMin=M,      maxRMax=0;
    float minMidR = 0,  OminMidR = 0,   minMidRMin=M,   minMidRMax=0;
    float openVol = 0,  OopenVol = 0,   openVolMin=M,   openVolMax=0;
    float fullVol = 0,  OfullVol = 0,   fullVolMin=M,   fullVolMax=0;
    float surfaceA = 0, OsurfaceA = 0,  surfaceAMin=M,  surfaceAMax=0;
    long OnumPts=0;
    long numPtsMin=LONG_MAX, numPtsMax=0;
    
    for (int c=0; c<numConts; c++) // for each contour:
    {
      cont_calcPtsizeInfo( obj, getCont(obj,c), zScale, scaleValues, openL,fullL,
                           avgR,firstR,lastR,minR,maxR,minMidR,
                           openVol,fullVol,surfaceA );
      
      OopenL   += openL;
      OfullL   += fullL;
      OavgR    += avgR;
      OfirstR  += firstR;
      OlastR   += lastR;
      OminR    += minR;
      OmaxR    += maxR;
      OminMidR += minMidR;
      OopenVol += openVol;
      OfullVol += fullVol;
      OsurfaceA += surfaceA;
      long numPts = psize( getCont(obj,c) );
      
      OnumPts += numPts;
      updateMin( numPtsMin, numPts );
      updateMax( numPtsMax, numPts );
      
      updateMin( openLMin, openL );
      updateMin( fullLMin, fullL );
      updateMin( avgRMin, avgR );
      updateMin( firstRMin, firstR );
      updateMin( lastRMin, lastR );
      updateMin( minRMin, minR );
      updateMin( maxRMin, maxR );
      updateMin( minMidRMin, minMidR );
      updateMin( openVolMin, openVol );
      updateMin( fullVolMin, fullVol );
      updateMin( surfaceAMin, surfaceA );
      
      updateMax( openLMax, openL );
      updateMax( fullLMax, fullL );
      updateMax( avgRMax, avgR );
      updateMax( firstRMax, firstR );
      updateMax( lastRMax, lastR );
      updateMax( minRMax, minR );
      updateMax( maxRMax, maxR );
      updateMax( minMidRMax, minMidR );
      updateMax( openVolMax, openVol );
      updateMax( fullVolMax, fullVol );
      updateMax( surfaceAMax, surfaceA );  
      
      out << o+1 <<","<< c+1 <<","<< numPts <<",,"<< openL <<","<< fullL <<",,"
        <<avgR<<","<< firstR <<","<< lastR <<","<<minR<<","<<maxR<<","<<minMidR<<",,"
        <<openVol<<","<<fullVol<<","<<surfaceA<<endl;
      
    }
    
    float div = numConts;
    out << "" << endl;
    out << "TOTAL:," << numConts <<","<< OnumPts <<",,"<< OopenL <<","<< OfullL <<",,"
      <<OavgR<<","<<OfirstR<<","<<OlastR<<","<<OminR<<","<<OmaxR<<","<<OminMidR<<",,"
      << OopenVol <<","<< OfullVol <<","<< OsurfaceA <<endl;
    
    out << "AVERAGE:," <<""<<","<<OnumPts/div<<",,"<<OopenL/div<<","<<OfullL/div<<",," 
      <<OavgR/div<<","<<OfirstR/div<<","<<OlastR/div<<","<<OminR/div<<","<<OmaxR/div<<","
      <<OminMidR/div<<",,"<<OopenVol/div<<","<<OfullVol/div<<","<<OsurfaceA/div<< endl;
    out << "" << endl;
    out << "MIN:," << "" <<","<<numPtsMin<< ",," << openLMin <<","<< fullLMin << ",,"
      <<avgRMin<<","<<firstRMin<<","<<lastRMin<<","<<minRMin<<","<<maxRMin<<","
      <<minMidRMin<< ",,"<<openVolMin <<","<< fullVolMin <<","<< surfaceAMin << endl;
    out << "MAX:," << "" <<","<<numPtsMax<< ",," << openLMax <<","<< fullLMax << ",,"
      <<avgRMax<<","<<firstRMax<<","<<lastRMax<<","<<minRMax<<","<<maxRMax<<","
      <<minMidRMax<< ",,"<<openVolMax <<","<< fullVolMax <<","<< surfaceAMax << endl;
  }
  
  return out.str();
}


//------------------------
//-- Finds all contours in "obj" which appear to branch off the given contour "contRoot".
//-- To be classed as a branch, a contour must have it FIRST point within "maxDistBranch"
//-- of any of the points in "contRoot" after it's first point.
//--
//--   S---o----o-----o-----o------E    <-- root contour
//--            S                                   
//--            |                       <-- branch off root contour (S=first point)
//--            o---o----E

vector<int> analysis_findBranches( Iobj *obj, Icont *contRoot, float maxDistBranch,
                                   float zScale )
{
  Ipoint scale;
  scale.x = 1.0f;
  scale.y = 1.0f;
  scale.z = zScale;
  
  vector<int> branches;
  
  for(int c=0; c<csize(obj); c++)             // for each contour in objB:
  {
    Icont *cont = getCont(obj,c);
    if( isEmpty(cont) || (cont==contRoot) )
      continue;
    
    Ipoint *ptStart             = getPt(cont,0);
    bool   isInRangeOfTestCont  = false;
    
    for(int p=0; p<psize(contRoot); p++)           // for each point in contM:
    {
      float dist = imodPoint3DScaleDistance( ptStart, getPt(contRoot,p), &scale );
      if(dist <= maxDistBranch)               // if point is in range:
      {
        branches.push_back(c);
        break;
      }
    }
  }
  
  return branches;
}


//------------------------
//-- Attempts to determine which contours branch off other contours
//-- and outputs the length and average radius of these branches.
//-- You can put "main lengths" and "branches" in two seperate objects,
//-- or all in the same object, but it's important that the start of each
//-- (i.e. first point, NOT the last point) is within the specified
//-- number of pixels from one of the points in the contour it banches off.

struct BranchInfo
{
  int depth;        // depth of this contour, where 0 = root contour
  int contIdx;      // index of this contour within it's object
  int branches;     // number of immediate branches coming off this contour
  
  int branchesAll;  // total number of branches, including sub-branches, off this contour
  float length;     // length of this contour
  float avgR;       // average point size radius along the length of this tube 
  float lengthAll;  // length of this contour plus all its sub-branches
  float avgRAll;    // average radius for this contour and all its sub-branches
  
  BranchInfo( int _depth, int _contIdx )
  {
    depth       = _depth;
    contIdx     = _contIdx;
    branches    = 0;
    branchesAll = 0;
    length      = 0;
    avgR        = 0;
    lengthAll   = 0;
    avgRAll     = 0;
  }
};


//------------------------
//-- Inputs the index of two objects - one with "root contours" (objMainLen)
//-- and another object (objBranches) representing contours which branch off
//-- these root contours. It then searches through all branch contours
//-- and tries to match each with a root.... to be connected the first
//-- point in a branch contour must be within (maxDistBranch) of any point
//-- in a root contour. Finally, it will output which branches connect
//-- to which root contours and show total lengths for each root contour
//-- which includes the length of the root + any branches and, if (addRadiusToLeng)
//-- is true, this length will include the radius of the first and last points
//-- of root contour and the radius of the last point in each branch.

string analysis_outputBranchingAnalysis( int objMainLen, int objBranches,
                                         int  branchDepth, float maxDistBranch,
                                         bool addRadiusToLen, bool convertUnits,
                                         bool showMatches )
{
  Imod *imod = ivwGetModel(plug.view);
  int objIdx, cIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &cIdx, &ptIdx);
  
  float pixelSize = imodGetPixelSize(imod);
  float zScale    = imodGetZScale(imod);
  Ipoint scale;
  scale.x = 1.0;
  scale.y = 1.0;
  scale.z = zScale;
    
  bool  sameObj     = (objMainLen == objBranches);
  float scaleValues = (convertUnits) ? pixelSize : 1;
  
  Iobj *objM = getObj(imod,objMainLen);
  Iobj *objB = getObj(imod,objBranches);
  
  if( csize(objM)==0 || csize(objB)==0 )
  {
    MsgBox( "Specified object must have contours!" );
    return "";
  }
  
  //## CREATE AN ARRAY OF BRANCHES UNDER EACH ROOT CONTOUR:
  
  vector<BranchInfo> branch;     // vector containing three integeter:
                                // branch depth, contour index and num branches.
  
  for (int c=0; c<csize(objM); c++)             // for each root contour: add to array
    if( psize( getCont(objM,c) ) > 1 )
      branch.push_back( BranchInfo(0,c) );
  
  for(int d=0; d<branchDepth; d++)              // for each depth:
  {
    for(int i=(int)branch.size()-1; i>=0; i--)    // for each contour in brach array:
    {
      int currBranchDepth = branch[i].depth;
      if( currBranchDepth == d )                    // if contour is at this depth:
      {
        int contIdx = branch[i].contIdx;
        Icont *cont = (d==0) ? getCont(objM,contIdx) : getCont(objB,contIdx);
        vector<int> contBranches = analysis_findBranches(objB,cont,maxDistBranch,zScale);
        branch[i].branches = (int)contBranches.size();
        for(int b=0; b<contBranches.size(); b++)      // add any branches off beneath it
          branch.insert( branch.begin()+(i+1), BranchInfo(d+1,contBranches[b]) );
      }
    }
  }
  
  
  //## CALCULATE LENGTH AND AVERAGE RADIUS OF ALL CONTOURS IN BRANCH LIST:
  
  for(int i=0; i<(int)branch.size(); i++)  // for each contour in brach array:
  {
    float openL,fullL,avgR,firstR,lastR,minR,maxR,minMidR,openVol,fullVol,surfaceA;
    int contIdx = branch[i].contIdx;
    if( branch[i].depth==0 )
    {
      cont_calcPtsizeInfo(objM, getCont(objM,contIdx), zScale, scaleValues, openL,fullL,
                          avgR,firstR,lastR,minR,maxR,minMidR,openVol,fullVol,surfaceA);
      branch[i].length = (addRadiusToLen) ? (openL + firstR + lastR) : (openL);
      branch[i].avgR   = avgR;
    }
    else
    {
      cont_calcPtsizeInfo(objB, getCont(objB,contIdx), zScale, scaleValues, openL,fullL,
                          avgR,firstR,lastR,minR,maxR,minMidR,openVol,fullVol,surfaceA);
      branch[i].length = (addRadiusToLen) ? (openL + lastR) : (openL);
      branch[i].avgR   = avgR;
    }
  }
  
  
  //## CALCULATE TOTAL NUMBER SUB-BRANCHES, TOTAL LENGTH, AND AVERAGE RADIUS
  //## FOR ROOT CONTOURS:
  
  int totalBranches       = 0;
  float  totalLength      = 0;
  float totalRTimeLength = 0;
  
  for(int i=(int)branch.size()-1; i>=0; i--)  // for each contour in brach array:
  {
    totalLength      += branch[i].length;
    totalRTimeLength += (branch[i].length * branch[i].avgR);
    totalBranches    += 1;
    
    if( branch[i].depth == 0 )
    {
      branch[i].branchesAll = totalBranches - 1;
      branch[i].lengthAll   = totalLength;
      branch[i].avgRAll     = fDiv( totalRTimeLength, totalLength);
      
      totalBranches    = 0;
      totalLength      = 0;
      totalRTimeLength = 0;
    }
  }
  
  
  //## DETERMINE WHICH CONTOURS HAVE BEEN MATCHED AS BRANCHES:
  
  vector<int> branchMatches( csize(objB) );   // tallies which conts in objB are
                                              // matched as branches off objM
  
  for(int i=(int)branch.size()-1; i>=0; i--)  // for each contour in brach array:
    if( branch[i].depth != 0 )
      branchMatches[ branch[i].contIdx ]++;
  
  
  //## COUNT NUMBER OF CONTOURS NOT MATCHED AS BRANCHES:
  
  int numContsNotMatchedAsBranch = 0;
  int numContsMatchedAsMultipleBranches = 0;
  for(int i=0; i<(int)branchMatches.size(); i++)
  {
    if( branchMatches[i]==0 )
      numContsNotMatchedAsBranch++;
    else if( branchMatches[i]>1 )
      numContsMatchedAsMultipleBranches++;
  }
  
  
  //## TALLY NUMBER OF BRANCHES FOR EACH ROOT CONTOUR:
  
  int totBranches     = 0;
  int totBranchesAll  = 0;
  int numRoot         = 0;
  int numRootNoBranch = 0;
  int numRoot1Branch  = 0;
  int numRoot2Branch  = 0;
  int numRoot3Branch  = 0;
  int numRoot4Branch  = 0;
  int numRoot5Branch  = 0;
  int numRootOver5Branch = 0;
  
  for(int i=0; i<(int)branch.size(); i++)  // for each contour in brach array:
  {
    if( branch[i].depth == 0 )
    {
      totBranches    += branch[i].branches;
      totBranchesAll += branch[i].branchesAll;
      numRoot++;
      if      (branch[i].branchesAll==0)    numRootNoBranch++;
      else if (branch[i].branchesAll==1)    numRoot1Branch++;
      else if (branch[i].branchesAll==2)    numRoot2Branch++;
      else if (branch[i].branchesAll==3)    numRoot3Branch++;
      else if (branch[i].branchesAll==4)    numRoot4Branch++;
      else if (branch[i].branchesAll==5)    numRoot5Branch++;
      else if (branch[i].branchesAll>=6)    numRootOver5Branch++;
    }
  }
  
  
  if( numRoot == 0 )
  {
    MsgBox( "ERROR: No valid root contours found. \n"
            "Contours must have >1 point" );
    return "";
  }
  
  
  //## WRITE OUT BRANCH INFORMATION:
  
  ostringstream out;
  
  out << endl;
  out << ">--------------------" << endl;
  out << "BRANCH INFORMATION:" << endl<<endl;
  
  out << endl;
  out << "HIERARCHY OF BRANCHES:" << endl<<endl;
  
  for(int i=0; i<(int)branch.size(); i++)  // for each contour in brach array:
  {
    BranchInfo b = branch[i];
    if(b.depth==0)
      out << endl;
    for( int d=0; d<b.depth; d++ )
      out << "  |";
    out << "_cont " << b.contIdx+1 <<", length="<< b.length << ", rad="<< b.avgR << endl;
  }
  
  out << endl;
  out << "SUMMARY OF ROOT CONTOURS:" << endl;
  out << "cont,length,avgRadius,branches,totBranches,totLength,totAvgRadius" << endl;
  for(int i=0; i<(int)branch.size(); i++)  // for each contour in brach array:
  {
    BranchInfo &b = branch[i];
    if( b.depth != 0 )
      continue;
    out << b.contIdx+1 <<","<< b.length <<","<< b.avgR <<"," << b.branches <<","
      << b.branchesAll <<","<< b.lengthAll<<","<< b.avgRAll <<endl;
  }
  
  
  //## OUTPUT SUMMARY OF RESULTS:
  
  if(showMatches)
  {
    out << endl;
    out << "OBJECT " << objBranches+1 << ":" << endl;
    out << "cont \t# times matched as branch "<< endl;
    
    int contNoBranches;
    for(int i=0; i<(int)branchMatches.size(); i++)
    {
      out << "  " << i+1 << "   \t" << branchMatches[i];
      out << ((branchMatches[i]==1) ? "" : " **") << endl;
    }
    out << endl;
    out << "num conts not matched:    \t" << numContsNotMatchedAsBranch << endl;
    out << "num conts matched > once: \t" << numContsMatchedAsMultipleBranches << endl;
  }
  
  float bPerRoot    = fDiv(totBranches,   numRoot);
  float bAllPerRoot = fDiv(totBranchesAll,numRoot);
  
  int percNoB = calcPercentInt( numRootNoBranch,  numRoot );
  int perc1B  = calcPercentInt( numRoot1Branch,  numRoot );
  int perc2B  = calcPercentInt( numRoot2Branch,  numRoot );
  int perc3B  = calcPercentInt( numRoot3Branch,  numRoot );
  int perc4B  = calcPercentInt( numRoot4Branch,  numRoot );
  int perc5B  = calcPercentInt( numRoot5Branch,  numRoot );
  int perc6B  = calcPercentInt( numRootOver5Branch,  numRoot );
  
  string percNoBStr = (percNoB) ? (" \t(" + toString(percNoB) + "%)") : "";
  string perc1BStr  = (perc1B)  ? (" \t(" + toString(perc1B)  + "%)") : "";
  string perc2BStr  = (perc2B)  ? (" \t(" + toString(perc2B)  + "%)") : "";
  string perc3BStr  = (perc3B)  ? (" \t(" + toString(perc3B)  + "%)") : "";
  string perc4BStr  = (perc4B)  ? (" \t(" + toString(perc4B)  + "%)") : "";
  string perc5BStr  = (perc5B)  ? (" \t(" + toString(perc5B)  + "%)") : "";
  string perc6BStr  = (perc6B)  ? (" \t(" + toString(perc6B)  + "%)") : "";
  
  out << endl;
  out << "OBJECT " << objMainLen+1 << ":" << endl << endl;
  out << " non-empty root contours: \t" << numRoot << endl;
  out << " immediate branches off these: \t" << totBranches
      << "  (" << bPerRoot << " per root)" << endl;
  out << " total branches off these: \t" << totBranchesAll
      << "  (" << bAllPerRoot << " per root) - includes sub-branches" << endl;
  out << endl;
  out << " number of root contours with ..." << endl;
  out << "  no branches: \t" << numRootNoBranch << percNoBStr << endl;
  out << "  1 branch:    \t" << numRoot1Branch  << perc1BStr  << endl;
  out << "  2 branches:  \t" << numRoot2Branch  << perc2BStr  << endl;
  out << "  3 branches:  \t" << numRoot3Branch  << perc3BStr  << endl;
  out << "  4 branches:  \t" << numRoot4Branch  << perc4BStr  << endl;
  out << "  5 branches:  \t" << numRoot5Branch  << perc5BStr  << endl;
  out << "  6+ branches: \t" << numRootOver5Branch << perc5BStr << endl;
  out << endl;

  
  return out.str();
}



//------------------------
//-- 
//-- 
//-- 

string analysis_outputVolumeWithinXAnalysis( int objToAnalyze,
                                             int objInIdx, int objOut1Idx, int objOut2Idx,
                                             float distThres,
                                             int  numRandomPts, 
                                             bool useMbr, bool addPtsNewObj )
{
  Imod *imod = ivwGetModel(plug.view);
  
  float pixelSize = imodGetPixelSize(imod);
  float zScale    = imodGetZScale(imod);
  Ipoint scale;
  scale.x = 1.0;
  scale.y = 1.0;
  scale.z = zScale;
    
  Iobj *obj = getObj(imod, objToAnalyze);  
  float distThresPix = distThres / pixelSize;
  float maxSlices = (distThresPix / zScale);
  
  Iobj *objIn   = (objInIdx<0)   ? NULL : getObj(imod, objInIdx);
  Iobj *objOut1 = (objOut1Idx<0) ? NULL : getObj(imod, objOut1Idx);
  Iobj *objOut2 = (objOut2Idx<0) ? NULL : getObj(imod, objOut2Idx);
  
  
  //## GENERATE MBR AROUND OBJECT AND EXPAND BY DISTANCE THRESHOLD:
  
  Ipoint ll, ur;
  mbr_reset( &ll, &ur );
  
  for( int c=0; c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj,c);
    for( int p=0; p<psize(cont); p++ )
      mbr_addPt(getPt(cont,p), &ll, &ur );
  }
  
  ll.x -= distThresPix;
  ll.y -= distThresPix;
  ll.z -= (distThresPix / zScale);
  
  ur.x += distThresPix;
  ur.y += distThresPix;
  ur.z += (distThresPix / zScale);

  cout << endl;
  cout << ">------------------------------------------------------" << endl;
  cout << "BOX DIMENSIONS:" << endl;
  cout << " x: " << ll.x << " to " << ur.x << endl;
  cout << " y: " << ll.y << " to " << ur.y << endl;
  cout << " z: " << ll.z << " to " << ur.z << endl << endl;
  cout << "                            ";
  
  
  //## RANDOMLY GENERATE POINTS WITHIN RECTANGLE AND TEST IF < THRESHOLD DISTANCE
  //## OF ANY POINT IN THE OBJECT:
  
  Icont *contRandPts = imodContourNew();
  Ipoint randPt;
  
  int numPtsInsideThreshold = 0;
  
  for( int i=0; i<numRandomPts; i++ )
  {
    if( i % 500 == 0 )
    {
      cout << "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b";        // shows progress
      string percentDoneStr = toString(i) + " > "
														+ toString( calcPercentInt(i,numRandomPts) ) + "%"  ;
      char pad = ' ';
      cout << toStringPadNumber( percentDoneStr,15,pad );
      flush(cout);
    }
    
    //## GENERATE RANDOM POINT:
    randPt.x = randDbl( ll.x, ur.x );
    randPt.y = randDbl( ll.y, ur.y ); 
    randPt.z = randDbl( ll.z, ur.z );
    int z = roundToInt( randPt.z );
    
    if( addPtsNewObj )
      imodPointAppend( contRandPts, &randPt );
    
    //## CHECK IF POINT IS WITHIN THRESHOLD DIST OF ANY POINT IN OBJECT:
    
    float closestDist = FLOAT_MAX;
    
    for( int c=0; c<csize(obj); c++ )
    {
      Icont *cont = getCont(obj,c);
      if(    getZ(cont) < (randPt.z - maxSlices)
          || getZ(cont) > (randPt.z + maxSlices) )
        continue;
      
      for( int p=0; p<psize(cont); p++ )
      {
        float dist = imodPoint3DScaleDistance( &randPt, getPt(cont,p), &scale ) * pixelSize;
        closestDist = MIN( closestDist, dist );
      }
      
      if(closestDist < distThres)
        break;
    }
    
    if( closestDist < distThres )
    {
      //## CHECK IF POINT IS INSIDE / OUTSIDE REQUIRED OBJECTS:
      int c = 0;
      if( objIn )
      {
        for( c=0; c<csize(objIn); c++ )
          if( getZ( getCont(objIn,c) ) == z && imodPointInsideCont( getCont(objIn,c) ,&randPt) )
            break;
        if( c == csize(objIn)  )
          continue;
      }
      
      if( objOut1 )
      {
        for( c=0; c<csize(objOut1); c++ )
          if( getZ( getCont(objOut1,c) ) == z && imodPointInsideCont( getCont(objOut1,c) ,&randPt) )
            break;
        if( c < csize(objOut1)  )
          continue;
      }
      
      if( objOut2 )
      {
        for( c=0; c<csize(objOut2); c++ )
          if( getZ( getCont(objOut2,c) ) == z && imodPointInsideCont( getCont(objOut2,c) ,&randPt) )
            break;
        if( c < csize(objOut2)  )
          continue;
      }
      
      //## ADD POINT:
      
      numPtsInsideThreshold++;
      if( addPtsNewObj )
        imodPointSetSize( contRandPts, psize(contRandPts)-1, 15 );
    }
  }
  cout << "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b";        // shows progress
  
  
  if( addPtsNewObj )
  {
    Iobj *objLast = getObj(imod, osize(imod)-1 );
    edit_addContourToObj( objLast, contRandPts, false );
  }
  
  imodContourDelete(contRandPts);
  
  //## OUTPUT RESULTS:
  
  float xDiff = (ur.x - ll.x) * pixelSize;
  float yDiff = (ur.y - ll.y) * pixelSize;
  float zDiff = (ur.z - ll.z) * pixelSize * scale.z;
  double totalVolBox = xDiff * yDiff * zDiff;
  
  float fractPtsInside = fDiv( numPtsInsideThreshold, numRandomPts);
  float estimatedVol   = totalVolBox * fractPtsInside;
  float estimatedVolUm = estimatedVol / (1000.0f*1000.0f*1000.0f);
  
  ostringstream out;
  
  out << ">--------------------" << endl;
  out << "VOLUME WITHIN X DISTANCE INFORMATION:" << endl;
  out << endl;
  out << "Objects: " << endl;
  out << " > objToAnalyze: " << objToAnalyze+1 << endl;
  out << " > objInIdx:     " << objInIdx+1 << endl;
  out << " > objOut1Idx:   " << objOut1Idx+1 << endl;
  out << " > objOut2Idx:   " << objOut2Idx+1 << endl;
  out << endl;
  out << "Volume rectangle: " << endl;
  out << "      " << (int)xDiff << " x " << (int)yDiff << " x " << (int)zDiff << " nm" << endl;
  out << "    = " << totalVolBox << " nm^3" << endl;
  out << endl;
  out << "Number points within " << distThres << " nm: " << endl;
  out << "      " << numPtsInsideThreshold << " out of " << numRandomPts << endl;
  out << "    = " << fractPtsInside*100 << "%" << endl;
  out << endl;
  out << "Extimated volume within " << distThres << " nm: " << endl;
  out << "      " << numPtsInsideThreshold << " out of " << numRandomPts << endl;
  out << "    = " << estimatedVol << " nm ^3" << endl;
  out << "    = " << estimatedVolUm << " um ^3" << endl;
  out << endl;
  
  return out.str();
}










struct SurfPoints
{
  Ipoint mbrLL, mbrUR;    // stores a minimum bounding rectangle around the surface
  Icont *p;
  float minDist;
  
  int surfsCompared;
  Ipoint closestPtThisSurf;
  Ipoint closestPtOtherObj;
  int closestSurfNumOtherObj;  
  
  SurfPoints()  {      //-- Default constructor
    mbr_reset(&mbrLL,&mbrUR);
    minDist = FLOAT_MAX;
    surfsCompared = 0;
    closestSurfNumOtherObj = -1;
    //p = imodContourNew();
  }
};


//------------------------
//-- Finds the closest distances between each surface in (objAIdx) and the nearest
//-- surface in (objBIdx) and returns analysis as comma seperated list.
//-- This function works by finding the closest points,
//-- thus may not necessarily be the absolute closest distance.
//-- Another method to increase processing time: all surfaces in each object
//-- are put into bounding boxes, allowing the algorithm to skip surfaces in object B
//-- which have a bounding box further away from the closest distance already found.
//-- If addPtsNewObj is true, two new objects are created to show closest distances,
//-- although this can be buggy sometimes for some reason.

string analysis_closestDistanceSurfsTwoObjects( int objAIdx, int objBIdx,
																							  bool addPtsNewObj, float maxDistLimit )
{
  Imod *imod = ivwGetModel(plug.view);
  
  float pixelSize = imodGetPixelSize(imod);
  float modelZScale    = imodGetZScale(imod);
  
  Iobj *objA = getObj(imod, objAIdx);  
  Iobj *objB = getObj(imod, objBIdx);
  
  //## INITIALIZE VECTORS OF POINTS REPRESENTING MINIMUM BOUNDING BOXES FOR EACH SURFACE:
  
  int nSurfacesA = imodObjectGetValue( objA, IobjMaxSurface);
  int nSurfacesB = imodObjectGetValue( objB, IobjMaxSurface);
  vector<SurfPoints> surfA;           // |-- stores a list of contours in each surface
  vector<SurfPoints> surfB;           // |   and information about the surface itself
  surfA.resize( nSurfacesA );         //  |-- allow enough room for all surfaces
  surfB.resize( nSurfacesB );         //  | 
  
  
  for(int sA=0; sA<(int)surfA.size(); sA++)
    surfA[sA].p = imodContourNew();
  for(int sB=0; sB<(int)surfB.size(); sB++)
    surfB[sB].p = imodContourNew();
  
  
  //## POPULATE CONTOURS INTO THE SURFACE VECTOR AND MAKE MBR:
  
  for (int c=0; c<csize(objA); c++)     // for each contour: if it has a valid surfNum,
  {																			//  add it in the appropriate surface
    Icont *cont = getCont(objA,c);
    int surfNum = imodContourGetSurface( cont ) - 1;
    if( surfNum >=0 && surfNum < (int)surfA.size()  )
    {
      for( int p=0; p<psize(cont); p++ )
      {
        imodPointAppend( surfA[surfNum].p, getPt(cont,p) );
        mbr_addPt( getPt(cont,p), &surfA[surfNum].mbrLL, &surfA[surfNum].mbrUR );
      }
    }
  }
  
  for (int c=0; c<csize(objB); c++)     // for each contour: if it has a valid surfNum,
  {																			//  add it in the appropriate surface
    Icont *cont = getCont(objB,c);
    int surfNum = imodContourGetSurface( cont ) - 1;
    if( surfNum >=0 && surfNum < (int)surfB.size()  )
    {
      for( int p=0; p<psize(cont); p++ )
      {
        imodPointAppend( surfB[surfNum].p, getPt(cont,p) );
        mbr_addPt( getPt(cont,p), &surfB[surfNum].mbrLL, &surfB[surfNum].mbrUR );
      }
    }
  }
  
  
  //cout << "surf A1 has this many points =" << psize( surfA[1].p ) << endl;   //%%%%%%%%%%%%
  //cout << "surf A1 LL.x =" << surfA[1].mbrLL.x << endl;   //%%%%%%%%%%%%
  //cout << "surf B1 has this many points =" << psize( surfB[1].p ) << endl;   //%%%%%%%%%%%%
  //cout << "surf B1 LL.x =" << surfB[1].mbrLL.x << endl;   //%%%%%%%%%%%%
  //flush(cout);
  
  //## FOR EACH SURFACE IN A: FIND NEAREST POINT IN SURFACE B
  
  int totalMinDistsFound = 0;
  float totalSumMinDists = 0;
  
  cout << endl << "BEGINNING SURFACE COMPARISON....." << endl << endl;
  cout << "                              ";
  
  for(int sA=0; sA<(int)surfA.size(); sA++)
  {
    //## SHOW PROGRESS:
    
    cout << "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b";        // shows progress
    string progressStr = "surf " + toString(sA) + " of " + toString(nSurfacesA)
      + " ("+ toString( calcPercentInt(sA,nSurfacesA) ) + "%)"  ;
    cout << toStringPadNumber( progressStr,30,(char)' ' );
    flush(cout);
    
    //## FIND THE SURFACE IN B WITH THE MBR NEAREST TO SURFACE A'S MBR
    
    float minDistToBBAnySurface = FLOAT_MAX;
    int minDistSurfBIdx = 0;
    
    for(int sB=0; sB<(int)surfB.size(); sB++)
    {
      float distBetweenBB = mbr_distBetweenBBoxes3D( &surfA[sA].mbrLL, &surfA[sA].mbrUR,
                              &surfB[sB].mbrLL, &surfB[sB].mbrUR, modelZScale );
      if( distBetweenBB < minDistToBBAnySurface )
      {
        minDistToBBAnySurface = distBetweenBB;
        minDistSurfBIdx = sB;
      }
    }
    
    //cout << "surf " << sA+1 << " closest BB in objB surf # " << minDistSurfBIdx+1
    //  << " dist = " << minDistToBBAnySurface << endl; 
    
    //## STARTING AT SURFACE IN B WITH CLOSEST MBR, SEARCH FOR CLOSEST POINT IN ANY
    //## SURFACE A TO ANY POINT IN B:
    
    surfA[sA].minDist = maxDistLimit;
    
    for(int s=0; s<(int)surfB.size(); s++)
    {
      int sB = (s+minDistSurfBIdx) % (int)surfB.size();
      float distBetweenBB = mbr_distBetweenBBoxes3D( &surfA[sA].mbrLL, &surfA[sA].mbrUR,
                              &surfB[sB].mbrLL, &surfB[sB].mbrUR, modelZScale );
      
      if( distBetweenBB > surfA[sA].minDist )   // if distance between bounding 
        continue;																//  boxes > min dist: skip it
      
      surfA[sA].surfsCompared++;
      Ipoint minPtHereInA;
      Ipoint minPtHereInB;
      float minDistHere = cont_minDistBetweenContPts3D( surfA[sA].p, surfB[sB].p,
																											  modelZScale,
                                                        &minPtHereInA, &minPtHereInB );
      
      if( minDistHere < surfA[sA].minDist )
      {
        surfA[sA].minDist = minDistHere;
        surfA[sA].closestPtThisSurf = minPtHereInA;
        surfA[sA].closestPtOtherObj = minPtHereInB;
        surfA[sA].closestSurfNumOtherObj = sA;
      }
    }
    
    if( surfA[sA].minDist < maxDistLimit )
    {
      totalMinDistsFound++;
      totalSumMinDists += surfA[sA].minDist;
    }
  }
  
  
  //## AS USER IF THEY WANTS TO GENERATE OUTPUT INTO NEW OBJECTS:
  
  string question = "A total of " + toString(totalMinDistsFound) + " closest distances "
                    "have been found from surfaces in object " + toString(objAIdx) +
                    " to object " + toString(objBIdx) + ".\n\n"
                    "Would you like to output results into a new object\n"
                    "called 'MIN DISTANCES BETWEEN SURFACES'?";
  if( addPtsNewObj && MsgBoxYesNo( plug.window, question) )
  {
    if( imodNewObject(imod) ) return "";
    Iobj *objOutLines = getObj( imod, osize(imod)-1 );
    imodObjectSetName (objOutLines,"MIN DISTANCES BETWEEN SURFACES");
    imodObjectSetColor(objOutLines, 1.0, 0.0, 0.0);     // red
    imodObjectSetValue(objOutLines, IobjFlagClosed, 0);
    
    //if( imodNewObject(imod) ) return "";
    //Iobj *objOutPts = getObj( imod, osize(imod)-1 );
    //imodObjectSetName (objOutPts,"CENTER POINT");
    //imodObjectSetColor(objOutPts, 1.0, 1.0, 0.0);     // yellow
    //imodObjectSetValue(objOutPts, IobjFlagClosed, 0);
    //imodObjectSetValue(objOutPts, IobjFlagClosed, 0);
    
    for(int sA=0; sA<(int)surfA.size(); sA++)
    {
      if( addPtsNewObj )
      {
        Icont *newCont = imodContourNew();
        imodPointAppend( newCont, &surfA[sA].closestPtThisSurf );
        imodPointAppend( newCont, &surfA[sA].closestPtOtherObj );
        edit_addContourToObj( objOutLines, newCont, true );
        imodContourDelete(newCont);
      }
    }
  }
  
  //## TALLY RESULTS:
  
  float minMinDist = FLOAT_MAX;
  float maxMinDist = 0;
  for(int sA=0; sA<(int)surfA.size(); sA++)
  {
    surfA[sA].minDist *= pixelSize;
    minMinDist = MIN( minMinDist, surfA[sA].minDist );
    maxMinDist = MAX( maxMinDist, surfA[sA].minDist );
  }
  float avgMinDist = fDiv( totalSumMinDists, totalMinDistsFound );
  string unitsStr = toString( imodUnits(imod) );
  
  //## OUTPUT RESULTS:
  
  ostringstream out;
  out << endl << endl;
  out << ">--------------------" << endl;
  out << "CLOSEST DISTANCE BETWEEN SURFACES IN OBJECT A TO OBJECT B:" << endl;
  out << endl;
  out << "sur#A,\tsur#B,\tminDist(nm),\tpsize(surfA),surfsCompared,\tBBsurfA" << endl;
  for(int sA=0; sA<(int)surfA.size(); sA++)
    out << sA << ",\t" << surfA[sA].closestSurfNumOtherObj << ",\t"
      << surfA[sA].minDist << ",\t" << psize( surfA[sA].p ) << ",\t"
      << surfA[sA].surfsCompared << ",\t"
      << "(" << surfA[sA].mbrLL.x << "," << surfA[sA].mbrLL.y << "," << surfA[sA].mbrLL.z << ")-"
      << "(" << surfA[sA].mbrUR.x << "," << surfA[sA].mbrUR.y << "," << surfA[sA].mbrUR.z << ")" << endl;
  out << endl;
  out << endl;
  out << "SUMMARY:" << endl;
  out << endl;
  out << "Objects: " << endl;
  out << " > objA: " << objAIdx+1 << "  \t... with " << nSurfacesA << " surfaces" << endl;
  out << " > objB: " << objBIdx+1 << "  \t... with " << nSurfacesB << " surfaces" << endl;
  out << " > max distance limit: " << ((maxDistLimit < FLOAT_MAX) ? toString(maxDistLimit) : "none") << endl;
  out << endl;
  out << "Number distances found: " << totalMinDistsFound << "   (of " << nSurfacesA << ")" << endl;
  out << endl;
  out << "Smallest min dist: " << minMinDist << " " << unitsStr <<endl;
  out << "Largest min dist: "  << maxMinDist << " " << unitsStr <<endl;
  out << "Average min dist: "  << avgMinDist << " " << unitsStr <<endl;
  out << endl;
  
  for(int sA=0; sA<(int)surfA.size(); sA++)
    imodContourDelete( surfA[sA].p );
  for(int sB=0; sB<(int)surfB.size(); sB++)
    imodContourDelete( surfB[sB].p );
  
  return out.str();
}





//----------------------------------------------------------------------------
//
//          DEFORMATION GRID FUNCTIONS:
//
//----------------------------------------------------------------------------






//---------------------------------
//-- Takes an object and extracts a vector of Ivectors from all slices or (if specified) a single slice.
//-- Note that only contours with two points will be added as Ivectors.

vector<Ivector> def_generateVectorsFromRefPairs( Iobj *obj, bool allSlices, int sliceNum )
{
  int numBadVectors = 0;
  
  vector<Ivector> vect;      // list of displacement vectors
  for( int i=0; i<csize(obj); i++ )
  {
    Icont *cont = getCont(obj, i);
    if( psize(cont) != 2 )
    {
      wprint("\a contour %d doesn't have 2 points\n", i+1);
      numBadVectors++;
      continue;
    }
    
    if ( ( allSlices || ( getZInt(cont) == sliceNum) ) ) 
      vect.push_back( Ivector( *getPt(cont,0), *getPt(cont,1) ) );
  }
  
  wprint( "\n%d vectors found... \n", (int)vect.size() );
  
  return vect;
}


//---------------------------------
//-- Generates a deformation grid over all slices based on the vectors (contours with two points)
//-- in the specified object.

bool def_generateDefGrid( int objIdxToUse, int colsX, int rowsY, float powerUsed, bool applyCumulativeTransforms )
{
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = getObj( imod, objIdxToUse );
  
  if ( !obj || csize(obj) == 0 || colsX < 1 || rowsY < 1)
    return false;
  
  //## SET UP DEFORMATION GRID:
  
  vector<Ivector> vect = def_generateVectorsFromRefPairs( obj, true );
  plug.dgrid.setupGrid( colsX, rowsY, plug.xsize, plug.ysize, plug.zsize, powerUsed );
  plug.dgrid.powerUsed = powerUsed;
    
  wprint( "DEFORMATION GRID:\n" );
  wprint( "  rows: %d \n", plug.dgrid.rowsY );
  wprint( "  cols: %d \n", plug.dgrid.colsX );
  wprint( "  power used: %f \n", plug.dgrid.powerUsed );
  wprint( "  enforce straight edges: %f \n", plug.dgrid.enforceStraightEdges );
  
  plug.dgrid.deformEntireGridUsingVectors( vect, true, true, applyCumulativeTransforms );
  
  plug.dgrid.printInfo( false );
  
  return true;
}

//---------------------------------
//-- 
//-- 

string def_outputDefPointsAnalysis( int objIdxToUse, bool convertUnits )
{
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = getObj( imod, objIdxToUse );
  int maxZ = plug.zsize;
  float pixelSize = imodGetPixelSize(imod);
  float scaleValues = (convertUnits) ? pixelSize : 1;
  
  vector<Ivector> vect = def_generateVectorsFromRefPairs( obj, true );
  
  ostringstream out;
  out <<endl<< "REFERENCE POINT DISPLACEMENT SUMMARY:" << endl;
  
  vector< vector<Ivector> > sliceVect;
  sliceVect.resize( maxZ );
  
  for( int i=0; i<(int)vect.size(); i++ )
  {
	  int zVal = vect[i].getZVal();
	  if( zVal < 0 || zVal >= maxZ ) {
      out << "ERROR: deformEntireGridUsingVectors - found vector out of range" << endl;
      continue;
	  }
	  sliceVect[ zVal ].push_back( vect[i] );
  }
  
  //## OUTPUT OFFSETS OF REFERENCE POINTS:
  
  out << "boundary#, bottomSlice, topSlice, numVectors,";
  out << "total displacement (pixels), avg displacement (pixels),,,,LIST_OF_LENGTHS \n";
  
  
  int numSectionBoundaries = 0;
  int totalDefPts = 0;
  float totalOffsetAllDefPts = 0;
  for( int z=0; z<maxZ; z++ ) 
  {
    int nVectors = sliceVect[z].size();
    if( nVectors>0 )
    {
      numSectionBoundaries++;
      out << numSectionBoundaries << "," << z << "," <<  z+1 << "," << nVectors << ",";
      float totalOffsetB = 0;
      for( int i=0; i<sliceVect[z].size(); i++ )
      {
        float offset = imodPointDistance( &sliceVect[z][i].ptS, &sliceVect[z][i].ptE ) * scaleValues;
        totalOffsetB += offset;
        totalOffsetAllDefPts += offset;
        totalDefPts++;
      }
      float avgOffsetB = totalOffsetB / nVectors;
      out << totalOffsetB << "," << avgOffsetB << ",,,,";
      for( int i=0; i<sliceVect[z].size(); i++ )
        out << imodPointDistance(&sliceVect[z][i].ptS,&sliceVect[z][i].ptE)*scaleValues << ",";
      out << endl;
    }
  }
  float avgOffsetAllDefPts = fDiv( totalOffsetAllDefPts, totalDefPts);
  
  out << endl;
  out << "total deformation points:" << totalOffsetAllDefPts << endl;
  out << "avg offset ALL points:" << avgOffsetAllDefPts << endl;
  
  return out.str();
}

//---------------------------------
//-- Takes an object and shifts all the contours and points using the deformation grid

bool def_deformObjectsUsingDefGrid( int objIdx, bool inverse, bool duplicate )
{
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = getObj( imod, objIdx );
  
  if ( !obj )
    return false;
  
  int badPoints = 0;
  
  for (int c=0; c<csize(obj); c++)
  {
    Icont *cont = getCont(obj,c);
    for (int p=0; p<psize(cont); p++ )
      if( plug.dgrid.adjustPointByGrid( getPt(cont,p), inverse, true ) == false)
        badPoints++;
  }
  
  //## OUTPUT A SUMMARY:
  wprint( "Object %d deformed... %d bad points\n", objIdx+1, badPoints);
  return true;
}


//---------------------------------
//-- Creates a deformation grid for a single slice using all the vectors (i.e. contours with two points)
//-- of the specified object, on that slice

bool def_createDefGridForSlice( int objIdxToUse, int colsX, int rowsY, int zVal,
                                    bool displayGrid, bool displayInDiffObj,
                                    int objIdxForDisplay, float powerUsed )
{
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = getObj( imod, objIdxToUse );
  
  if ( !obj || colsX < 1 || rowsY < 1)
    return false;
  
  vector<Ivector> vect = def_generateVectorsFromRefPairs( obj, false, zVal );
  plug.dgrid.setupGrid( colsX, rowsY, plug.xsize, plug.ysize, plug.zsize, powerUsed );
  plug.dgrid.deformBoundarySliceUsingVectors( vect, zVal, 1.0 );
  
  return true;
}







//----------------------------------------------------------------------------
//
//          IMAGE FUNCTIONS:
//
//----------------------------------------------------------------------------





//------------------------
//-- Returns the unsigned char at coordinates (x,y) or plug.data

inline unsigned char &pix(unsigned char *data, int x, int y)
{
  return data[y*plug.xsize + x];
}


//------------------------
//-- Returns 1 if the value of the pixel in "data" at coordinates (x,y) is
//-- equal to "val", or 0 if not equal.
//-- If the given coordinates are not within the image (w x h) then
//-- the value returned will be equal to "outOfBoundsReturnsVal"

inline int isPixEqual( unsigned char val, unsigned char *data, int x, int y,
                       int maxX, int maxY, int outOfBoundsReturnsVal)
{
  if( x < 0 || x > maxX || y < 0 || y > maxY )
    return ( outOfBoundsReturnsVal );
  return (data[y*maxX + x] == val) ? 1 : 0;
}


//------------------------
//-- Returns the fraction (between 0 and 1) of cont1 which overlaps cont2
//-- or fraction of cont2 which overlaps cont1: whichever is bigger.

float img_fractionOverlap( Icont *cont1, Icont *cont2 )
{
  float fract1, fract2;
  Ipoint c1LL, c1UR, c2LL, c2UR;
  imodContourGetBBox(cont1, &c1LL, &c1UR);
  imodContourGetBBox(cont2, &c2LL, &c2UR);
  
  Icont *cs1 = imodel_contour_scan( cont1 );
  Icont *cs2 = imodel_contour_scan( cont2 );
  
  imodel_overlap_fractions( &cs1,c1LL,c1UR,
                            &cs2,c2LL,c2UR,
                            &fract1,&fract2 );
  imodContourDelete( cs1 );
  imodContourDelete( cs2 );
  
  return( max( fract1,fract2 ) );
}


//------------------------
//-- Inputs a 2D "image" of w*h pixels and reduces salt-and-pepper 
//-- noise using a 8-pixel neighbourhood method over the specified 
//-- number of "rounds".
//-- For each pixel with value "ON", if less than "minNeighborsOn" of it's 
//-- 8-adjacent neighbours are "ON" it will be set to "OFF".
//-- Returns the number of pixels changed.
//-- NOTE: "temp" should be an image of the same dimensions as "image" - used
//-- to work out which pixels to change.... if "temp" is NULL noise reduction
//-- will be antsiotropic, as each pixel changed will affect pixels not yet changed.
//-- 
//-- [7][6][5]
//-- [8] p [4]
//-- [1][2][3]


int img_noiseReduction( unsigned char ON, unsigned char OFF, int minNeighborsOn,
                    unsigned char *image, int w, int h, int rounds,
                    unsigned char *temp )
{
  int pixelsChanged = 0;
  int nPixels = w*h;
  
  if( temp )
  {
    for (int i=0; i<nPixels; i++)
      temp[i] = ON;
  }
  
  for (int r=0; r<rounds; r++)
  {
    for(int y=0; y<h; y++)
      for(int x=0; x<w; x++)
      {
        int pixIsOn  = isPixEqual(ON,image,x  ,y  , w,h,true);
        if(!pixIsOn)
          continue;
        
        int neigh1On = isPixEqual(ON,image,x-1,y-1, w,h,true);
        int neigh2On = isPixEqual(ON,image,x  ,y-1, w,h,true);
        int neigh3On = isPixEqual(ON,image,x+1,y-1, w,h,true);
        int neigh4On = isPixEqual(ON,image,x+1,y  , w,h,true);
        int neigh5On = isPixEqual(ON,image,x+1,y+1, w,h,true);
        int neigh6On = isPixEqual(ON,image,x  ,y+1, w,h,true);
        int neigh7On = isPixEqual(ON,image,x-1,y+1, w,h,true);
        int neigh8On = isPixEqual(ON,image,x-1,y  , w,h,true);
        
        int numNeighsOn = neigh1On + neigh2On + neigh3On + neigh4On
          + neigh5On + neigh6On + neigh7On + neigh8On;
        
        if( numNeighsOn < minNeighborsOn )
        {
          if(temp)
            pix(temp,x,y) = OFF;
          else
            pix(image,x,y) = OFF;
          pixelsChanged++;
        }
      }
  }
  
  if( temp )
  {
    for (int i=0; i<nPixels; i++)
      if( temp[i] == OFF)
        image[i] == OFF;
  }
  
  return (pixelsChanged);
}



//------------------------
//-- Creates a single contour around an edge of "ON" values
//-- starting at coordinates (startX,startY). The value of this pixel
//-- must be on, and the one below not on for it to work.
//-- "maxPts" can be used for an early exit.
//-- Returns 1 if successful, 0 if unsuccessful.
//-- Although this function works, it seems redundant now that I have
//-- "img_contoursFromImagePoints" working.

int img_createContourAroundEdge( Icont *cont, unsigned char ON,
                   int startX, int startY, int z,
                   unsigned char *image, int w, int h, int maxPts )
{
  //## CHECK PT IS ON EDGE AND START CONTOUR GOING LEFT:
  
  //cout << "Expanding contour ..." << endl;    //%%%%%
  bool pixOn      = isPixEqual(ON,image, startX,startY  , w,h);
  bool pixBelowOn = isPixEqual(ON,image, startX,startY-1, w,h);
  
  if( !pixOn || pixBelowOn )
  {
    wprint( "(%d,%d) is not a good starting point\n", startX, startY );
    return 0;
  }
    
  imodContourDefault(cont);
  imodPointAppendXYZ( cont, startX+1, startY, z );
  imodPointAppendXYZ( cont, startX  , startY, z );
  
  //## CONTINUE ADDING POINTS TO CONT AND FOLLOWING EDGE UNTIL
  //## LAST POINT MEETS UP WITH FIRST POINT:
  
  while( !ptsEqual( getLastPt(cont), getFirstPt(cont) ) )
  {
    int npts = psize(cont);
    if( npts > maxPts )
      return 0;
    
    int x  = (int)getPt( cont, npts-1 )->x;
    int y  = (int)getPt( cont, npts-1 )->y;
    int xS = (int)getPt( cont, npts-2 )->x;
    int yS = (int)getPt( cont, npts-2 )->y;
    
    bool segUp    = ( y == yS+1 );
    bool segDown  = ( y == yS-1 );
    bool segLeft  = ( x == xS-1 );
    bool segRight = ( x == xS+1 );
    
    bool pixBLOn = isPixEqual(ON,image, x-1,y-1, w,h);
    bool pixTLOn = isPixEqual(ON,image, x-1,y  , w,h);
    bool pixTROn = isPixEqual(ON,image, x  ,y  , w,h);
    bool pixBROn = isPixEqual(ON,image, x  ,y-1, w,h);
    
    /*
    cout << " x=" << x << " y=" << y << " xS=" << xS << " yS=" << yS << endl;
    cout << " segUp=" << segUp << " segDown=" << segDown
      << " segLeft=" << segLeft << " segRight=" << segRight << endl;
    cout << " pixBLOn=" << pixBLOn << " pixTLOn=" << pixTLOn
      << " pixTROn=" << pixTROn << " pixBROn=" << pixBROn << endl;
    */
    
    if( segUp )           // if segment UP
    {
      if     ( pixTLOn )   imodPointAppendXYZ( cont, x-1,y  , z);
      else if( pixTROn )   imodPointAppendXYZ( cont, x  ,y+1, z);
      else if( pixBROn )   imodPointAppendXYZ( cont, x+1,y  , z);
      else{ wprint("EU\n"), imodPointAppendXYZ( cont, x,y+1  , z); }
    }
    else if( segLeft )    // if segment LEFT
    {
      if     ( pixBLOn )   imodPointAppendXYZ( cont, x  ,y-1, z);
      else if( pixTLOn )   imodPointAppendXYZ( cont, x-1,y  , z);
      else if( pixTROn )   imodPointAppendXYZ( cont, x  ,y+1, z);
      else{ wprint("EL\n"), imodPointAppendXYZ( cont, x,y+1  , z); }
    }
    else if( segDown )    // if segment DOWN
    {
      if     ( pixBROn )   imodPointAppendXYZ( cont, x+1,y  , z);
      else if( pixBLOn )   imodPointAppendXYZ( cont, x  ,y-1, z);
      else if( pixTLOn )   imodPointAppendXYZ( cont, x-1,y  , z);
      else{ wprint("ED\n"), imodPointAppendXYZ( cont, x,y+1  , z); }
    }
    else if( segRight )    // if segment RIGHT
    {
      if     ( pixTROn )   imodPointAppendXYZ( cont, x  ,y+1, z);
      else if( pixBROn )   imodPointAppendXYZ( cont, x+1,y  , z);
      else if( pixBLOn )   imodPointAppendXYZ( cont, x  ,y-1, z);
      else{ wprint("ER\n"), imodPointAppendXYZ( cont, x,y+1  , z); }
    }
    else
    { wprint("ERROR\n"), imodPointAppendXYZ( cont, x,y+1  , z); }
    
    
  }
  
  return 1;
}

//------------------------
//-- Scales the image value in the current slice so the minimum and
//-- maximum values span from 0 to 255

void img_scaleSlice( unsigned char **slice, int w, int h )
{
  int minVal = 255;
  int maxVal = 0;
  
  for (int y = 0; y < h; y++) {
  for (int x = 0; x < w; x++) {
      minVal = min( minVal, (int)slice[y][x] );
      maxVal = max( maxVal, (int)slice[y][x] );
  }}
  
  if( maxVal - minVal == 0 ) {
    wprint("ERROR: img_scaleSlice - no variation in values");
    return;
  }
  float scaleFact = 255 / (maxVal - minVal);
  
  cout << "minVal=" << minVal << endl;
  cout << "maxVal=" << maxVal << endl;
  
  for (int y = 0; y < h; y++) {
  for (int x = 0; x < w; x++) {
    slice[y][x] = ((int)slice[y][x] - minVal) * scaleFact; 
  }}
}


//------------------------
//-- Puts the value from "data" into "slice" and 
//-- will "scale" and "redraw" the image values if needed.
//-- Note that the screen may appear black or white until
//-- you hit autocontrast [shift]+[a]

int img_changeSliceData( unsigned char **slice, unsigned char *data,
                     int w, int h, bool scale, bool redraw )
{
  cout << "CHANGING SLICE" << endl;
  
  for (int y = 0; y < h; y++)
    for(int x = 0; x < w; x++)
      slice[y][x] = pix(data,x,y);
  
  if(scale)
    img_scaleSlice(slice,w,h);
  
  if(redraw)
    ivwDraw( plug.view, IMOD_DRAW_IMAGE );
  
  //QKeyEvent event(QEvent::KeyPress, Qt::Key_PagDown, 0, "=");
  //imodPlugKeys(plug.view, &event);
  //int z = edit_getZOfTopZap();
  //ivwSetTopZapCenter( plug.view, w*0.5, w*0.5, z, true );
}


//------------------------
//-- Sets all the pixels in "data" to the given value

int img_setAll( unsigned char *data, unsigned char VAL, int w, int h )
{
  for(int y=0; y<plug.ysize; y++)
    for(int x=0; x<plug.xsize; x++)
      pix(data,x,y) = VAL;
}

//------------------------
//-- Returns the number of pixels in "data" equal to the given value

long img_countOccurances( unsigned char *data, unsigned char VAL, int w, int h )
{
  long count = 0;
  for(int y=0; y<plug.ysize; y++)
    for(int x=0; x<plug.xsize; x++)
      if( pix(data,x,y) == VAL)
        count++;
  return (count);
}

//------------------------
//-- Sets pixels in "data" to ON, if the corresponding pixel in "slice" has a value
//-- between "minVal" and "maxVal", or OFF, if the value is outside this range.
//-- If "skipPixelsAlreadyOff" is ticked, any "OFF" pixel in data is skipped
//-- (i.e. left off)
//-- Returns the number of values turned on.

int turnOnRangeOfValues( unsigned char *data, unsigned char ON, unsigned char OFF,
                         int minVal, int maxVal, bool skipPixelsAlreadyOff,
                         unsigned char **slice, int w, int h )
{
  int numPixelsOn = 0;
  
  for(int y=0; y<plug.ysize; y++)
  {
    for(int x=0; x<plug.xsize; x++)
    {
      if( skipPixelsAlreadyOff && pix(data,x,y)==OFF )
        continue;
      
      int greyVal = (int)slice[y][x];
      if( greyVal >= minVal && greyVal <= maxVal )
      {
        pix(data,x,y) = ON;
        numPixelsOn++;
      }
      else
        pix(data,x,y) = OFF;
    }
  }
  
  return (numPixelsOn);
}


//------------------------
//-- Turns "ON" every pixel in "data" which falls inside the contour.
//-- If "toggle" checked, "ON" pixels inside the contour are turned "OFF" and vice versa.

void img_changeValuesInsideContour( Icont *cont, unsigned char ON, unsigned char OFF,
                               bool toggle, unsigned char *image, int w, int h )
{
  if( psize(cont)<2 )
    return;
  
  Ipoint ll, ur;
  imodContourGetBBox(cont, &ll, &ur);
  
  int minX = max(0,(int)ll.x);
  int minY = max(0,(int)ll.y);
  int maxX = min(w,(int)ur.x);
  int maxY = min(h,(int)ur.y);
  
  Ipoint pt;
  setPt(&pt,minX,minY, getZInt(cont) );
  
  for (int y = minY; y < maxY; y++)
  {
    for(int x = minX; x < maxX; x++)
    {
      pt.x = x+0.5f;
      pt.y = y+0.5f;
      if( imodPointInsideCont( cont, &pt ) )
      {
        if(toggle)
        {
          if( pix(image,x,y)==ON )
            pix(image,x,y) = OFF;
          else
            pix(image,x,y) = ON;
        }
        else
        {
          pix(image,x,y)=ON;
        }
      }
    }
  }
}

//------------------------
//-- Turns "ON" every pixel in "data" which falls inside a contour in the given object
//-- "obj" on the specified slice "z".
//-- If "toggle" checked, "ON" pixels inside contours are turned "OFF" and vice versa.

void img_changeValuesInsideObject( Iobj *obj, unsigned char ON, unsigned char OFF,
                              bool toggle, unsigned char *image, int w, int h, int z)
{
  for( int c=0; c<csize(obj); c++ )
  {
    Icont *cont = getCont( obj, c );
    if( getZInt(cont) != z )
      continue;
    
    img_changeValuesInsideContour( cont, ON, OFF, toggle, image, w, h );
  }
}


//------------------------
//-- Is almost identical to "imodContoursFromImagePoints()" in "libimod/cont.c".
//-- Unfortunately I was unable to pull more than the first contour out of the
//-- returned "*Icont". This function instead uses a vector of "IcontPtr".
//-- 
//-- EXCERPT from "cont.c":
//--    Forms contours around marked points in an array. ^
//--    [data] is an array of flags marking the image points. ^
//--    [imdata] is the corresponding actual image array, which is used to compute
//--    interpolated positions for the edges between marked and unmarked points.
//--    If is NULL, no interpolation is done and contours will follow horizontal,
//--    vertical, and 45 degree diagonal lines. ^
//--    [xsize] and [ysize] specify the X and Y dimensions of the array. ^
//--    [z] is the Z value to assign to the contours. ^
//--    [testmask] is the value to AND with the image flag points to select them.
//--    The mask should use the first four bits only. ^
//--    If [diagonal] is non-zero, then pixels that touch only at corners will be
//--    contained within the same contour. ^
//--    [threshold] is the threshold to use for interpolating the edge position
//--    between pixels.  If it is less than 0 and [imdata] is defined, then an 
//--    effective threshold will be computed from the mean value of pixels on
//--    both sides of the edges. ^
//--    [reverse] should be set non-zero if marked pixels have lower values than
//--    unmarked ones. ^
//--    The number of contours created is returned in [ncont]. ^
//--    The function returns a pointer to an array of contours, or NULL for error.


#define RIGHT_EDGE  16
#define TOP_EDGE    (RIGHT_EDGE << 1)
#define LEFT_EDGE   (RIGHT_EDGE << 2)
#define BOTTOM_EDGE (RIGHT_EDGE << 3)
#define ANY_EDGE    (RIGHT_EDGE | TOP_EDGE | LEFT_EDGE | BOTTOM_EDGE )

long img_contoursFromImagePoints( vector<IcontPtr> &contSegs,
                                     unsigned char *data, unsigned char **imdata,
                                     int xsize, int ysize, int z, 
                                     unsigned char testmask, int diagonal,
                                     float threshold, int reverse, long maxConts )
{
  long ncont = 0;
  
  Ipoint point;
  Icont *cont;
  int i, j, itst, jtst;
  int side;
  int xs, ys;
  int edgemask[4] = {RIGHT_EDGE, TOP_EDGE, LEFT_EDGE, BOTTOM_EDGE};
  int nextx[4]    = {0, -1, 0, 1};
  int nexty[4]    = {1, 0, -1, 0};
  int cornerx[4]  = {1, -1, -1, 1};
  int cornery[4]  = {1, 1, -1, -1};
  int otherx[4]   = {1, 0, -1, 0};
  int othery[4]   = {0, 1, 0, -1};
  float frac;
  int found, iedge, ixst, iyst, iedgest, nsum, nayx, nayy, polarity, diff;
  double edgeSum;
  
  xs = xsize - 1;
  ys = ysize - 1;
  point.z = z;
  nsum = 0;
  edgeSum = 0.;
  polarity = reverse ? -1 : 1;
  if (!imdata)
    threshold = -1.;
  
  // Go through all points including the edges of the image area, and 
  //  mark all the edges of defined area.  Compute a 
  
  for (j = 0; j < ysize; j++) {
    for(i = 0; i < xsize; i++)
    {
      if (data[i + (j * xsize)] & testmask) {
        
        // Mark a side if on edge of image, or if next pixel over
        // is not in the set
        side = 0;
        if (i == xs)
          side |= RIGHT_EDGE;
        else if (!(data[(i + 1) + (j * xsize)] & testmask)) {
          side |= RIGHT_EDGE;
          if (threshold < 0 && imdata) {
            edgeSum += imdata[j][i] + imdata[j][i + 1];
            nsum++;
          }
        }
        if (j == ys)
          side |= TOP_EDGE;
        else if (!(data[i + ((j + 1) * xsize)] & testmask)) {
          side |= TOP_EDGE;
          if (threshold < 0 && imdata) {
            edgeSum += imdata[j][i] + imdata[j + 1][i];
            nsum++;
          }
        }
        if (!i)
          side |= LEFT_EDGE;
        else if (!(data[(i - 1) + (j * xsize)] & testmask)) {
          side |= LEFT_EDGE;
          if (threshold < 0 && imdata) {
            edgeSum += imdata[j][i] + imdata[j][i - 1];
            nsum++;
          }
        }
        if (!j)
          side |= BOTTOM_EDGE;
        else if (!(data[i + ((j - 1) * xsize)] & testmask)) {
          side |= BOTTOM_EDGE;
          if (threshold < 0 && imdata) {
            edgeSum += imdata[j][i] + imdata[j - 1][i];
            nsum++;
          }
        }
        data[i +(j * xsize)] |= side;
        // if (side) printf("i %d  j %d  side %d  data %d\n",
        //  i, j, side, data[i +(j * xsize)]);
      }
    }
  }
  
  if (nsum) {
    threshold = 0.5 * edgeSum / nsum;
    // printf("Derived threshold %.2f\n", threshold);
  }
  
  found = 1;
  while (found)
  {
    found = 0;
    for (j = 0; j < ysize && !found; j++)
      for(i = 0; i < xsize; i++)
      {
        if (!(data[i + (j * xsize)] & ANY_EDGE))
          continue;
        
        //## FIND LOWEST EDGE:
        for (iedge = 0; iedge < 4; iedge++)
          if (data[i + j * xsize] & edgemask[iedge])
            break;
        
        //## ADD NEW CONTOUR:
        
        if( ncont == 0 || psize(contSegs.back().cont) )
          contSegs.push_back( IcontPtr() );
        cont = contSegs.back().cont;
        
        ncont = (int)contSegs.size();
        if( ncont % 1000 == 0 )
          cout << " " << ncont << " conts processed ..." << endl;
        if( ncont > maxConts )
          return (ncont);
        
        // keep track of starting place and stop when reach 
        // it again
        iedgest = iedge;
        ixst = i;
        iyst = j;
        
        // printf ("starting %d %d %d\n", ixst, iyst, iedge)
        while (!psize(cont) || i != ixst || j != iyst || iedge != iedgest)
        {
          if (!diagonal)
          {
            // If no diagonals, look for next edge first 
            // around corner on same pixel
            if (data[i + j * xsize] & 
                edgemask[(iedge + 1) % 4]) {
              iedge = (iedge + 1) % 4;
            } else if (data[i + nextx[iedge] + 
              (j + nexty[iedge]) * xsize] &
                       edgemask[iedge]) {
              // same edge, next pixel
              i += nextx[iedge];
              j += nexty[iedge];
            } else {
              // pixel on an inside corner - it's got to
              // be, but put in check for testing
              i += cornerx[iedge];
              j += cornery[iedge];
              iedge = (iedge + 3) % 4;
              if (!(data[i + j * xsize] & edgemask[iedge]))
                printf("no edge around corner at i %d, j %d, edge %d\n", 
                       i, j, iedge);
            }
          }
          else
          {
            // If diagonals, look for next edge first on pixel 
            // around inside corner if it's legal
            itst = i + cornerx[iedge];
            jtst = j + cornery[iedge];
            if (itst >= 0 && itst < xsize && jtst >= 0 && 
                jtst < ysize && (data[itst + jtst * xsize] & 
                                 edgemask[(iedge + 3) % 4])){
              i = itst;
              j = jtst;
              iedge = (iedge + 3) % 4;
            } else if (data[i + nextx[iedge] + 
              (j + nexty[iedge]) * xsize] &
                       edgemask[iedge]) {
              // then same edge, next pixel
              i += nextx[iedge];
              j += nexty[iedge];
            } else {
              // go around corner on this pixel - the
              // edge has to be there, but put in check
              // for testing
              iedge = (iedge + 1) % 4;
              if (!(data[i + j * xsize] & 
                    edgemask[iedge]))
                printf("no edge around corner at i %d, j %d, edge %d\n", 
                       i, j, iedge);
            }
          }
          
          frac = 0.45;
          nayx = i + otherx[iedge];
          nayy = j + othery[iedge];
          if (threshold > 0 && nayx >= 0 && nayx < xsize && nayy >= 0 &&
              nayy < ysize) {
            diff = imdata[nayy][nayx] - imdata[j][i];
            if (polarity * diff < 0) {
              frac = (threshold - imdata[j][i]) / diff;
              frac = B3DMIN(0.99, frac);
              frac = B3DMAX(0.01, frac);
            }
          }
          point.x = i + 0.5 + frac * otherx[iedge];
          point.y = j + 0.5 + frac * othery[iedge];
          
          // add the point and clear the edge
          // point.x = i + delx[iedge];
          // point.y = j + dely[iedge];
          imodPointAdd(cont, &point, psize(cont) );
          data[i + j * xsize] &= ~edgemask[iedge];
          // printf ("at %d %d %d, adding %f %f   diff %d  frac %.2f\n", 
          //                 i, j, iedge, point.x, point.y, diff, frac);
          
        }
        found = 1;
        break;
      }
  }
  
  cout << " " << contSegs.size() << " total conts processed ..." << endl;
  return (int)contSegs.size();
}




