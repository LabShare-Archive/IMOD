/*
 *  interpolator.c -- Special plugin for interpolation contours
 *
 */

/*****************************************************************************
*   Copyright (C) 2007 by Andrew Noske from the Institute for Molecular     *
*   Bioscience at the University of Queensland (Australia)                  *
*****************************************************************************/

/*  $Author$

    $Date$

    $Revision$

    Use hg log to see log
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
#include <qbuttongroup.h>
#include <qradiobutton.h>
#include <qdialog.h>
#include <qspinbox.h>
#include <qlayout.h>
#include <qgroupbox.h>
#include <qtooltip.h>
#include <qstringlist.h>
#include <qmessagebox.h>
#include <qinputdialog.h>
#include <qtoolbutton.h>
#include "../../3dmod/pegged.xpm"
#include "../../3dmod/unpegged.xpm"

#include <qdatetime.h>

#include "_common_functions.h"
#include "customdialog.h"
#include "imodplugin.h"
#include "dia_qtutils.h"
#include "interpolator.h"
#include "mkmesh.h"

#include <qfiledialog.h>

#include <sstream>			// for formatting string output
#include <fstream>			// for input/output of binary files

//############################################################

static InterpolatorData plug = { 0, 0 };

InterpolationEvent ie;

//############################################################

//------------------------
//-- MAPPED FUNCTION: Called by the imod plugin load function

const char *imodPlugInfo(int *type)
{
	if (type)
		*type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS + IMOD_PLUG_MESSAGE;
	return("Interpolator");
}

//------------------------
//-- MAPPED FUNCTION: Grab hotkey input. return 1 if we handle the key.

int imodPlugKeys(ImodView *vw, QKeyEvent *event)
{
  int keyhandled = 1;
  
  if (!plug.view)          // if plugin window isn't open: don't grab keys
    return 0;
  
  int keysym = event->key();            // key value (Key_A, Key_Space... etc)
  int ctrl    = event->modifiers() & Qt::ControlModifier;   // ctrl modifier
  int shift   = event->modifiers() & Qt::ShiftModifier;     // shift modifier
  
	switch(keysym)
  {
    case Qt::Key_Enter: 
    case Qt::Key_Return: 
			if(shift)
				plug.window->interpolateLinearWithLastContour();
			else
				plug.window->interpolateContour();
      break;
      
    //case Qt::Key_T:                  // temporary testing purposes - comment out
    //  if( ctrl && shift )
    //    plug.window->test();
    //  else
    //    return 0;
    //  break;
      
    case Qt::Key_X:
      if(!ctrl && !shift )
        plug.window->deleteNearbyInterpolatedContours();
      else
        return 0;
      break;
      
    case Qt::Key_G:
      if(!ctrl && !shift)
        plug.window->findNextIsolatedContour();
      else
        return 0;
      break;
      
    case Qt::Key_H:
      if(!ctrl)
        plug.window->findNextBiggestHoleBetweenKeyConts();
      else
        return 0;
      break;
      
    case Qt::Key_U: 
      plug.window->toggleInterp();
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
    plug.interType          = INT_SPHERICAL;
    plug.tilingMethod       = TM_AUTO;
    plug.surfResolveMethod	= SR_AUTO;
    plug.branchingMethod    = BR_BRANCHING_OFF;
		plug.ptResolveMethod    = PT_FOUR_PTS;
    
    plug.zBridge                 = 50;
    plug.interSepDistBetweenConts = 5.0;
    plug.interFractOverlap        = 0.5;
    plug.interLineTracker         = false;					//	( %%% NOT YET IMPLEMENTED %%% )
    plug.minHoleSize              = 10;
    plug.maxGapSize               = 5;
    
    plug.deselectAfterEnter       = true;
    plug.selectedAction           = 0;
    plug.hideSurfSettings         = false;
		
		plug.contIdxLastInterp        = -1;
		plug.objIdxLastInterp         = 0;
		
    plug.window->loadSettings();
    
    plug.initialized = true;
	}
  plug.view = inImodView;
	ivwEnableStipple( plug.view, 1 );     // enables the display of stippled lines
  ivwGetImageSize(inImodView, &plug.xsize, &plug.ysize, &plug.zsize);
  
	//## CREATE THE PLUGIN WINDOW:
  
	plug.window	= new Interpolator(imodDialogManager.parent(IMOD_DIALOG), "Interpolator");
	plug.window->setWindowTitle("Interpolator (*by SLASH*)");		// to help with our grant
	
	imodDialogManager.add((QWidget *)plug.window, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)plug.window, IMOD_DIALOG );
}


//############################################################

//----------------------------------------------------------------------------
//
//          Interpolator METHODS:
//
//----------------------------------------------------------------------------



//## THE WINDOW CLASS CONSTRUCTOR

static const char *buttonLabels[] = {(char*)"Done", (char*)"Video", (char *)"Help"};
static const char *buttonTips[]   = {(char*)"Close this plugin window",
	                             (char*)"See SLASH help videos showing \n"
	                                    "how to use this plugin",
														   (char*)"Open help window"};

Interpolator::Interpolator(QWidget *parent, const char *name)
	: DialogFrame(parent, 3, buttonLabels, buttonTips, true, "Interpolator", "", name)
{
	const int LAY_MARGIN = 4;
	const int LAY_SPACING = 4;
	const int GROUP_MARGIN = 1;
	const int SPACER_HEIGHT   = 15;
  
	//## RECOLOR MIDDLE "Video" BUTTON:
	
	mButtons[1]->setStyleSheet("color: rgb(150, 180, 255);");
	mButtons[1]->setFlat( true );
	mButtons[1]->setCursor( Qt::PointingHandCursor );
	mButtons[2]->setCursor( Qt::PointingHandCursor );
	
	
	//## Type:
	
	QGroupBox *typeGbox = new QGroupBox("Interpolation Type:", this);
  	typeButtonGroup = new QButtonGroup(this);
	QVBoxLayout *gbLayout = new QVBoxLayout(typeGbox);
	gbLayout->setSpacing(0);
	gbLayout->setContentsMargins(5, 2, 5, 5);
	typeGbox->setMaximumHeight(160);
	connect(typeButtonGroup, SIGNAL(buttonClicked(int)), this, SLOT(changeType(int)));
	
	typeRadio_None = diaRadioButton("None", typeGbox, typeButtonGroup, gbLayout, 0, 
					"No interpolation is applied");
	
	typeRadio_Linear = diaRadioButton("Linear", typeGbox, typeButtonGroup, gbLayout, 1, 
					  "Interpolated contours are generated in straight "
					  "lines between connected key contours");
	
	typeRadio_Spherical = diaRadioButton("Spherical", typeGbox, typeButtonGroup, gbLayout, 2,
					     "Best for segmenting approximately "
					     "spherical compartments");
	
	typeRadio_Smooth = diaRadioButton("Smooth", typeGbox, typeButtonGroup, gbLayout, 3, 
					  "Best for segmenting compilicated shapes");
	typeRadio_Smooth = diaRadioButton("Smooth Pts", typeGbox, typeButtonGroup, gbLayout, 4,
					  "Best for segmenting compilicated shapes... \n"
					  "But may be buggy");
  
	changeTypeSelected( plug.interType );
  	
  
  //## Pin-to-top Button:
  QHBoxLayout* topLay = new QHBoxLayout();
  topLay->setSpacing(LAY_SPACING);
  topLay->setContentsMargins(0,0,0,0);
  
  QVBoxLayout* pinLay = new QVBoxLayout();
  pinLay->setSpacing(0);
  pinLay->setContentsMargins(0,0,0,0);
  
  QToolButton *toolBut = new QToolButton(this);
  toolBut->setCheckable(true);
  toolBut->setFocusPolicy(Qt::NoFocus);
  QIcon iconSet;
  iconSet.addPixmap(QPixmap((const char **)pegged), QIcon::Normal, QIcon::On);
  iconSet.addPixmap(QPixmap((const char **)unpegged), QIcon::Normal, QIcon::Off);
  toolBut->setIcon(iconSet);
  toolBut->setChecked(false);
  QSize hint = toolBut->sizeHint();
  toolBut->setFixedWidth(hint.width());
  toolBut->setFixedHeight(hint.height());
  connect(toolBut, SIGNAL(toggled(bool)), this, SLOT(keepOnTop(bool)));
  toolBut->setToolTip("Keep interpolator window on top");
  
  pinLay->setAlignment( Qt::AlignTop );
  pinLay->addWidget(toolBut);
  topLay->addWidget(typeGbox);
  topLay->addLayout(pinLay);
  mLayout->addLayout(topLay);
	
	//## Interpolation Options:
	
	grpOptions = new QGroupBox("Interpolation Options:", this);
	grpOptions->setFocusPolicy(Qt::NoFocus);
	grpOptions->setMaximumHeight(75);
	grpOptions->setContentsMargins(GROUP_MARGIN, GROUP_MARGIN, GROUP_MARGIN, GROUP_MARGIN);
	
	//gridLayout1 = new QGridLayout();
  gridLayout1 = new QGridLayout(grpOptions);
	gridLayout1->setSpacing(LAY_SPACING);
	gridLayout1->setMargin(LAY_MARGIN);
	gridLayout1->addItem( new QSpacerItem(1,SPACER_HEIGHT), 0, 0);
	
	lblZBridge = new QLabel("Z Bridge:", grpOptions);
	lblZBridge->setFocusPolicy(Qt::NoFocus);
	gridLayout1->addWidget(lblZBridge, 1, 0);

	zBridgeSpinner = new QSpinBox(grpOptions);
	zBridgeSpinner->setFocusPolicy(Qt::NoFocus);
	zBridgeSpinner->setMinimum(1);
	zBridgeSpinner->setMaximum(200);
	zBridgeSpinner->setValue( plug.zBridge );
	QObject::connect(zBridgeSpinner, SIGNAL(valueChanged(int)), this, 
                  SLOT(changeZBridge(int)));
	zBridgeSpinner->setToolTip("The maximum slices apart two key contours can be "
                "but still 'connected' during interpolation");
	gridLayout1->addWidget(zBridgeSpinner, 1, 1);
	
	//applyLineTrackerCheckbox = new QCheckBox("Apply Line Tracker", grpOptions);
	//applyLineTrackerCheckbox->setFocusPolicy(Qt::NoFocus);
  //applyLineTrackerCheckbox->setChecked( plug.interLineTracker );
	//QObject::connect(applyLineTrackerCheckbox, SIGNAL(clicked()), this,
  //                 SLOT(changeLineTracker()));
	//applyLineTrackerCheckbox->setToolTip("Not implemented **");
	//gridLayout1->addWidget(applyLineTrackerCheckbox, 2, 0);
	
	mLayout->addWidget(grpOptions);
	//mLayout->addLayout(gridLayout1);
  
	//## Actions
	
	grpActions = new QGroupBox("Object Actions:", this);
	grpActions->setFocusPolicy(Qt::NoFocus);
	grpActions->setMaximumHeight(145);
	grpActions->setContentsMargins(GROUP_MARGIN, GROUP_MARGIN, GROUP_MARGIN, GROUP_MARGIN);
	
	vboxLayout1 = new QVBoxLayout(grpActions);
	vboxLayout1->setSpacing(LAY_SPACING);
	vboxLayout1->setMargin(LAY_MARGIN);
	vboxLayout1->addItem( new QSpacerItem(1,SPACER_HEIGHT) );
	
  
  applyInterpolationButton = new QPushButton("Interpolate Contour [enter]",
                                             grpActions);
	applyInterpolationButton->setFocusPolicy(Qt::NoFocus);
	connect(applyInterpolationButton, SIGNAL(clicked()), this,
          SLOT(interpolateContourBtn()));
	applyInterpolationButton->setToolTip
	  ( "Generates interpolated contours either side  <br>"
	    "of the current contour <br><br>"
	    "<b>TIP</b>: Use the <b>[enter]</b> key to perform this quickly<br>"
	    "and hold [shift]+[enter] to force linear interpolation <br>"
	    "with the last contour you interpolated" );																			
	vboxLayout1->addWidget(applyInterpolationButton);
  
  
	clearAllInterpButton = new QPushButton("Clear All Interp", grpActions);
	clearAllInterpButton->setFocusPolicy(Qt::NoFocus);
	connect(clearAllInterpButton, SIGNAL(clicked()), this,
          SLOT(clearInterpolatedContours()));
	clearAllInterpButton->setToolTip("Deletes all interpolated from "
                                   "the selected object");
	vboxLayout1->addWidget(clearAllInterpButton);
	
  
	regenerateInterpButton = new QPushButton("Regenerate Interp", grpActions);
	regenerateInterpButton->setFocusPolicy(Qt::NoFocus);
	connect(regenerateInterpButton, SIGNAL(clicked()), this,
          SLOT(regenerateInterpolation()));
	regenerateInterpButton->setToolTip("Re-interpolates every key contour in "
                                     "the current object");
	vboxLayout1->addWidget(regenerateInterpButton);
  
	
	mLayout->addWidget(grpActions);
	
	
	//## Surface:
	
	grpSurface = new QGroupBox("Surface Settings:", this);
	grpSurface->setFocusPolicy(Qt::NoFocus);
	grpSurface->setMaximumHeight(300);
	grpSurface->setContentsMargins(GROUP_MARGIN, GROUP_MARGIN, GROUP_MARGIN, GROUP_MARGIN);
	
	gridLayout2 = new QGridLayout(grpSurface);
	gridLayout2->setSpacing(LAY_SPACING);
	gridLayout2->setMargin(LAY_MARGIN);
	gridLayout2->addItem( new QSpacerItem(1,SPACER_HEIGHT), 0, 0);
	
  
  QString surfaceStr =
    "The method used to determine if two key contours are connected "
    "(i.e. part of same surface). "
    "\nTwo adjacent key contours with < 'Z Bridge' slices between them are connected if: "
    "\n"
	  "\n > auto - will choose the most appropriate option based on "
	    "which interpolation type you've selected [*****] <-- RECOMMENDED "
    "\n > touching - they touch (in XY) [***] "
    "\n > centroid - the centroid (center of mass) of either falls inside the "
      "other [****] "
    "\n > max dist - they are less the specified distance apart "
      "(More Settings > 'max distance') [**] "
    "\n > overlap - they have over the specified percentage overlap "
      "(More Settings > 'percent overlap') [*] <-- slowest "
    "\n > mbr touch - the minimum bounding rectangles (mbr) of both overlap in XY "
      "[*****] <-- fastest "
    "\n"
    "\nNOTE: 'centroid' is always be set when using spherical interpolation ";
  
	lblSurface = new QLabel("surface", grpSurface);
	lblSurface->setFocusPolicy(Qt::NoFocus);
	lblSurface->setToolTip(surfaceStr);
  gridLayout2->addWidget(lblSurface, 1, 0);
	
	surfaceCombo = new QComboBox(grpSurface);
	surfaceCombo->setFocusPolicy(Qt::NoFocus);
  surfaceCombo->clear();
  surfaceCombo->addItem("auto");
	surfaceCombo->addItem("touching");
  surfaceCombo->addItem("center");
  surfaceCombo->addItem("max dist");
  surfaceCombo->addItem("overlap");
  surfaceCombo->addItem("mbr touch");
  surfaceCombo->setCurrentIndex( plug.surfResolveMethod );
	connect(surfaceCombo, SIGNAL(activated(int)), this,
          SLOT(changeSurfaceMethod(int)));
	surfaceCombo->setToolTip(surfaceStr);
	gridLayout2->addWidget(surfaceCombo, 1, 1);
  
  
  QString tilingStr =
    "The method used to connect points between connected key contours. "
    "\n"
	  "\n > auto - will choose the most appropriate option based on "
	    "which interpolation type you've selected [*****] <-- RECOMMENDED "
	  "\n > length - matches pts eqivalent percentage length along "
      "contour from the closest point <-- fastest "
    "\n > min area - connects points in the configuration which minimizes the "
      "surfaces area of the results triangluar mesh <-- slower, but more accurate "
    "\n > feature - attempts to match any cavities in the shape [NOT IMPLEMENTED] ";
  
  lblTilingMethod = new QLabel("tiling method:", grpSurface);
	lblTilingMethod->setFocusPolicy(Qt::NoFocus);
	lblTilingMethod->setToolTip(tilingStr);
  gridLayout2->addWidget(lblTilingMethod, 2, 0);
  
	tilingMethodCombo = new QComboBox(grpSurface);
	tilingMethodCombo->setFocusPolicy(Qt::NoFocus);
	tilingMethodCombo->addItem("auto");
	tilingMethodCombo->addItem("length");
	tilingMethodCombo->addItem("min area");
  tilingMethodCombo->addItem("feature");
  tilingMethodCombo->setCurrentIndex( plug.tilingMethod );
	connect(tilingMethodCombo, SIGNAL(activated(int)), this,
          SLOT(changeTilingMethod(int)));
	tilingMethodCombo->setToolTip(tilingStr);
	gridLayout2->addWidget(tilingMethodCombo, 2, 1);
	
  
  QString branchStr =
    "The method used to deal with cases of branching contours / surfaces."
    "\n"
    "\n > off - don't look for branches (best for simple surfaces) [*****] <-- fastest "
    "\n > min area - connects braches seperately, then merges interpolated "
      "contours which overlap "
    "\n > merge conts - connects braches seperately, then merges interpolated "
      "contours which overlap "
    "\n > bridge gaps - creates a bridge above the gap [NOT IMPLEMENTED] "
    "\n > add point - adds a single point above the gap [NOT IMPLEMENTED] ";
  
	lblBranchMethod = new QLabel("branching", grpSurface);
	lblBranchMethod->setFocusPolicy(Qt::NoFocus);
	lblBranchMethod->setToolTip(branchStr);
  gridLayout2->addWidget(lblBranchMethod, 3, 0);
	
	branchMethodCombo = new QComboBox(grpSurface);
	branchMethodCombo->setFocusPolicy(Qt::NoFocus);
	branchMethodCombo->addItem("off");
  branchMethodCombo->addItem("merge conts");
  branchMethodCombo->addItem("bridge gaps");
  branchMethodCombo->addItem("add point");
  branchMethodCombo->setCurrentIndex( plug.branchingMethod );
	connect(branchMethodCombo, SIGNAL(activated(int)), this,
          SLOT(changeBranchingMethod(int)));
	branchMethodCombo->setToolTip(branchStr);
  gridLayout2->addWidget(branchMethodCombo, 3, 1);
	
  
	mLayout->addWidget(grpSurface);
	
	grpSurface->setVisible( !plug.hideSurfSettings );
  
	
  //## Extra Buttons
  
  widget1 = new QWidget(this);
  
  gridLayout3 = new QGridLayout(widget1);
  gridLayout3->setSpacing(LAY_SPACING);
  gridLayout3->setMargin(LAY_MARGIN);
  
  moreActionsButton = new QPushButton("More Actions", widget1);
  connect(moreActionsButton, SIGNAL(clicked()), this, SLOT(moreActions()));
  moreActionsButton->setToolTip("Contains several other actions I didn't want to squeeze "
                "into this window");
  gridLayout3->addWidget(moreActionsButton, 0, 0);
  
  moreSettingsButton = new QPushButton("More Settings", widget1);
  connect(moreSettingsButton, SIGNAL(clicked()), this, SLOT(moreSettings()));
  moreSettingsButton->setToolTip("Contains several other settings I didn't want to squeeze "
                "into this window");
  gridLayout3->addWidget(moreSettingsButton, 0, 1); 
  
  mLayout->addWidget(widget1);
  
  
  mLayout->addStretch();
  this->adjustSize();
  
	connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
}




//------------------------
//-- Loads most of the settings for InterpolatorData from user preferences

void Interpolator::loadSettings()
{
  double savedValues[NUM_SAVED_VALS];
  
  int nvals = prefGetGenericSettings("Interpolator", savedValues, NUM_SAVED_VALS);
  
  if(nvals!=NUM_SAVED_VALS )
  {
    wprint("DrawingTools: Could not load saved values");
    QMessageBox::about( this, "-- Documentation --",
                        "If this is your first time using 'Interpolator' \n"
                        "we HIGHLY recommended you click 'Help' \n"
                        "(at bottom of the plugin) and watch the video tutorial! \n\n"
                        "                                   -- Andrew Noske" );
    return;
  }
  
  plug.interType                  = savedValues[0];
  plug.tilingMethod               = savedValues[1];
  plug.surfResolveMethod          = savedValues[2];
  plug.branchingMethod            = savedValues[3];
	plug.ptResolveMethod            = savedValues[4]; 
  plug.zBridge                    = savedValues[5];
  plug.interSepDistBetweenConts   = savedValues[6];
  plug.interFractOverlap          = savedValues[7];
  plug.interLineTracker           = savedValues[8];
  plug.minHoleSize                = savedValues[9];
  plug.maxGapSize                 = savedValues[10];
  plug.selectedAction             = savedValues[11];
	plug.hideSurfSettings           = savedValues[12];
}


//------------------------
//-- Saves most of the settings within InterpolatorData in user preferences
//-- so they will load next time the plugin is started

void Interpolator::saveSettings()
{
  double saveValues[NUM_SAVED_VALS];
  
  saveValues[0]  = plug.interType;
  saveValues[1]  = plug.tilingMethod;
  saveValues[2]  = plug.surfResolveMethod;
  saveValues[3]  = plug.branchingMethod;
	saveValues[4]  = plug.ptResolveMethod; 
  saveValues[5]  = plug.zBridge;
  saveValues[6]  = plug.interSepDistBetweenConts;
  saveValues[7]  = plug.interFractOverlap;
  saveValues[8]  = plug.interLineTracker;
  saveValues[9]  = plug.minHoleSize;
  saveValues[10] = plug.maxGapSize;
  saveValues[11] = plug.selectedAction;
  saveValues[12] = plug.hideSurfSettings;
	
  prefSaveGenericSettings("Interpolator",NUM_SAVED_VALS,saveValues);
}



//------------------------
//-- Change to flag to keep on top or run timer as for info window
void Interpolator::keepOnTop(bool state)
{
#ifdef STAY_ON_TOP_HACK
  mStayOnTop = state;
  // Start or kill the timer
  if (state)  
    mTopTimerID = startTimer(200);
  else if (mTopTimerID) {
    killTimer(mTopTimerID);
    mTopTimerID = 0;
  }
#else
  Qt::WindowFlags flags = windowFlags();
  if (state)
    flags |= Qt::WindowStaysOnTopHint;
  else
    flags ^= Qt::WindowStaysOnTopHint;
  QPoint p2 = pos();
  setWindowFlags(flags);
  move(p2);
  show();
#endif
}



//## BUTTON EVENT METHODS:

//------------------------
//-- Execute interpolation on selected contour

void Interpolator::interpolateContour()
{
	Imod *imod = ivwGetModel(plug.view);
	Iobj *obj  = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
	
	int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
	if( !isContValid(cont) || isEmpty(cont)  )
	{
		wprint("\aHave not selected valid contour\n");
		return;
	}
	
	if( isInterpolated(cont) )
	{
		undoContourPropChgCC( plug.view );            // REGISTER UNDO
		setInterpolated( cont, 0 );
		undoFinishUnit( plug.view );                  // FINISH UNDO
	}
	
	ie.performInterpolationOnCont(obj, contIdx, plug.interType);
  
  if(plug.deselectAfterEnter)
    imodSetIndex(imod, objIdx, -1, -1 );					// deselects current contour
	
	plug.contIdxLastInterp = contIdx;		// |- recorded for use in
	plug.objIdxLastInterp  = objIdx;		// |  "interpolateLinearWithLastContour()"
	
  ivwRedraw( plug.view );
}

//------------------------
//-- Called when the "Interpolate" button is pressed.
//-- Executes interpolation on selected contour and displays
//-- a message on the 10th time the button is pressed.

void Interpolator::interpolateContourBtn()
{
	static int numTimesPressed = 0;
	numTimesPressed++;
	
	if( numTimesPressed == 10 )
	{
		MsgBox( this, "...", "You do realize you can press [enter] \n"
					               "to interpolate right?! \n\n"
					               "Read the 'help' page for more info.");
	}
	
	interpolateContour();
}

//------------------------
//-- Executes linear interpolation between the currently selected
//-- contour and whatever contour was last interpolated
//-- as recorded in the variables "plug.contIdxLastInterp" and 
//-- "plug.objIdxLastInterp". Note that this will ignore
//-- the values of zBridge and interType - and interpolated
//-- linearly between any two contours so long as they're in
//-- the same object

void Interpolator::interpolateLinearWithLastContour()
{
	Imod *imod = ivwGetModel(plug.view);
	Iobj *obj  = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
	
	int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
	if( !isContValid(cont) || isEmpty(cont)  )
	{
		wprint("\aHave not selected valid contour\n");
		return;
	}
	
	if( plug.objIdxLastInterp != objIdx || plug.contIdxLastInterp == contIdx
	 || plug.contIdxLastInterp < 0      || plug.contIdxLastInterp >= csize(obj) )
	{
		wprint("\aWith shift down must interpolate to last interpolated "
					 "contour in same object\n");
		
		plug.contIdxLastInterp = contIdx;		// |- recorded for use in
		plug.objIdxLastInterp  = objIdx;		// |  "interpolateLinearWithLastContour()"
		
		return;
	}
	
	if( isInterpolated(cont) )
	{
		undoContourPropChgCC( plug.view );            // REGISTER UNDO
		setInterpolated( cont, 0 );
		undoFinishUnit( plug.view );                  // FINISH UNDO
	}
	
	ie.interp_Linear_BetweenTwoConts( contIdx, plug.contIdxLastInterp );
	
  if(plug.deselectAfterEnter)
    imodSetIndex(imod, objIdx, -1, -1 );					// deselects current contour
	
	plug.contIdxLastInterp = contIdx;		// |- recorded for use in
	plug.objIdxLastInterp  = objIdx;		// |  "interpolateLinearWithLastContour()"
	
  ivwRedraw( plug.view );
}

//------------------------
//-- Deletes any (same surface) interpolated contours either side of the
//-- currently selected contour between the nearest key contours.

void Interpolator::deleteNearbyInterpolatedContours()
{
	Imod *imod = ivwGetModel(plug.view);
	Iobj *obj  = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
	
	int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
	if( !isContValid(cont) || isEmpty(cont)  )
	{
		wprint("\aHave not selected valid contour\n");
		return;
	}
	
  int contsDeleted = ie.deleteImmediatelyAdjacentInterpolatedConts(obj, contIdx);
  
  wprint("%d interpolated contours deleted\n", contsDeleted);
  
  imodSetIndex(imod, objIdx, -1, -1 );					// deselects current contour
  undoFinishUnit( plug.view );                  // FINISH UNDO
  
  ivwRedraw( plug.view );
}


//------------------------
//-- Finds and selects the next contour past the currently selected contour which is 
//-- either isolated (ie. has no same surface contours in the slice immediately above or
//-- or below it), or is on the bottom edge of a gap (ie. there is a same surface contour
//-- within "plug.maxGapSize" slice above it, but not immediately above).

void Interpolator::findNextIsolatedContour()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
  
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  for (int i=0; i<csize(obj); i++)      // for each contour (contI) in obj:
  {
    int cIdxI = (i+contIdx+1) % csize(obj);
    Icont *contI = getCont(obj,cIdxI);
    int zI = getZ(contI);
    
    if( cIdxI == 0 )
      wprint("Starting from beginning (first contour)...\n");
    
    //## CHECK IF CONTOUR IS ISOLATED:
    
    bool contAbove = false;
    bool contBelow = false;
    
    for (int j=0; j<csize(obj); j++)      // for each contour (contJ) in obj:
    {
      Icont *contJ = getCont(obj,j);
      int zJ = getZ(contJ);
      
      if( zJ == zI+1 && ie.contoursSameSurfSlow(contI, contJ) )
      {
        contAbove = true;
        break;
      }
      else if( zJ == zI-1 && ie.contoursSameSurfSlow(contI, contJ) )
      {
        contBelow = true;
      }
    }
    
    if( !contAbove && !contBelow )        // if no adjacent contours found:
    {
      imodSetIndex( imod, objIdx, cIdxI, 0 );
      wprint("Isolated contour found\n");
      ivwRedraw( plug.view );
      return;
    }
    
    
    //## CHECK IF CONTOUR IS ON EDGE OF GAP:
    
    if( contAbove || plug.maxGapSize == 0 )     // if we found cont above, cannot be gap
      continue;
    
    bool contFoundInGapAbove = false;
    int maxZToSearch        = zI+plug.maxGapSize+1;
    
    for (int j=0; j<csize(obj); j++)      // for each contour in obj:
    {
      Icont *contJ = getCont(obj,j);
      int zJ = getZ(contJ);
      
      if( zJ > (zI+1) && zJ <= (maxZToSearch) )
      {
        if( ie.contoursSameSurfSlow(contI, contJ) )
        {
          contFoundInGapAbove = true;
          break;
        }
      }
    }
    
    if( contFoundInGapAbove )           // if gap was found:
    {
      imodSetIndex( imod, objIdx, cIdxI, 0 );
      wprint("Gap above contour %d\n", cIdxI+1);
      ivwRedraw( plug.view );
      return;
    }
  }
  
  wprint("\aNo more isolated contours in this object\n");
}


//------------------------
//-- Find the next biggest ho

void Interpolator::findNextBiggestHoleBetweenKeyConts()
{
	Imod *imod = ivwGetModel(plug.view);
	Iobj *obj  = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
  
	int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
  int idxMiddle = ie.findMiddleNextLargeInterpolatedSpan(obj, contIdx, plug.minHoleSize);
  
  if( idxMiddle != -1 )
    imodSetIndex(imod, objIdx, idxMiddle, 0 );					// select contour
  else
    wprint("\aNo more holes between key conts >= %d found\n", plug.minHoleSize);
  
  ivwRedraw( plug.view );
}


//------------------------
//-- Toggles the current contour's stippled flag

void Interpolator::toggleInterp()
{
  if( !isCurrContValid() )
    return;
  
  Icont *cont = getCurrCont();
  undoContourPropChgCC( plug.view );        // REGISTER UNDO
  setInterpolated( cont, isInterpolated(cont) ? 0 : 1 );
  undoFinishUnit( plug.view );              // FINISH UNDO
	ivwRedraw( plug.view );
}


//------------------------
//-- Counts the number of interpolated contours, key contours and
//-- non-empty contours in the object.

bool Interpolator::countContoursCurrObj( int &interp, int &key,
                                         int &totalNonEmpty )
{
  totalNonEmpty = 0;
	interp = 0;
	key = 0;
  
  if( !isCurrObjValid() )
    return false;
  
	Imod *imod = ivwGetModel(plug.view);
	Iobj *obj  = imodObjectGet(imod);
	Icont *cont;
  
	for(int c=0; c < csize( obj ); c++)
	{
		cont = getCont( obj, c );
    if( isEmpty(cont) )
      continue;
    
    totalNonEmpty++;
		if( isInterpolated( cont ) )
      interp++;
    else
      key++;
	}
}


//------------------------
//-- Removes all interpolated contours from the selected object

void Interpolator::clearInterpolatedContours()
{
  if( !isCurrObjValid() )
  {
    MsgBox("Current contour is not valid");
    return;
  }
  
  //## PROMPT USER TO CONTINUE:
  
  int interp, key, totalNonEmpty;
  countContoursCurrObj( interp, key, totalNonEmpty );
  if ( interp>1 && 
       !MsgBoxYesNo( this, "Delete all " + toString(interp)
                     + " interpolated contours?" ) )
    return;
  
  //## REMOVE ALL INTERPOLATED CONTOURS:
    
	Imod *imod = ivwGetModel(plug.view);
	Iobj *obj  = imodObjectGet(imod);
	Icont *cont;
  
	int nDeleted = 0;
	int error = 0;
	
	for(int c=0; c < csize( obj ); c++)
	{
		cont = getCont( obj, c );
		if( isInterpolated( cont ) )
		{
			undoContourRemovalCO( plug.view, c );              // REGISTER UNDO
			error += imodObjectRemoveContour( obj, c );
			nDeleted++;
			c--;
		}
	}
	if (nDeleted)
		undoFinishUnit( plug.view );                          // FINISH UNDO
	if(error)
		wprint("%d ERRORS HAVE OCCURED\n", error);
	
	ivwRedraw( plug.view );
	wprint("%d interpolated contours have been removed\n", nDeleted);
}

//------------------------
//-- Generates interpolation for all key contours in the selected object

void Interpolator::regenerateInterpolation()
{
  if( !isCurrObjValid() )
  {
    MsgBox("Current contour is not valid");
    return;
  }
  
  //## PROMPT USER TO CONTINUE:
  
  int interp, key, totalNonEmpty;
  countContoursCurrObj( interp, key, totalNonEmpty );
  if ( key == 0 )
  {
    MsgBox("There are no key contours to interpolated");
    return;
  }
  if ( !MsgBoxYesNo( this, "Are you sure you want to "
                     "reinterpolate all " + toString(key)
                     + " key contours?" ) )
  {
    return;
  }
  
  int numKeyDone = 0;
	Imod *imod = ivwGetModel(plug.view);
	Iobj *obj  = imodObjectGet(imod);
  
  int numContsStart = csize( obj );
  
  wprint("\nInterpolating   0/%d  (0%%)", key);
  
	for(int c=0; c < numContsStart && c < csize( obj ); c++)
	{
		Icont *cont = getCont( obj, c );
    if( isEmpty(cont) )
      continue;
    
		if( !isInterpolated( cont ) )
    {
      ie.performInterpolationOnCont( obj, c, plug.interType );
      numKeyDone++;
      int percentDone = calcPercentFloor( numKeyDone, key );
      wprint("Interpolated %d / %d  (%d%%)\r", numKeyDone, key, percentDone);
      QApplication::flush();
    }
	}
  
  undoFinishUnit( plug.view );                  // FINISH UNDO
  
	ivwRedraw( plug.view );
	wprint("\rInterpolated contours have been regenerated\n");
}


//------------------------
//-- Marks all contours in selected object as normal

void Interpolator::changeInterpContRange()
{
  if( !isCurrObjValid() )
  {
    MsgBox("Current object is not valid");
    return;
  }
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int interp, key, totalNonEmpty;
  countContoursCurrObj( interp, key, totalNonEmpty );
  
  Iobj *obj  = getCurrObj();
  int nConts = csize(obj);
  
  int contMin             = 1;
  int contMax             = nConts;
  static int interpolated = 0;
  
	CustomDialog ds("Change Options", this);
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour \n(inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour \n(inclusive) will be changed" );
	ds.addRadioGrp( "mark as:",
                  "key contours|"
                  "interpolated contours",
                  &interpolated );
  ds.addLabel   ( "... are you sure?" );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  contMin -= 1;
  contMax -= 1;
  
  //## MARK ALL CONTOURS IN RANGE AS KEY/INTERPOLATED:
  
  Imod *imod  = ivwGetModel(plug.view);
  int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	int nChanged = 0;
  
	for(int c=contMin; c<=contMax && c<csize(obj); c++)
	{
		Icont *cont = getCont( obj, c );
		if( isInterpolated( cont ) != interpolated )
		{
			undoContourPropChg( plug.view, objIdx, c );           // REGISTER UNDO
			setInterpolated( cont, interpolated );
			nChanged++;
		}
	}
	if(nChanged)
		undoFinishUnit( plug.view );                           // FINISH UNDO
	
	ivwRedraw( plug.view );
  if(interpolated)
    wprint("Changed %d of %d contours to interpolated\n", nChanged, contMax-contMin );
  else
    wprint("Changed %d of %d contours to key\n", nChanged, contMax-contMin );
}


//------------------------
//-- Prints a list of contours in the current object which don't have any
//-- matching contour in the slice above or below belonging to the same surface

void Interpolator::findIsolatedContours()
{
  if( !isCurrObjValid() )
  {
    MsgBox("Current object is not valid");
    return;
  }
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool listIsolated = true;
  static bool listGaps     = true;
  static int  gapSize      = plug.maxGapSize;
  
	CustomDialog ds("Find Isolated Contours", this);;
  ds.addLabel   ( "contour range:" );
	ds.addCheckBox( "list isolated contours",
                  &plug.deselectAfterEnter,
                  "Prints out any contour which had no matching (same surface)\n"
                  "contours above or below it" );
	ds.addCheckBox( "check for gaps",
                  &plug.deselectAfterEnter,
                  "Checks for missing contours/slices in an otherwise \n"
                  "continuous surface" );
  ds.addSpinBox ( "max gap size:", 1, 10, &plug.maxGapSize, 1,
                  "Checks for gaps in a surface over this many slices \n"
                  "(similar to 'Z Bride')" );
  ds.addLabel   ( "... NOTE: uses currently selected 'surf method'" );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  
  //## FIND ISOLATED CONTOURS:
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = getCurrObj();
  
  int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	int nIsolated = 0;
  int firstIsolatedContIdx = contIdx;
  
  wprint("\nISOLATED CONTOURS (OBJECT %d):\n", objIdx+1);
  
	for(int i=0; i<csize(obj); i++)
	{
		Icont *contI = getCont( obj, i );
		int zI = getZInt( contI );
    
    bool contIsolated = true;
    for(int j=0; j<csize(obj); j++)
    {
      Icont *contJ = getCont( obj, j );
      int zJ = getZInt( contJ );
      
      if( zJ == zI+1 || zJ == zI-1 )
      {
        if( ie.contoursSameSurfSlow(contI, contJ) )
        {
          contIsolated = false;
          break;
        }
      }
    }
    
    if( contIsolated == true )
    {
      wprint(" %d -> isolated\n", i+1 );
      if(nIsolated == 0)
        firstIsolatedContIdx = i;
      nIsolated++;
    }
	}
  
  
  //## FIND ANY GAPS:
  
  int nGaps = 0;
  
  if(listGaps)
  {
    for(int i=0; i<csize(obj); i++)
    {
      Icont *contI = getCont( obj, i );
      int zI = getZInt( contI );
      
      if(zI >= plug.zsize-2)
        continue;
      
      bool contAbove      = false;      // matching contour in the slice above found
      bool contInGapAbove = false;      // matching contour in the "gap region" found
      int closestMatchIdx = -1;
      int closestZ = zI+gapSize+2;
      
      for(int j=0; j<csize(obj); j++)
      {
        Icont *contJ = getCont( obj, j );
        int zJ = getZInt( contJ );
        
        if( zJ > zI )
        {
          if( zJ == zI+1 && ie.contoursSameSurfSlow(contI, contJ) )
          {
            contAbove = true;
            break;
          }
          else if( zJ < closestZ && ie.contoursSameSurfSlow(contI, contJ) )
          {
            closestZ = zJ;
            closestMatchIdx = j;
            contInGapAbove = true;
          }
        }
      }
      
      if( !contAbove && contInGapAbove )
      {
        wprint(" -> gap between %d and %d\n", i+1, closestMatchIdx+1 );
        nGaps++;
      }
      
    }
  }
  
  //## PRINT RESULTS:
  
  wprint("\n%d isolated contours found\n", nIsolated );
  wprint("%d gaps found\n", nGaps );
  
  if( nIsolated )
  {
    imodSetIndex(imod, objIdx, firstIsolatedContIdx, 0);
    ivwRedraw( plug.view );
  }
}



//------------------------
//-- Prints some basic interpolation information about the current object including
//-- the number of interpolated contours, key contours and empty contours.

void Interpolator::printModelInterpInfo()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  
  wprint("\nINTERPOLATION SUMMARY\n");
  
  int totConts  = 0;
  int totKey    = 0;
  int totInter  = 0;
  int totEmpty  = 0;
  
  for( int o=0; o<osize(imod); o++ )
  {
    Iobj *obj = getObj(imod,o);
    
    wprint("\nOBJECT %d\n", o+1);
    
    if( csize(obj)==0 )
    {
      wprint(" ... empty\n");
      continue;
    }
    
    int totObjConts  = csize(obj);
    int totObjKey   = 0;
    int totObjInter  = 0;
    int totObjEmpty  = 0;
    
    for( int c=0; c<csize(obj); c++ )
    {
      Icont *cont   = getCont(obj,c);
      if( isEmpty(cont) )
        totObjEmpty++;
      else if( isInterpolated(cont) )
        totObjInter++;
      else
        totObjKey++;
    }
    
    totConts += totObjConts;
    totKey  += totObjKey;
    totInter += totObjInter;
    totEmpty += totObjEmpty;
    
    int percentObjKey   = calcPercentInt( totObjKey,  totObjKey+totObjInter );
    int percentObjInter = calcPercentInt( totObjInter, totObjKey+totObjInter ); 
    
    wprint(" # conts = %d\n", totObjConts );
    wprint(" # empty conts  \t= %d\n", totObjEmpty );
    wprint(" # key conts    \t= %d (%d%%)\n", totObjKey,  percentObjKey );
    wprint(" # interpolated \t= %d (%d%%)\n", totObjInter, percentObjInter );
  }
  
  int percentKey   = calcPercentInt( totKey,  totKey+totInter );
  int percentInter = calcPercentInt( totInter, totKey+totInter ); 
  
  wprint("\n------------\n");
  wprint("OVERALL:\n");
  wprint(" # conts  = %d\n", totConts );
  wprint(" # empty conts  \t= %d\n", totEmpty );
  wprint(" # key conts    \t= %d (%d%%)\n", totKey,   percentKey );
  wprint(" # interpolated \t= %d (%d%%)\n", totInter, percentInter );
}


//------------------------
//-- Prints out a table with one slices per row showing the number of
//-- key contours and interpolated contours in the currently selected
//-- object which fall on each slice.

void Interpolator::printObjectSliceInterpInfo()
{
	if( !isCurrObjValid() )
  {
    wprint("Have not selected valid object\n");
    return;
  }
  
  Imod *imod = ivwGetModel(plug.view);
	Iobj *obj  = imodObjectGet(imod);
	int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
  
  int emptySlices = 0;
  int numSlicesKey = 0;
  int numSlicesInt = 0;
  int totKey = 0;
  int totInt = 0;
  
  ie.resetAll();
  ie.obj = obj;
  ie.regenerateZTables();
  
  
  wprint("\nOBJECT %d - CONTOURS ON EACH SLICE\n", objIdx+1);
  wprint("\n");
  wprint("SLICE\tN KEY\tN INTERP\n");
  wprint("----------------------------\n");
  
  
  for(int z=plug.zsize-1; z>=0; z--)
  {
    int numKey = ie.numKeyContsAtZ(z);
    int numInt = ie.numIntContsAtZ(z);
    
    totKey += numKey;
    totInt += numInt;
    
    //wprint("%d\t%d\t%d\n",z+1,numKey,numInt);
    
    wprint("%d\t",z+1);
    (numKey) ? wprint("%d\t",numKey) : wprint("-\t");
    (numInt) ? wprint("%d\t",numInt) : wprint("-\t");
    wprint("\n");
    
    if( numKey )
      numSlicesKey++;
    if( numInt )
      numSlicesInt++;    
    if( numKey==0 && numInt==0 )
      emptySlices++;
  }
  
  int totConts = totInt + totKey;
  int percentKey   = calcPercentInt( totKey, totConts );
  int percentInter = calcPercentInt( totInt, totConts ); 
  
  int percentSlicesEmpty = calcPercentInt( emptySlices,  plug.zsize ); 
  int percentSlicesKey   = calcPercentInt( numSlicesKey, plug.zsize ); 
  int percentSlicesInter = calcPercentInt( numSlicesInt, plug.zsize ); 
  
  wprint("\n------------\n");
  wprint(" # key conts   \t= %d (%d%%)\n", totKey, percentKey  );
  wprint(" # inter conts \t= %d (%d%%)\n", totInt, percentInter );
  wprint("\n" );
  wprint(" # empty slices   \t= %d (%d%%)\n", emptySlices,  percentSlicesEmpty );
  wprint(" # slices w key   \t= %d (%d%%)\n", numSlicesKey, percentSlicesKey );
  wprint(" # slices w interp\t= %d (%d%%)\n", numSlicesInt, percentSlicesInter );
}



//------------------------
//-- Gives a choice of several other options for the user.

void Interpolator::moreActions()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  CustomDialog ds("Perform Action", this);
  ds.addRadioGrp( "action:",
                  "mark contour range as interpolated|"
                  "find isolated contours|"
                  "print model interpolation info|"
                  "print number contours each slice",
                  &plug.selectedAction,
                  "",
                  "Lets you select a range of contours within the current \n"
                    "object to mark as interpolated contours or key contours.|"
                  "Prints a list of contours in the current object which \n"
                    "do not have a same-surface contours in the slice above or below.|"
                  "Prints some simple interpolation information about \n"
                    "the current object including the number of interpolated \n"
                    "contours; key contours and empty contours.|"
                  "Prints a table showing the number of key contours and \n"
                    "interpolated contours in the current object on EACH slice.|"
                  "Lets you select a range of contours within the current \n"
                    "object to mark as interpolated contours or key contours." );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  switch(plug.selectedAction)
  {
    case(0):      // mark contour range as interpolated
    {
      changeInterpContRange();
    } break;
    
    case(1):      // find isolated contours
    {
      findIsolatedContours();
    } break;
      
    case(2):      // print model interpolation info
    {
      printModelInterpInfo();
    } break;
    
    case(3):      // print number contours each slice
    {
      printObjectSliceInterpInfo();
    } break;
  }
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Allows user to change other plugin values/settings.

void Interpolator::moreSettings()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int interFractOverlap = plug.interFractOverlap * 100.0f;
  int interSepDistBetweenConts = plug.interSepDistBetweenConts;
  
  CustomDialog ds("More Settings", this);
  ds.addLabel   ( "--- CONTOUR CONNECTION: ---");
  ds.addSpinBox ( "max distance:",
                  0, 1000, &interSepDistBetweenConts, 1,
                  "The maximum distance in pixels two \n"
                  "contours must be to be connected \n"
                  "(if surface method is 'max dist')" );
  ds.addSpinBox ( "percent overlap:",
                  1, 100, &interFractOverlap, 5,
                  "The percentage out of 100 which contours \n"
                  "must overlap in X-Y to be connected \n"
                  "(if surface method is 'overlap')" );
  ds.addComboBox( "point resolve method:",
                  "four pt|"
                  "all convex",
                  &plug.ptResolveMethod,
                  "This is the method which will be used to find \n"
                  "a starting point for the 'line conservation' method \n"
                  "when the tiling method is set to 'length'. \n"
                  "The 'all convex' method is more thorough, but slighly slower." );
  
  ds.addLabel   ( "--- KEYBOARD: ---");
  //ds.addCheckBox( "disable [g]:", &plug.gDisable,
  //                "disables this shortcut for finding gaps and isolated contours \n"
  //                "so you can toggle ghosting (as normal)." );
  ds.addSpinBox ( "max gap size for [g]:",
                  0, 500, &plug.maxGapSize, 1,
                  "The maximum number of missing countours (in Z) \n"
                  "between two (same surface) contours to search for \n"
                  "when [g] is pressed." );
  ds.addSpinBox ( "min hole size for [h]:",
                  1, 500, &plug.minHoleSize, 1,
                  "The minimum number of (same surface) consecutive interpolated \n"
                  "contours (along Z) to search for when [h] is pressed." );
  
	ds.addCheckBox( "deselect contour after [Enter]",
                  &plug.deselectAfterEnter,
                  "After interpolation using [Enter] the selected contour \n"
                  "will be deselected (same results as pressing [n]), leaving \n"
                  "you ready to immediately draw another contour. \n"
                  "\nRECOMMENDED VALUE: on" );
  
	ds.addCheckBox( "hide surface settings",
								  &plug.hideSurfSettings,
								  "If true, will hide the surface settings area. \n"
								  "\nRECOMMENDED VALUE: on" );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
	
	plug.window->grpSurface->setVisible( !plug.hideSurfSettings );
  plug.interFractOverlap         = (float)interFractOverlap / 100.0f;
  plug.interSepDistBetweenConts  = interSepDistBetweenConts;
  
  ivwRedraw( plug.view );
}



//------------------------
//-- Method used for testing new routines.

void Interpolator::test()
{  
  //test_selector();
	//wprint("TEST EXECUTED\n");
}


//## BASIC METHODS TO CHANGE INTERPOLATIONDATA

//------------------------
//-- Change interType

void Interpolator::changeType( int value )  {
	plug.interType = value;	
}

//------------------------
//-- Change drawMode and set appropriate radio button

void Interpolator::changeTypeSelected( int newType) {
  plug.interType = newType;
  diaSetGroup(typeButtonGroup, plug.interType);
}

//------------------------
//-- Change zBridge

void Interpolator::changeZBridge( int value ) {
	plug.zBridge = value;	
}
//------------------------
//-- Change interLineTracker

void Interpolator::changeLineTracker()  {
	plug.interLineTracker = applyLineTrackerCheckbox->isChecked();
  MsgBox( "This option has not been implemented yet sorry" );
}

//------------------------
//-- Change tilingMethod

void Interpolator::changeTilingMethod( int value )  {
  if( value == TM_FEATURE_RECOG ) {
    MsgBox( "This option has not been implemented yet sorry" );
    tilingMethodCombo->setCurrentIndex( TM_CONSERVE_LENGTH );
    return;
	}
  plug.tilingMethod = value;	
}

//------------------------
//-- Change branchingMethod

void Interpolator::changeBranchingMethod( int value )	{
  if( value == BR_BRIDGE_GAPS || value == BR_PT_BELOW ) {
    MsgBox( "This option has not been implemented yet sorry" );
    branchMethodCombo->setCurrentIndex( BR_MERGE_CONTS );
    return;
  }
  plug.branchingMethod = value;	
}

//------------------------
//-- Change surfResolveMethod

void Interpolator::changeSurfaceMethod( int value ) {
	plug.surfResolveMethod = value;	
}

//------------------------
//-- Change interSepDistBetweenConts

void Interpolator::changeMinDist( int value ) {
	plug.interSepDistBetweenConts = value;	}

//------------------------
//-- Change interFractOverlap

void Interpolator::changeOverlap( int value ) {
	plug.interFractOverlap = (float)value / 100.0f;	
}


//------------------------
//-- Returns the type of surface resolving method which should be used.
//-- Typically this is the same value as "plug.surfResolveMethod",
//-- unless "SR_AUTO" is selected and then it depends on "plug.interType".

int Interpolator::getSurfaceResolveMethod()
{
	if( plug.surfResolveMethod == SR_AUTO )
	{
		if      ( plug.interType==INT_LINEAR    )		return (SR_WITHIN_MIN_DIST);
		else if ( plug.interType==INT_SPHERICAL )		return (SR_CENTER_OVERLAP);
		else																				return (SR_TOUCHING);
	}
	return ( plug.surfResolveMethod );
}

//------------------------
//-- Returns the type of tiling method which should be used.
//-- Typically this is the same value as "plug.tilingMethod",
//-- unless "TM_AUTO" is selected and then it depends on "plug.interType".

int Interpolator::getTilingMethod()
{
	if( plug.tilingMethod == TM_AUTO )
	{
		if      ( plug.interType==INT_LINEAR    )		return (TM_MIN_SA);
		else if ( plug.interType==INT_SPHERICAL )		return (TM_CONSERVE_LENGTH);
		if      ( plug.interType==INT_SMOOTH    )		return (TM_CONSERVE_LENGTH);
		else																				return (TM_MIN_SA);
	}
	return ( plug.tilingMethod );
}


//############################################################
//## PROTECTED SLOTS:


//------------------------
//-- Displays a (html) help page with information about the plugin

void Interpolator::helpPluginHelp()
{
  imodShowHelpPage("../plughelp/interpolator.html#TOP");
}


//------------------------
//-- Callback for the buttons at the bottom

void Interpolator::buttonPressed(int which)
{
  if      (which==0)
    close();
  else if (which==1)
		openUrl( "http://www.slashsegmentation.com/tools/imod/interpolator-plugin" );
	else if (which==2)
    helpPluginHelp();
}


//------------------------
//-- Window closing event handler - removes this pluging from the imod dialog manager

void Interpolator::closeEvent ( QCloseEvent * e )
{
	imodDialogManager.remove((QWidget *)plug.window);
	
  plug.window->saveSettings();
  
	plug.view = NULL;
	plug.window = NULL;
	e->accept();
}


//------------------------
//-- Key press event handler - closes on escape or passes on event to "ivwControlKey"

void Interpolator::keyPressEvent ( QKeyEvent * e )
{
	if (e->key() == Qt::Key_Escape)
		close();
	else
		ivwControlKey(0, e);
}

//------------------------
//-- Key release event hander - passes on event to "ivwControlKey"

void Interpolator::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}








//############################################################

//----------------------------------------------------------------------------
//
//          InterpolationEvent METHODS:
//
//----------------------------------------------------------------------------



//-------------------------------
//** CONSTRUCTORS:



  
  
//------------------------
//-- Resets all variables in the InterpolationEvent structure to defaults,
//-- clears the contour information vector and resizes/empies both z tables.

void InterpolationEvent::resetAll()
{
  obj      = NULL;
	sContIdx = -1;
	sContZ   = -1;
  
  conti.clear();
  
  if( !ztableKey.empty() )
    for(int i=0; i<(int)ztableKey.size(); i++ )
      ztableKey[i].idxs.clear();
  
  if( !ztableInt.empty() )
    for(int i=0; i<(int)ztableInt.size(); i++ )
      ztableInt[i].idxs.clear();
  
	closestAboveIdx = -1;
	closestBelowIdx = -1;
  
	aboveIdxs.clear();
	belowIdxs.clear();
}



//------------------------
//-- Is typically executed after a new (key) contour is added/modified by
//-- the user and we want to perform interpolation on it. This function
//-- first tries to find the nearest key contour above and below the
//-- new one (within a set range), and, depending on if any are found
//-- (and also the type of interpolation selected) the function will
//-- delete nearby interpolated contours in the way (above and below) and
//-- generate series of new interpolated contours.

void InterpolationEvent::
performInterpolationOnCont( Iobj *_obj, int _contIdx, int interpolationType )
{
  //## SETUP INTERPOLATION EVENT DATA (POINTERS, VECTORS, Z TABLES, ETC):
  resetAll();
  
	obj     = _obj;
	sContIdx = _contIdx;
	sContZ   = getZInt( getC(sContIdx) );
  
  regenerateContInfoVector();
  regenerateZTables();
  regenerateConnections();
  
  //## PERFORM INTERPOLATION:
  
	int numAdded = 0;
	int numFlagged = 0;
	int numDeleted = 0;
  
	numFlagged = deleteInterpolatedContsBetweenKeyContsAboveAndBelow();
  
	switch( interpolationType )
	{
		case (INT_NO_INTERPOLATE):
			break;
      
		case (INT_LINEAR):
			numAdded = interp_Linear( sContIdx, plug.zBridge );
			break;
			
		case (INT_SPHERICAL):
			interp_Spherical( sContIdx, plug.zBridge );
			break;
			
		case (INT_SMOOTH):
			interp_SmoothCrudeAll( sContIdx, plug.zBridge );
			break;
			
		case (INT_SMOOTH_PTS):
			interp_SmoothPointwise( sContIdx, plug.zBridge );
			break;
      
		default:
			wprint("This type of interpolation not implemented yet\n");
			break;
	}
	
	numDeleted = removeAllDeleteFlaggedContoursFromObj( obj );
	
	if(numDeleted < numFlagged)
		wprint( "ERROR - not all conts flagged for delete were deleted\n" );
	
	//wprint("interpolation cont %d, deleted %d, added %d\n",sContIdx,numDeleted,numAdded);
}


//------------------------
//-- Find the nearest key contour above and below the contour given,
//-- then deletes all the interpolated contours in between which appear to
//-- belong to the same surface

int InterpolationEvent::
deleteImmediatelyAdjacentInterpolatedConts( Iobj *_obj, int _contIdx )
{
  //## SETUP INTERPOLATION EVENT DATA (POINTERS, VECTORS, Z TABLES, ETC):
  resetAll();
  
	obj     = _obj;
	sContIdx = _contIdx;
	sContZ   = getZInt( getC(sContIdx) );
  
  regenerateContInfoVector();
  regenerateZTables();
  regenerateConnections();
  
  //## PERFORM DELETION:
  
  deleteInterpolatedContsBetweenKeyContsAboveAndBelow();
	return ( removeAllDeleteFlaggedContoursFromObj( obj ) );  
}


//------------------------
//-- Attempts to find a list of consequtive (same-surface) interpolated contours
//-- with greater than "minSpanSize" contours, then returns the index of the
//-- one in the middle. If none is found it returns -1.
//-- The search begins from the contour in "_obj" at "_contIdx".

int InterpolationEvent::
findMiddleNextLargeInterpolatedSpan( Iobj *_obj, int _contIdx, int minSpanSize )
{
  //## SETUP INTERPOLATION EVENT DATA (POINTERS, VECTORS, Z TABLES, ETC):
  resetAll();
  
	obj     = _obj;
	sContIdx = _contIdx;
	sContZ   = getZInt( getC(sContIdx) );
  
  regenerateContInfoVector();
  regenerateZTables();
  
  //## PERFORM DELETION:
  
  for(int i=0; i<(int)conti.size(); i++ )     // for each interpolated contour (contI):
  {
    int cIdxI = (i+sContIdx+1) % csize(obj);
    Icont *contI = getC(cIdxI);
    if( !isInterpolated(contI) || conti[cIdxI].checked || psize(contI)==0 )
      continue;
    
    if( cIdxI == 0 )
      wprint("Starting from beginning ...\n");
    
    int contZ = getCZ(cIdxI);
    
    //## COMPILE LIST OF CONNECTED INTERPOLATED CONTOURS ABOVE AND BELOW CONTI:
    
    vector<int> interpConts;            // list of connected, interpolated contours
    interpConts.resize(0);
    interpConts.push_back(cIdxI);
    conti[cIdxI].checked = 1;
    int j, cIdxPrev;
    
    for(int z=contZ+1; z<plug.zsize; z++)  // for each slice above current contour:
    {
      for(j=0; j<numIntContsAtZ(z); j++)  // for each key contour at this z:
      {
        int cIdxJ = idxIntContZ(z,j);
        if ( conti[cIdxJ].checked == 0
             && contoursSameSurf(cIdxJ,cIdxI) )	{ // if belongs to same surface:
          interpConts.push_back( cIdxJ );
          conti[cIdxJ].checked = 1;
          break;
        }
      }
      if(j==numIntContsAtZ(z))     // if no connected cont found at this z: break
        break;
    }
    for(int z=contZ-1; z>=0; z--)           // for each slice below current contour:
    {
      for(j=0; j<numIntContsAtZ(z); j++)  // for each key contour at this z:
      {
        int cIdxJ = idxIntContZ(z,j);
        if ( conti[cIdxJ].checked == 0
             && contoursSameSurf(cIdxJ,cIdxI) )	{ // if belongs to same surface:
          interpConts.insert( interpConts.begin(), cIdxJ );
          conti[cIdxJ].checked = 1;
          break;
        }
      }
      if(i==numIntContsAtZ(z))     // if no connected cont found at this z: break
        break;
    }
    
    int numInterpConts = (int)interpConts.size();    // number of connected/consequtive
                                                     // interpoalted contours found
    
    if( numInterpConts >= minSpanSize )
    {
      int middleMostContIdx = interpConts[ int(numInterpConts/2) ];
      wprint("Span of %d interpolated contours found\n", numInterpConts);
      return ( middleMostContIdx );
    }
  }
  
  return -1;
}


//------------------------
//-- Resizes and populates the contour information vector (conti),
//-- calculating the minimum bounding rectangle and other information
//-- for any contours which lie in the given Z range


void InterpolationEvent::
regenerateContInfoVector( int _minZLimit, int _maxZLimit, int startIdx )
{
  minZLimit = _minZLimit;
  maxZLimit = _maxZLimit;
  
  //## POPULATE CONTOURINFO VECTOR:
  
  conti.resize( csize(obj) );
  for(int c=startIdx; c<csize(obj); c++ )
  {
    //conti[c].resetAll();
    
    Icont *cont = getC(c);
    conti[c].idx = c;
    conti[c].z   = getZInt(cont);
    
    //if( conti[c].z < minZLimit || conti[c].z > maxZLimit )
    //  continue;
    
    cont_getMBR( cont, &conti[c].ll, &conti[c].ur );
    conti[c].centerPt = line_getPtHalfwayBetween( &conti[c].ll, &conti[c].ur );
    conti[c].loaded = true;
  }
}


//------------------------
//-- Regenerates the Z table for key contours and interpolated contours,
//-- which list the index of contours (within the object) on each z slice.

void InterpolationEvent::regenerateZTables(  )
{
  //## POPULATE Z TABLES:
  
  ztableKey.resize( plug.zsize );
  ztableInt.resize( plug.zsize );
  
  for(int c=0; c<csize(obj); c++ )
  {
    Icont *cont = getC(c);
    
    if( isEmpty(cont) )
      continue;
    
    int z = getZInt(cont);
    if( z<0 || z>=plug.zsize )
      continue;
    
    if( isInterpolated(cont) )
      ztableInt[z].idxs.push_back(c);
    else
      ztableKey[z].idxs.push_back(c);
  }
}

//------------------------
//-- Redetermines the closest contour(s) above and below the contour
//-- currently being interpolated.
//-- Input:  'sContIdx'
//-- Output: 'aboveIdxs', 'belowIdxs', 'closestAboveIdx', 'closestBelowIdx'

void InterpolationEvent::regenerateConnections(  )
{
  //## FIND CLOSEST KEY CONTOURS (CLOSEST IN Z) WHICH SEEM TO BELONG TO THE
  //## SAME SURFACE ABOVE AND BELOW THE BASE CONTOUR:
  
  aboveIdxs = findIdxsNearestKeyConts(sContIdx,true,plug.zBridge);
  belowIdxs = findIdxsNearestKeyConts(sContIdx,false,plug.zBridge);
  
  //## FIND CLOSEST KEY CONTOURS IN X & Y FROM THE LISTS ABOVE:
  
  closestAboveIdx = findIdxClosestKeyContInList(sContIdx,aboveIdxs);
  closestBelowIdx = findIdxClosestKeyContInList(sContIdx,belowIdxs);
}


//------------------------
//-- Flags interpolated contours which are between the start contour (sContIdx)
//-- and the closest key contour above and below (and appear to be in the same
//-- surface) for deletion.

int InterpolationEvent::deleteInterpolatedContsBetweenKeyContsAboveAndBelow()
{
		int deleteRangeMaxZ = isKeyContAbove() ? getCZ(closestAboveIdx) : sContZ;
		int deleteRangeMinZ = isKeyContBelow() ? getCZ(closestBelowIdx) : sContZ;
		return deleteAllSameSurfInterpContsInZRange(deleteRangeMinZ,deleteRangeMaxZ,sContIdx);
}









//-------------------------------
//** SURFACE RESOLVING METHODS:





//------------------------
//-- Determines if two contours (appear to) belong to the same surface.
//-- It (typically) does this by determining the amount by which they overlap.

bool InterpolationEvent::
contoursSameSurf( int c1Idx, int c2Idx )
{  
  Icont *cont1 = getC(c1Idx);
  Icont *cont2 = getC(c2Idx);
  
  bool mbrsTouch = mbr_doBBoxesOverlap2D( getLL(c1Idx), getUR(c1Idx),
                                          getLL(c2Idx), getUR(c2Idx) );
  
	int surfMethod = plug.window->getSurfaceResolveMethod();
	
	switch (surfMethod)
	{
		case( SR_TOUCHING ):
    {
      return ( mbrsTouch && cont_doContsTouch(cont1,cont2) );
    }
			break;
      
		case( SR_CENTER_OVERLAP ):
    {
      if( !mbrsTouch )
        return false;
      return (   imodPointInsideCont(cont1,getCenterPt(c2Idx))
              || imodPointInsideCont(cont2,getCenterPt(c1Idx)) );
    }
			break;
      
    case( SR_WITHIN_MIN_DIST ):
    {
      float distBetweenMBRs = mbr_distBetweenBBoxes2D( getLL(c1Idx), getUR(c1Idx),
                                                       getLL(c2Idx), getUR(c2Idx) );
      if( distBetweenMBRs > plug.interSepDistBetweenConts )
        return false;
      float minDist = cont_minDistBetweenContPts2D(cont1, cont2, true);
      return ( minDist <= plug.interSepDistBetweenConts );
    }
			break;
      
		case( SR_FRACTION_OVERLAP ):
    {
      if( !mbrsTouch )
        return false;
      
      float frac1, fract2;
      Icont *cs1 = imodel_contour_scan( cont1 );
      Icont *cs2 = imodel_contour_scan( cont2 );
      imodel_overlap_fractions( &cs1,*getLL(c1Idx),*getUR(c1Idx),
                                &cs2,*getLL(c2Idx),*getUR(c2Idx),
                                &frac1,&fract2 );
      imodContourDelete( cs1 );
      imodContourDelete( cs2 );
      
      float fractionOverlap = max( frac1, fract2 );
      return ( fractionOverlap >= plug.interFractOverlap );
    }
			break;
			
    case( SR_MBR_TOUCH ):
    {
      return (mbrsTouch);
    }
			break;
      
      
			
		default:
			return (false);
	}
}


//------------------------
//-- Determines if two contours (appear to) belong to the same surface.
//-- It (typically) does this by determining the amount by which they overlap.

bool InterpolationEvent::
contoursSameSurfSlow( Icont *cont1, Icont *cont2 )
{  
	Ipoint cont1ll, cont1ur;
	Ipoint cont2ll, cont2ur;
	
	imodContourGetBBox( cont1, &cont1ll, &cont1ur );
	imodContourGetBBox( cont2, &cont2ll, &cont2ur );
	
	Ipoint centerMBRCont1 = line_getPtHalfwayBetween( &cont1ll, &cont1ur );
	Ipoint centerMBRCont2 = line_getPtHalfwayBetween( &cont2ll, &cont2ur );
  
  bool mbrsTouch = mbr_doBBoxesOverlap2D( &cont1ll, &cont1ur,
                                          &cont2ll, &cont2ur );
  
	
	int surfMethod = plug.window->getSurfaceResolveMethod();
	
	switch (surfMethod)
	{
		case( SR_TOUCHING ):
    {
      return ( mbrsTouch && cont_doContsTouch(cont1,cont2) );
    }
			break;
      
		case( SR_CENTER_OVERLAP ):
    {
      if( !mbrsTouch )
        return false;
      return (   imodPointInsideCont(cont1,&centerMBRCont2)
                 || imodPointInsideCont(cont2,&centerMBRCont1) );
    }
			break;
      
    case( SR_WITHIN_MIN_DIST ):
    {
      float distBetweenMBRs = mbr_distBetweenBBoxes2D( &cont1ll, &cont1ur,
                                                       &cont2ll, &cont2ur );
      if( distBetweenMBRs > plug.interSepDistBetweenConts )
        return false;
      float minDist = cont_minDistBetweenContPts2D(cont1, cont2, true);
      return ( minDist <= plug.interSepDistBetweenConts );
    }
			break;
      
		case( SR_FRACTION_OVERLAP ):
    {
      if( !mbrsTouch )
        return false;
      
      float frac1, fract2;
      Icont *cs1 = imodel_contour_scan( cont1 );
      Icont *cs2 = imodel_contour_scan( cont2 );
      imodel_overlap_fractions( &cs1,cont1ll,cont1ur,
                                &cs2,cont2ll,cont2ur,
                                &frac1,&fract2 );
      imodContourDelete( cs1 );
      imodContourDelete( cs2 );
      
      float fractionOverlap = max( frac1, fract2 );
      return ( fractionOverlap >= plug.interFractOverlap );
    }
			break;
			
    case( SR_MBR_TOUCH ):
    {
      return (mbrsTouch);
    }
			break;
      
      
			
		default:
			return (false);
	}
}

//------------------------
//-- Takes a base contour and finds/returns the indexes of the nearest key 
//-- contours (nearest in z) which (seem to) belong to the same surface and 
//-- are within a specified range of z slices relative to the base contours).
//-- NOTE: Even if "minZDist" is 0, surfaces on the SAME z slice are skipped.
//-- NOTE: By changing "maxDistAway" to "minZRelativeSlice" and using a
//--       "maxZVal" and "zDistFromBase" integers can easily change this function
//--       to search both ABOVE and BELOW the base contour - but haven't found
//--       a need yet.

vector<int> InterpolationEvent::
findIdxsNearestKeyConts( int cbIdx, bool above, int maxZDist, int minZDist  )
{
	if( minZDist < 0 || maxZDist < 0 )	{   // should never happen
		wprint( "ERROR/MISUSE: findIdxsNearestKeyConts()" );
	}
	
  vector<int> closestContIdxs;		// stores indexes of closest (same-surface) conts found
  int baseContZ = getCZ( cbIdx );
  int lastZLists = (int)ztableKey.size() - 1;
  
  if(above)
  {
    int minZ  = MAX( baseContZ + minZDist, 0 );
    int maxZ  = MIN( baseContZ + maxZDist, lastZLists );
    
    for(int z=minZ; z<=maxZ; z++)        // for each z list in range:
    {
      for(int i=0; i<numKeyContsAtZ(z); i++)  // for each key contour at this z:
      {
        int cidx = idxKeyContZ(z,i);
        if ( contoursSameSurf(cidx,cbIdx) )	 // if belongs to same surface:
          closestContIdxs.push_back( cidx );			// add it to list of "closest conts"
      }
      if( closestContIdxs.size() > 0 )    // if matching contours were found:
        break;                              // return them!
    }
  }
  else 
  {
    int minZ  = MIN( baseContZ - minZDist, lastZLists );
    int maxZ  = MAX( baseContZ - maxZDist, 0 );
    
    for(int z=minZ; z>=maxZ; z--)       // for each z list in range:
    {
      for(int i=0; i<numKeyContsAtZ(z); i++)  // for each key contour at this z:
      {
        int cidx = idxKeyContZ(z,i);
        if ( contoursSameSurf(cidx,cbIdx) )	 // if belongs to same surface:
          closestContIdxs.push_back( cidx );			// add it to list of "closest conts"
      }
      if( closestContIdxs.size() > 0 )    // if matching contours were found:
        break;                              // return them!
    }
  }
  
  return closestContIdxs;
}



//------------------------
//-- Takes a base contour and finds the nearest key contour based on centroid.
//-- Returns -1 if vector is empty.

int InterpolationEvent::
findIdxClosestKeyContInList( int cbIdx, vector<int> conts )
{
	if(conts.empty())             // if no contours were found: return -1
  {		
		return -1;
	}
	else if (conts.size() == 1)   // if one closest contour was found: return it's index
  {	
		return conts[0];
	}
	else                    // else (if multiple contours were found on the same z slice): 
  {                       // find & return the one has the closest centroid.
    
		float closestDistToCentroid = FLOAT_MAX;
		int   closestContIdx = 0;
		
		for(int i=0; i<int(conts.size()); i++)
    {
			float distToThisCentroid = imodel_point_dist( getCenterPt( conts[i] ),
                                                    getCenterPt( cbIdx) );
			if ( distToThisCentroid < closestDistToCentroid )
			{
				closestDistToCentroid = distToThisCentroid;
				closestContIdx = conts[i];
			}
		}
		return closestContIdx;
	}
}


//------------------------
//-- Takes a base contour and finds the nearest key contour above or below it, within
//-- a specified number of slices, which (seems to) belong to the same surface.
//-- If no contour matches this descritpion -1 is returned.
//-- If multiple overlapping contours on the same Z slices are found, the one with
//-- the CLOSEST centroid to the base contour is returned.

int InterpolationEvent::
findIdxNearestKeyCont( int cbIdx, int maxDist, bool above )
{
	vector<int> closestConts = findIdxsNearestKeyConts(cbIdx,above,maxDist);
	return findIdxClosestKeyContInList( cbIdx, closestConts );
}

//------------------------
//-- Takes a base contour and finds/returns the indexes of all key 
//-- contours which appear to belong to the same surface, by searching
//-- iteratively down and then up. This list will be ordered from lowest
//-- to highest contour (base contour included) HOWEVER ignores any branching:
//-- if the contour splits into two contours, only the nearest
//-- will be considered.

vector<int> InterpolationEvent::
findIdxsAllKeyContsNoBranching( int idxBaseCont, int maxDist )
{
  vector<int> contsInSurface;
	
	//## SEARCH DOWN:
	int idxCurrCont = idxBaseCont;
	while (true) {
		int idxNextCont = findIdxNearestKeyCont( idxCurrCont, maxDist, false );		
                                                // see if there is a contour below this
		if ( idxNextCont == -1 )                    // if no contour if found: break
			break;
    
		contsInSurface.insert( contsInSurface.begin(), idxNextCont );
                                      // add newly found contour to start of list
		idxCurrCont = idxNextCont;        // make this the current contour - (will
                                      // check if one above next iteration)
	}
	
	contsInSurface.push_back( idxBaseCont );     // add base contour's idx (in the middle)
	
	
	//## SEARCH UP:
	idxCurrCont = idxBaseCont;
	while (true) {
		int idxNextCont = findIdxNearestKeyCont( idxCurrCont, maxDist, true );	
                                              // see if there is a contour above this
		if ( idxNextCont == -1 )									// if no contour if found: break
			break;
    
		contsInSurface.push_back(idxNextCont);		// add newly found contour to end of list
		idxCurrCont = idxNextCont;								// make this the current contour - (will
                                              // check if one above next iteration)
	}
	
	return contsInSurface;
}

//------------------------
//-- Deletes any interpolated contours (in the model) which are in the given range
//-- of slices and appears to belong to the same surface as the contour provided.

int InterpolationEvent::
deleteAllSameSurfInterpContsInZRange( int minZ, int maxZ, int cbIdx )
{
  int numDeleted = 0;
	
  minZ = MAX( minZ, 0 );
  maxZ = MIN( maxZ,plug.zsize-1 );
  
	for(int z=minZ; z<=maxZ; z++)
	{
    for(int i=0; i<numIntContsAtZ(z); i++)
    {
      int cidx = idxIntContZ(z,i);
      if ( contoursSameSurf(cidx,cbIdx) )	 // if belongs to same surface:
      {
        setDeleteFlag( getC(cidx), 1 );
        numDeleted++;
      }
    }
	}
	return numDeleted;
}

//------------------------
//-- Deletes all adjacent interpolated contours (in the model) above and below 
//-- the given contour. Unlike "deleteAllSameSurfInterpContsInZRange",
//-- this works by checking each adjacent slice in order, finding a matching 
//-- interpolated contour, then using this to check the next slice until no more 
//-- matching interpolated contour is found... (no range is specified).

int InterpolationEvent::
deleteAllSameSurfInterpContsEitherSideOfCont( int cbIdx )
{
  int numDeleted = 0;
	
	//## SEARCH DOWN AND FLAG MATCHING INTERPOLATED CONTOURS FOR DELETION:
	
  int prevFoundIdx = cbIdx;              // countour to check for overlap
  bool matchFound = true;
  for (int z=getCZ(cbIdx)-1; z>=0 && (matchFound); z--)
  {
    matchFound = false;
    
    for(int i=0; i<numIntContsAtZ(z); i++)
    {
      int cidx = idxIntContZ(z,i);
      if( contoursSameSurf(cidx, prevFoundIdx) )
      {
        prevFoundIdx = cidx;                  // set this to "previously found contour"
        matchFound   = true;                  // allows another iteration of the loop
        setDeleteFlag( getC(cidx), 1 );
        numDeleted++;
        //break;
      }
    }
  }
  
  //## SEARCH UP AND FLAG MATCHING INTERPOLATED CONTOURS FOR DELETION:
  
  prevFoundIdx = cbIdx;
  matchFound = true;
  for (int z=getCZ(cbIdx)+1; z<plug.zsize && (matchFound); z++)
  {
    matchFound = false;
    
    for(int i=0; i<numIntContsAtZ(z); i++)
    {
      int cidx = idxIntContZ(z,i);
      if( contoursSameSurf(cidx, prevFoundIdx) )
      {
        prevFoundIdx = cidx;                  // set this to "previously found contour"
        matchFound   = true;                  // allows another iteration of the loop
        setDeleteFlag( getC(cidx), 1 );
        numDeleted++;
        //break;
      }
    }
  }
  
  return numDeleted;
}



//-------------------------------
//** SURFACE RESOLVING METHODS:




//------------------------
//-- Returns the two points (one from each contour) which best correspond
//-- by applying the point corrspondance algorithm specified by plug.ptResolveMethod

void InterpolationEvent::
findCoorrespondingPts( Icont *cont1, Icont *cont2,
                       int *idxCont1, int *idxCont2 )
{
  if( plug.ptResolveMethod == PT_FOUR_PTS )
  {
    findCoorrespondingPts_FourPtMBR    (cont1,cont2,idxCont1,idxCont2);
  }
  else
  {
    findCoorrespondingPts_AllConvexPts (cont1,cont2,idxCont1,idxCont2);
  }
}

//------------------------
//-- Takes two contours and tries to find two points which are MOST
//-- likely to correspond. In this (relatively processing inexpensive) 
//-- function it finds the (convex) points in both contours which are 
//-- furthest to left, right, top and bottom. It then works out which pair
//-- of points form the closest angles with the center of their the
//-- minimum bounding rectangle around their contour - and returns these
//-- indexes as "coorespoding points".


void InterpolationEvent::
findCoorrespondingPts_FourPtMBR( Icont *cont1, Icont *cont2,
                                 int *idxCont1, int *idxCont2 )
{
	if( psize( cont1 ) < 3 || psize( cont2 ) < 3 ) {
		idxCont1 = 0;
		idxCont2 = 0;
		return;
	}
	
//## GENERATE A BOUNDING BOX AND DETERMINE THE CENTER OF THE BOX FOR BOTH CONTOURS:
	
	Ipoint cont1ll, cont1ur;
	Ipoint cont2ll, cont2ur;
	
	imodContourGetBBox( cont1, &cont1ll, &cont1ur );
	imodContourGetBBox( cont2, &cont2ll, &cont2ur );
	
	Ipoint centerMBRCont1 = line_getPtHalfwayBetween( &cont1ll, &cont1ur );
	Ipoint centerMBRCont2 = line_getPtHalfwayBetween( &cont2ll, &cont2ur );
            // represents the center of the minimum bounding box of cont1 and cont2
	
//## IDENTIFY WHICH POINTS (INDEXES) CONTAIN THE MIN AND MAX VALUES
//## ALONG X AND Y (CALL THESE "EXTREME POINTS") FOR BOTH CONTOURS:
	
	Ipoint *cont1pts = imodContourGetPoints( cont1 );
	Ipoint *cont2pts = imodContourGetPoints( cont2 );
	
	int minXCont1, maxXCont1, minYCont1, maxYCont1;
                // stores the INDEX of point contain the minimum and maximum
                // values for both X and Y in cont1
	
	for (int i=0; i< psize(cont1);i++) {
		if      ( cont1pts[i].x == cont1ll.x )
			minXCont1 = i;
		else if ( cont1pts[i].x == cont1ur.x )
			maxXCont1 = i;
			
		if      ( cont1pts[i].y == cont1ll.y )
			minYCont1 = i;
		else if ( cont1pts[i].y == cont1ur.y )
			maxYCont1 = i;
	}
	
	int minXCont2, maxXCont2, minYCont2, maxYCont2;
                  // stores the INDEX of point contain the minimum and maximum
                  // values for both X and Y in cont1
	
	for (int i=0; i<psize( cont2 ); i++) {
		if      ( cont2pts[i].x == cont2ll.x )
			minXCont2 = i;
		else if ( cont2pts[i].x == cont2ur.x )
			maxXCont2 = i;
			
		if      ( cont2pts[i].y == cont2ll.y )
			minYCont2 = i;
		else if ( cont2pts[i].y == cont2ur.y )
			maxYCont2 = i;
	}
	
  
//## CALCULATE THE DIFFERENCE IN ANGLE BETWEEN THE LINES EXENDING FROM
//## THE CENTER OF BOTH CONTOURS TO EXTREME POINTS
	
	float angleDiffMinX = ABS(line_getAngle2DPos(&cont1pts[minXCont1], &centerMBRCont1)
                          - line_getAngle2DPos(&cont2pts[minXCont2], &centerMBRCont2) );
	float angleDiffMaxX = ABS(line_getAngle2DPos(&cont1pts[maxXCont1], &centerMBRCont1)
                          - line_getAngle2DPos(&cont2pts[maxXCont2], &centerMBRCont2) );
	float angleDiffMinY = ABS(line_getAngle2DPos(&cont1pts[minYCont1], &centerMBRCont1)
                          - line_getAngle2DPos(&cont2pts[minYCont2], &centerMBRCont2) );
	float angleDiffMaxY = ABS(line_getAngle2DPos(&cont1pts[maxYCont1], &centerMBRCont1)
                          - line_getAngle2DPos(&cont2pts[maxYCont2], &centerMBRCont2) );
	
	
//## DETERMINE WHICH EXTREME POINTS MATCH THE BEST (THOSE WITH THE SMALLEST
//## DIFFERENCE IN ANGLE) AND RETURN THESE "COORESPONDING" POINTS
	
	float lowestAngleDiff = MIN(angleDiffMaxY,MIN(angleDiffMinY,
                                                MIN(angleDiffMaxX,angleDiffMinX)));
	
	if		(angleDiffMinX == lowestAngleDiff) {
		*idxCont1 = minXCont1;
		*idxCont2 = minXCont2;
	}
	else if	(angleDiffMaxX == lowestAngleDiff) {
		*idxCont1 = maxXCont1;
		*idxCont2 = maxXCont2;
	}
	else if	(angleDiffMinY == lowestAngleDiff) {
		*idxCont1 = minYCont1;
		*idxCont2 = minYCont2;
	}
	else {
		*idxCont1 = maxYCont1;
		*idxCont2 = maxYCont2;
	}
}




//------------------------
//-- Takes two clockwise contours and tries to find two points which are MOST
//-- likely to correspond. For each contour it first finds all the convex
//-- points and determines the angle they make with the center of the MBR.
//-- The list of points indexes and angles are then compared and the indexes
//-- of the two points (one from each contour) with the closest angle
//-- are returned as the as best "coorespoding points".

void InterpolationEvent::
findCoorrespondingPts_AllConvexPts( Icont *cont1, Icont *cont2,
                                          int *idxCont1, int *idxCont2 )
{
	if( psize( cont1 ) < 3 || psize( cont2 ) < 3 ) {
		idxCont1 = 0;
		idxCont2 = 0;
		return;
	}
  
  //## GENERATE A MINIMUM BOUNDING BOX (MBR) AROUND EACH CONTOUR AND DETERMINE IT'S SIZE:
	
	Ipoint cont1ll, cont1ur;
	Ipoint cont2ll, cont2ur;
	
	imodContourGetBBox( cont1, &cont1ll, &cont1ur );
	imodContourGetBBox( cont2, &cont2ll, &cont2ur );
	
  
  float cont1MBRWidth  = cont1ur.x - cont1ll.x;
  float cont1MBRHeight = cont1ur.y - cont1ll.y;
  
  float cont2MBRWidth  = cont2ur.x - cont2ll.x;
  float cont2MBRHeight = cont2ur.y - cont2ll.y;
  
  if( cont1MBRWidth==0 || cont1MBRHeight==0 || cont2MBRWidth==0 || cont2MBRHeight==0 )
  {
    wprint("\aContour with zero area was found");
    findCoorrespondingPts_FourPtMBR( cont1, cont2, idxCont1, idxCont2 );
    return;
  }
  
  
  //## CREATE A COPY OF THE CONTOURS AND SET THE Z VALUE OF EACH
  //## POINT TO EQUAL IT'S ORIGINAL INDEX (SO WE DON'T LOSE IT WHEN WE MODIFY):
  
  Icont *c1 = imodContourDup(cont1);
  Icont *c2 = imodContourDup(cont2);
  
  for( int p=0; p<psize(c1); p++ )
    getPt(c1,p)->z = p;
  for( int p=0; p<psize(c2); p++ )
    getPt(c2,p)->z = p;
  
  
  //## SCALE THE CONTOUR WITH THE SMALLER MINIMUM BOUNDING RECTANGLE (MBR) 
  //## SO IT'S MBR BECOMES THE SAME SIZE AS THE LARGER ONE
  
  float cont1MBRArea   = cont1MBRWidth * cont1MBRHeight;
  float cont2MBRArea   = cont2MBRWidth * cont2MBRHeight;
    
  if( cont1MBRWidth == cont2MBRWidth &&     // if both MBRs are same dimensions:
      cont1MBRHeight == cont2MBRHeight )    //    do nothing
  {
    ;
  }
  if( cont2MBRArea > cont1MBRArea )   // if cont2's MBR is bigger: scale up cont1
  {
    float cont1ScaleX = fDiv( cont2MBRWidth,  cont1MBRWidth );
    float cont1ScaleY = fDiv( cont2MBRHeight, cont1MBRHeight );
    cont_scaleAboutPtXY( c1, &cont1ll, cont1ScaleX, cont1ScaleY );
    imodContourGetBBox( c1, &cont1ll, &cont1ur );
    
    cont1MBRWidth  = cont1ur.x - cont1ll.x;
    cont1MBRHeight = cont1ur.y - cont1ll.y;
  }
  else                                // else (if cont1's MBR is bigger): scale up cont2
  {
    float cont2ScaleX = fDiv( cont1MBRWidth,  cont2MBRWidth );
    float cont2ScaleY = fDiv( cont1MBRHeight, cont2MBRHeight );
    cont_scaleAboutPtXY( c2, &cont2ll, cont2ScaleX, cont2ScaleY );
    imodContourGetBBox( c2, &cont2ll, &cont2ur );
    
    cont2MBRWidth  = cont2ur.x - cont2ll.x;
    cont2MBRHeight = cont2ur.y - cont2ll.y;
  }  
  
	Ipoint centerMBRCont1 = line_getPtHalfwayBetween( &cont1ll, &cont1ur );
	Ipoint centerMBRCont2 = line_getPtHalfwayBetween( &cont2ll, &cont2ur );
  

  //## MAKE BOTH CONTOURS CLOCKWISE AND CONVEX (REMOVE ALL CONCAVE POINTS)
  
	//imodContourMakeDirection(c1, IMOD_CONTOUR_CLOCKWISE);     //|- make sure conts 
	//imodContourMakeDirection(c2, IMOD_CONTOUR_CLOCKWISE);     //|  are clockwise
  
  cont_makeConvex(c1);
  cont_makeConvex(c2);
  
  
  //## FOR EACH (CONVEX) POINT IN BOTH CONTOURS, CALCULATE THE ANGLE IT MAKES
  //## WITH THE CENTER OF THE CONTOUR'S MBR AND POPULATE THIS AS IT'S X VALUE
  //## AND DETERMINE THE POINT WITH THE GREATEST ANGLE:
  
  int   c1IdxMaxAngle      = -1;
  float maxAngleToCenterC1 = FLOAT_MIN_POS;
  for( int p=0; p<psize(c1); p++ ) {
    float angleTowardsCenter = line_getAngle2DPos( getPt(c1,p), &centerMBRCont1 );
    getPt(c1,p)->x = angleTowardsCenter;
    if( angleTowardsCenter > maxAngleToCenterC1 )
    {
      maxAngleToCenterC1 = angleTowardsCenter;
      c1IdxMaxAngle = p;
    }
  }
  
  int   c2IdxMaxAngle      = -1;
  float maxAngleToCenterC2 = FLOAT_MIN_POS;
  for( int p=0; p<psize(c2); p++ ) {
    float angleTowardsCenter = line_getAngle2DPos( getPt(c2,p), &centerMBRCont2 );
    getPt(c2,p)->x = angleTowardsCenter;
    if( angleTowardsCenter > maxAngleToCenterC2 )
    {
      maxAngleToCenterC2 = angleTowardsCenter;
      c2IdxMaxAngle = p;
    }
  }
  
  
  //## REORDER THE POINTS IN BOTH CONTOURS TO START FROM THE POINT
  //## WITH THE HIGHEST ANGLE AND DUPLICATE THIS ANGLE AT THE END:
  
  cont_reorderPtsToStartAtIdx( c1, c1IdxMaxAngle );
  cont_reorderPtsToStartAtIdx( c2, c2IdxMaxAngle );
  
  imodPointAppendXYZ( c1, getFirstPt(c1)->x-360.0f, 0, getFirstPt(c1)->z );
  imodPointAppendXYZ( c2, getFirstPt(c2)->x-360.0f, 0, getFirstPt(c2)->z );
  
  
  //## TRAVERSE FROM THE NEW START OF BOTH CONTOURS TO THE END WITH DESCENDING ANGLE
  //## VALUES TO DETERMINE THE TWO POINTS (ONE FROM EACH CONTOUR) WITH THE CLOSEST 
  //## ANGLE TO THEIR MBR:
  
  int cont1IdxMinDiff = -1;
  int cont2IdxMinDiff = -1;

  float minAngleDiff = FLOAT_MAX;
  
  int p1 = 0;   // the current index in c1
  int p2 = 0;   // the current index in c2
  
  
  while( p1 < psize(c1) && p2 < psize(c2) )
  {
    float currAngC1 = getPt(c1,p1)->x;
    float currAngC2 = getPt(c2,p2)->x;
    float angleDiffBetweenPts = ABS( currAngC1 - currAngC2 );
    
    if( angleDiffBetweenPts < minAngleDiff )
    {
      minAngleDiff    = angleDiffBetweenPts;
      cont1IdxMinDiff = getPt(c1,p1)->z;
      cont2IdxMinDiff = getPt(c2,p2)->z;
    }
    
    float nextAngInC1 = getPt(c1,p1+1)->x;
    float nextAngInC2 = getPt(c2,p2+1)->x;
    
    if( nextAngInC1 > nextAngInC2 )   // if next c1 is greater: increpment p1
      p1++;
    else                              // if next c2 is greater: increpment p2
      p2++;
  }
  
  *idxCont1 = cont1IdxMinDiff;
  *idxCont2 = cont2IdxMinDiff;
  
  imodContourDelete(c1);
  imodContourDelete(c2);
}





//------------------------
//-- Takes a base contour which branches into two contours and
//-- brakes the base contour into two new "branch" contours which
//-- roughly match to the branched contours, so they can be directly
//-- interpolated/connected together.
//--    _____         _____
//--   /     \       /    _|   - contBranch1 & contBranch2
//--   \____/        \___/
//--     ______   _____
//--    /      \_/     \       - baseCont
//--    \______________/
//--     ______   _____
//--    /      \ /     \       - newBranch1 & newBranch2
//--    \_______|______/

void InterpolationEvent::
breakBaseContIntoTwoForBranching( Icont *baseCont,
                                       Icont *contBranch1, Icont *contBranch2,
                                       Icont *newBranch1, Icont *newBranch2 )
{
	Ipoint centerB1;	
  cont_getCenterOfMBR(contBranch1, &centerB1);
	Ipoint centerB2;	
  cont_getCenterOfMBR(contBranch2, &centerB1);
	
	float angleSeperatingLine = line_getAngle2DPos(&centerB1,&centerB2) + 90;
	
	int dummyInt;
	float minDistB1ToCent2;   //|- stores the minimum distance from both branch contour
  float minDistB2ToCent1;		//|  to the centroid of the other one
  
	Ipoint closestPtBranch1;  //|- stores the closest point in both branch contour
  Ipoint closestPtBranch2;	//|  to the centroid of the other one
	
	cont_findClosestPtInContToGivenPt( &centerB2, contBranch1,
                                     &minDistB1ToCent2, &closestPtBranch1, &dummyInt );
                // gets the closest point (& distance) in
                // branch 1 contour to the centroid of branch 2
	cont_findClosestPtInContToGivenPt( &centerB1, contBranch2,
                                     &minDistB2ToCent1, &closestPtBranch2, &dummyInt );
                // gets the closest point (& distance) in branch 2
                // contour to the centroid of branch 1
	
	float distBetweenCentroids = line_distBetweenPts2D( &centerB1, &centerB2 );
	float gapBetweenClosestPts = (minDistB1ToCent2 + minDistB2ToCent1) 
                                - distBetweenCentroids;
	float fractBetweenCentroidsGapMiddle = 
                  (minDistB2ToCent1 - (gapBetweenClosestPts/2)) / distBetweenCentroids;
	Ipoint ptMiddleGap = line_findPtFractBetweenPts( &centerB1, &centerB2, 
                                                  fractBetweenCentroidsGapMiddle );
  
	bool isMiddleGapInsideBase = imodPointInsideCont( baseCont, &ptMiddleGap );
}




//********************************

//## DIFFERENT METHODS FOR TILING TWO KEY CONTOURS FOR INTERPOLATION:

//  The follow functions are different methods to prepare two key
//  contours for interpolation. These functions take two key contours, 
//  creates new versions of these such that they both have the same
//  number of points, and these points correspond one-to-one, ready for
//  point interpolation.





//------------------------
//-- Takes two key contours (which it can close if necessary), and
//-- creates new versions of each contour, which have an equal number of
//-- points in corresponding order, so that each point can be easily interpolated.
//-- 
//-- The algorithm used to determine connecting points depends
//-- on the value of 'plug.tilingMethod'


void InterpolationEvent::
modifyKeyContsForInterp( int clIdx, int cuIdx,
                         Icont *contLNew, Icont *contUNew,
                         bool closeContours,
                         bool findCoorrespondingPtsAndMakeClockwise )
{
  //## APPLY USER SELECTED TILING ALGORITHM:
  
	int tilingMethod = plug.window->getTilingMethod();
	
	switch(tilingMethod)
  {
    case(TM_CONSERVE_LENGTH):
    {
      modifyKeyContsForInterp_LineConservation( getC(clIdx), getC(cuIdx),
                                                contLNew, contUNew, closeContours,
                                                findCoorrespondingPtsAndMakeClockwise );
			break;
		}
		
    case(TM_MIN_SA):
    {
      modifyKeyContsForInterp_MinimumAreaCost( clIdx, cuIdx, contLNew, contUNew );
			break;
    }
      
  }
}


//------------------------
//-- Takes two key contours (which it can close if necessary), and
//-- creates new versions which are ready for one-to-one point interpolation.
//-- It does this adding points to each, such that coorresponding points
//-- on each contour are the same proportional distance along the
//-- length of their contour.
//-- NOTE: This method achieves better effective interpolation
//-- than tiling (mesh fitting) algorithms which do not add extra
//-- points, but instead connect existing ones.

void InterpolationEvent::
modifyKeyContsForInterp_LineConservation( Icont *contKeyL, Icont *contKeyU,
                                          Icont *contLNew, Icont *contUNew,
                                          bool closeContours,
                                          bool findCoorrespondingPtsAndMakeClockwise )
{
	imodContourDefault( contLNew );
	imodContourDefault( contUNew );
	
	if( isEmpty(contKeyL) || isEmpty(contKeyU) || getZ(contKeyL) >= getZ(contKeyU) ) {
		wprint( "\aERROR: modifyKeyContsForInterp_LineConservation()\n" );
		return;
	}
	
  //## MODIFY KEY CONTOURS SUCH THAT FIRST AND LAST POINTS CORRESPOND
	
	Icont *contL = imodContourDup(contKeyL);	//|- stores modified version of contour where
	Icont *contU = imodContourDup(contKeyU);  //|  first and last points in each correspond
	
	imodContourUnique( contL );		// |-- removes any surpurfluous points from contours
	imodContourUnique( contU );		// |
	
	if( psize(contL) == 1 ) {
    imodContourDelete(contUNew);
		contUNew = imodContourDup( contU );
		for( int i=0; i<psize( contU ); i++ )
			imodPointAppend( contLNew, getPt( contL, 0) );
		imodContourDelete( contL );
		imodContourDelete( contU );
		return;
	}
	
	if( psize(contU) == 1 ) {
    imodContourDelete(contLNew);
		contLNew = imodContourDup( contL );
		for( int i=0; i<psize( contL ); i++ )
			imodPointAppend ( contUNew, getPt( contU, 0) );
		imodContourDelete( contL );
		imodContourDelete( contU );
		return;
	}
	
	if(closeContours)        // if we want to close contours:
  {
		imodPointAppend(contL, getPt(contL,0)); // append first point to end
		imodPointAppend(contU, getPt(contU,0));
	}
	
	
	if ( findCoorrespondingPtsAndMakeClockwise )
	{
		imodContourMakeDirection(contL, IMOD_CONTOUR_CLOCKWISE);	//|- make sure conts 
		imodContourMakeDirection(contU, IMOD_CONTOUR_CLOCKWISE);	//|  are clockwise
		
		int idxStartContL, idxStartContU;
    
    findCoorrespondingPts(contL,contU,&idxStartContL,&idxStartContU);
              // finds two points (indexes) on the two contours which appear to match up
		
		cont_reorderPtsToStartAtIdx( contL, idxStartContL );
		cont_reorderPtsToStartAtIdx( contU, idxStartContU );
	}
	
	
  //## CREATE ARRAYS FOR BOTH CONTOURS SHOWING, FOR EACH POINT THE
  //## FRACTION OF ITS DISTANCE ALONG THE TOTAL LENGTH OF THE CONTOUR
	
  vector<float> contLFractsV = cont_getFractPtsAlongLength(contL,false,0);
  vector<float> contUFractsV = cont_getFractPtsAlongLength(contU,false,0);
  
  
  //## CREATE NEW CONTOURS WITH EQUAL NUMBER OF POINTS WHICH MATCH
  //## UP USING LINE FRACTIONS METHOD:
	
  cont_addPtsFractsAlongLength( contL, contLNew, contUFractsV, false, true, 0 );
  cont_addPtsFractsAlongLength( contU, contUNew, contLFractsV, false, true, 0 );
  
	imodContourDelete( contL );
	imodContourDelete( contU );
}




//------------------------
//-- Uses an area minimization metric to creates new versions
//-- of the contours ready for one-to-one point interpolation.
//-- 
//-- NOTE: It achieves this by first calling David's imodObjectMeshConts function
//-- (see: mkmesh.c > imeshContoursCost) to create a triangular mesh between
//-- the two contours a minimum area cost metric. It then looks through the
//-- lits of vertexs and indexes in the mesh to work out which points in one contour
//-- connect to the points in the other contour and returns two modified version
//-- of the contours which each have an equal number of cooresponding points.

void InterpolationEvent::
modifyKeyContsForInterp_MinimumAreaCost( int cbIdx, int ctIdx, 
                                          Icont *contBNew, Icont *contTNew )
{
  imodContourDefault(contBNew);
  imodContourDefault(contTNew);
  
  Icont *contB = getC(cbIdx);               // bottom contour
  Icont *contT = getC(ctIdx);               // top contour
  
  bool inside = 0;
  
  Ipoint scalePt;
  setPt( &scalePt, 1, 1, 1 );
  
  Imesh *mesh = NULL;             // stores a mesh with index connections between
                                  // the verticies in contB amd contT
  mesh = imeshContoursCost( obj, contB, contT, &scalePt, inside, cbIdx, ctIdx  );
  
  int numVert = imodMeshGetMaxVert(mesh);   // number of vertexes
  int numIndx = imodMeshGetMaxIndex(mesh);  // number of indexes
  
  
  //## PRINT MESH DATA:
  /*
  wprint( "MESH RESULTS\n");
  
  wprint("\n  numVert=%d\n", numVert );
  for(int v=0; v<numVert; v++ )
  {
    Ipoint *vert = imodMeshGetVert(mesh, v);
    wprint( "vert %d -> %d,%d,%d \n", v, (int)vert->x, (int)vert->y, (int)vert->z );
  }
  
  wprint("\n  numIndx=%d\n", numIndx );
  for(int i=0; i<imodMeshGetMaxIndex(mesh); i++ )
  {
    int index = imodMeshGetIndex(mesh, i);
    //wprint( "(%d) %d ,", i, index );
    wprint( "%d ,", index );
  }
  */
  
  
  //## DETERMINE THE FIRST VERTEX IN THE TOP CONTOUR:
  
  int firstVertT = 0;         // the first vertex in the mesh vertex array belonging
                              // to the top contour (all before are the bottom contour)
  
  for(int v=1; v<numVert; v++ )
    if( imodMeshGetVert(mesh, v-1)->z != imodMeshGetVert(mesh, v)->z )
    {
      firstVertT = v;
      break;
    }
  
  if( firstVertT != psize(contB)+1 )
    wprint("\aWARNING: modifyKeyContsForInterp_MinimumAreaCost - unexpected # points\n");
  
  
  //## MATCH THE VERTEXES TO THE TWO ORIGINAL CONTOURS:
  /*
  vector<IntAndInt> vertMatch;
  vertMatch.resize( numVert );
  
  for(int v=0; v<firstVertT; v++ )
  {
    Ipoint *vert = imodMeshGetVert(mesh, v);
    
    for( int p=0; p<psize(contB); p++ )
    {
      if( imodPointIsEqual( vert, getPt(contB,p) ) )
      {
        vertMatch[v].idx1 = 0;
        vertMatch[v].idx2 = p;
      }
    }
  }
  for(int v=firstVertT; v<numVert; v++ )
  {
    Ipoint *vert = imodMeshGetVert(mesh, v);
    
    for( int p=0; p<psize(contT); p++ )
    {
      if( imodPointIsEqual( vert, getPt(contT,p) ) )
      {
        vertMatch[v].idx1 = 1;
        vertMatch[v].idx2 = p;
      }
    }
  }
  
  for(int i=0; i<(int)vertMatch.size(); i++ )   //%%%%%%%
    wprint( "vertMatch %d = %d - %d \n", i, vertMatch[i].idx1, vertMatch[i].idx2 );
  */
  
  //## CREATE LIST OF CONNECTIONS:
  
  vector<IntAndInt> conn;           // stores a list of connections matching 
                                    // a point index in contB to a point index in contT
  
	//int increment = (numVert>1000) ? 6 : 3;
	//int increment = 1;
	
	
  for(int i=1; i<imodMeshGetMaxIndex(mesh); i+=1 )
  {
    int vIdx1 = imodMeshGetIndex(mesh, i-1);      // first  vertex index in edge
    int vIdx2 = imodMeshGetIndex(mesh, i);        // second vertex index in edge
    
    bool eitherIdxInstruction = vIdx1 < 0 || vIdx2 < 0;
    bool bothVIdxContB        = vIdx1  < firstVertT && vIdx2  < firstVertT;
    bool bothVIdxContT        = vIdx1 >= firstVertT && vIdx2 >= firstVertT;
    
    if ( !eitherIdxInstruction      // if neither vertex index is an instruction
         && !bothVIdxContB          // and the edge includes one vertex 
         && !bothVIdxContT )        // from each contour
    {
      int idxB = MIN(vIdx1,vIdx2);    // the vertex index in the bottom contour
      int idxT = MAX(vIdx1,vIdx2);    // the vertex index in the top    contour
      
      conn.push_back( IntAndInt( idxB, idxT ) );   // add to list
    }
  }
  
  
  //## SORT LIST OF CONNECTIONS AND ELIMINATE DUPLICATES:
  
  conn = vector_sort( conn );
  vector_eliminateDuplicates( conn );
  
  //for(int i=0; i<(int)conn.size(); i++ )   //%%%%%%%
  //  wprint( "connection %d = %d - %d \n", i, conn[i].idx1, conn[i].idx2 );
  
  
  //## CREATE MATCHING CONTOURS:
  
  for(int i=0; i<(int)conn.size(); i+=3 )
  {
    int idxB = conn[i].idx1;
    int idxT = conn[i].idx2;
    
    if( idxB >= numVert || idxT >= numVert ) {     // should never happen
      wprint("\aERROR: modifyKeyContsForInterp_MinimumAreaCost()\n");
      continue;
    }
    
    imodPointAppend( contBNew, imodMeshGetVert(mesh,idxB)  );
    imodPointAppend( contTNew, imodMeshGetVert(mesh,idxT)  );
  }
  
  /*
	for(int p=psize(contBNew); p<psize(contBNew); p--)
	{
		
	}*/
	
	
  //## IF FIRST POINT OF BOTH CONTOURS IS REPEATED AT THE END: REMOVE IT
  
  if(    imodPointIsEqual( getFirstPt(contBNew), getLastPt(contBNew)  )
      && imodPointIsEqual( getFirstPt(contTNew), getLastPt(contTNew)  ) )
  {
    imodPointDelete( contBNew, psize(contBNew)-1 );
    imodPointDelete( contTNew, psize(contTNew)-1 );
  }
  
  conn.clear();
  
  imodMeshDelete(mesh);
}







//------------------------
//-- Takes an ordered sequence of four contours and generates "crude" smoothly
//-- interpolated contours between c1 and c2. The interpolation is crude
//-- in that contours themselves are interpolated linearly, but then
//-- their centroid and size of each interpolated contour is adjusted
//-- using catmull rom-spline algorithm over the four key contour.


void InterpolationEvent::
interp_SmoothCrude_BetweenConts( int c0, int c1, int c2, int c3 )
{
  Icont *cont0 = getC( c0 );
  Icont *cont1 = getC( c1 );
  Icont *cont2 = getC( c2 );
  Icont *cont3 = getC( c3 );
  
  int c1Z = getZInt( cont1 );
  int c2Z = getZInt( cont2 );
  int diffInSlices = c2Z - c1Z;				// the difference in Z between the two key contours
  
  if( diffInSlices <=1 )
    return;
  
  
  //## FOR EACH KEY CONTOUR: CALCULATE AND STORE IT'S CENTROID AND RADIUS:
  
  Ipoint c0Centroid, c1Centroid, c2Centroid, c3Centroid;
  
  cont_getCentroid( cont0, &c0Centroid );
  cont_getCentroid( cont1, &c1Centroid );
  cont_getCentroid( cont2, &c2Centroid );
  cont_getCentroid( cont3, &c3Centroid );
  
	float c0Radius = cont_getRadius( cont0 );
  float c1Radius = cont_getRadius( cont1 );
  float c2Radius = cont_getRadius( cont2 );
  float c3Radius = cont_getRadius( cont3 );
  
  
  //## GENERAE A SET OF LINEARLY INTERPOLATED CONTOURS BETWEEN CONT1 AND CONT2:
  
  vector<IcontPtr> newConts = getLinearInterpConts( c1, c2 );		
  
	vector<float> fractsZ = calcCardinalSplineFractsEachSlice( c0Centroid, c1Centroid,
                                                                  c2Centroid, c3Centroid );
          // stores a vector of fractions representing how far between
          // cont1 and cont2 a cardinal spline between the centroids crosses
          // EACH z slice between them (in ascending order)
  
  
  //## FOR EACH INTERPOALTED CONTOUR MOVE AND RESIZE IT USING THE CARDINAL SPLINE
  //## VALUES FOR CENTROID AND RADIUS:
  
	for (int c=0; c<int(newConts.size()); c++)			 // for each interpolated contour: 
  {
    Icont *cont = newConts[c].cont;
    int interpCurrZ = getZInt(cont);
    
    float fractUpFromFirst = fDiv( float(interpCurrZ - c1Z), float(diffInSlices) );	
            // the fraction distance the current Z value
            // is between lower and upper key contours
    
    //## TRANSLATE CONTOUR USING CARDINAL SPLINE THROUGH CENTROID:
    
    Ipoint currCentroid;
    //cont_getCentroid( cont, &currCentroid );
    currCentroid = line_findPtFractBetweenPts(&c1Centroid,&c2Centroid,fractUpFromFirst);	
                        // the current centroid of the interpolated contour
    
    Ipoint newCentroid = getPtCardinalSpline( fractsZ[c], c0Centroid, c1Centroid,
                                              c2Centroid, c3Centroid, TENSILE_FRACT );
                      // the centroid we want (according to the spline algorithm)
    
    cont_translate( cont, newCentroid.x-currCentroid.x, newCentroid.y-currCentroid.y );		
                      // translates (moves) the interpolated contour
                      // so the centoid is in the correct position
    
    //## SCALE CONTOUR USING CARDINAL SPLINE THROUGH RADIUS:
    
    float currRadius = cont_getRadius( cont );	
    
    float newRadius = getValCardinalSpline( fractsZ[c], c0Radius, c1Radius,
                                            c2Radius, c3Radius, TENSILE_FRACT );
                      // the radius we want (according to the spline algorithm)
    
    float scaleFactor = fDiv( newRadius, currRadius );											
                      // factor by which we will resize the interpolated contour
    
    cont_scaleAboutPt( cont, &newCentroid, scaleFactor, true );
                      // resizes the contour about it's centroid to
                      // the desired size (represented by radius)
    
    
    //## ADD THE INTERPOLATED CONTOUR TO THE OBJECT:
    
    setZValue( cont, interpCurrZ );
    addInterpolatedContToObj( obj, cont );
  }
  
  deleteContours( newConts );
}






//------------------------
//-- Takes two key contours ("contKeyL", "contKeyU") and
//-- generates a series of smooth interpolated contours between them.
//-- It does this by trying to find contour c0 below contKeyL
//-- and cont c3 above contKeyU, then making versions of these
//-- such that they all have the same # of points, then interpolating
//-- between each set of cooresponing points using cardinal splines.

void InterpolationEvent::
interp_SmoothPointwise_BetweenTwoConts( int clIdx, int cuIdx )
{
  Icont* contL = getC(clIdx);
  Icont* contU = getC(cuIdx);
  
	if( psize(contL) == 0 || psize(contU) == 0
      || getZ(contL) >= getZ(contU) ) {
		cerr << "ERROR: interp_SmoothPointwise()" << endl;
		return;
	}
	int zDistApart = getZ(contU) - getZ(contL);
	if( zDistApart <= 1 )				// if conts only one slice apart, or lower contour
    return;                   // is above upper contour: return
	
//## PREPARE UPPER AND LOWER KEY CONTOUR FOR LINEAR INTERPOLATION BY
//## ADDING EXTRA POINTS TO EACH:
	
	Icont *c0 = imodContourNew();     //|- will store final contour, all with an
	Icont *c1 = imodContourNew();     //|  equal number of points, ready for
	Icont *c2 = imodContourNew();     //|  smooth interpolation.
	Icont *c3 = imodContourNew();     //|
  
	modifyKeyContsForInterp( clIdx, cuIdx, c1, c2, true, true );
              // prepares the lower and upper key contour (now called c1 and c2)
              // for linear interpolation by adding extra points to each
              // such that they an equal number of (cooresponding) points
	
//## IF THERE IS A CONTOUR BELOW THE (NOW MODIFIED) LOWER CONTOUR:
//## CREATE A VERSION WITH COORESPONDING POINTS READY FOR SMOOTH INTERPOLATION
	
	int idxC0 = findIdxNearestKeyCont( clIdx, plug.zBridge, false );
	if(idxC0 == -1)     // if no contour was found below c1: c0 is just c1
  {
    imodContourDelete(c0);
		c0 = imodContourDup(c1);
	}
	else                // else: generate a version of this into c0 which has 
  {                   // points cooresponding one-to-one with c2
    int idxStartC0, idxStartC1;
    
    findCoorrespondingPts( getC(idxC0), c1,&idxStartC0,&idxStartC1);
    vector<float> c1FractsV = cont_getFractPtsAlongLength(c1,true,idxStartC1);
    cont_addPtsFractsAlongLength( getC(idxC0), c0, c1FractsV, true,false,idxStartC0);
    cont_reorderPtsToStartAtIdx( c0, psize(c1)-idxStartC1 );
  }
  
	
//## IF THERE IS A CONTOUR ABOVE THE (NOW MODIFIED) UPPER CONTOUR:
//## CREATE A VERSION WITH COORESPONDING POINTS READY FOR SMOOTH INTERPOLATION
	
	int idxC3 = findIdxNearestKeyCont( cuIdx, plug.zBridge, true );
	if(idxC3 == -1)		// if no contour was found above c2: c3 is just c2
  {
    imodContourDelete(c3);
		c3 = imodContourDup(c2);
	}
	else            // else (if contour was found above c2): generate a version of 
  {               // this into c3 which has points cooresponding one-to-one with c2
    int idxStartC2, idxStartC3;
    
    findCoorrespondingPts_FourPtMBR   ( c2, getC(idxC3), &idxStartC2, &idxStartC3 );
    vector<float> c2FractsV = cont_getFractPtsAlongLength(c2,true,idxStartC2);
    cont_addPtsFractsAlongLength(getC(idxC3), c3, c2FractsV, true,false,idxStartC3);
    cont_reorderPtsToStartAtIdx( c3, psize(c2)-idxStartC2 );
	}
	
  
	if( (psize(c0) != psize(c1)) || (psize(c1) != psize(c2)) || (psize(c2) != psize(c3)) )
  {
		wprint("\aERROR: interp_SmoothPointwise() - conts not all same # points\n");
    //cout << "psize(c0)=" << psize(c0) << endl;     flush(std::cout); //%%%%%%%%%
    //cout << "psize(c1)=" << psize(c1) << endl;     flush(std::cout); //%%%%%%%%%
    //cout << "psize(c2)=" << psize(c2) << endl;     flush(std::cout); //%%%%%%%%%
    //cout << "psize(c3)=" << psize(c3) << endl;     flush(std::cout); //%%%%%%%%%
    return;
	}
	
  
//## USING THE FOUR COORESPONDING COUNTOURS (c0,c1,c2,c3):
//## INTERPOLATE EACH COORESPONDING POINT BETWEEN c1 AND c2
//## TO FORM "SMOOTH" INTERPOLATED CONTOURS
	
	vector<IcontPtr> newConts;				// will store the new (interpolated) contours
                                    // between the upper and lower contours (c1 and c2)
  
	for(int i=0; i<(zDistApart-1); i++  )	// populate newConts with empty contours
		newConts.push_back( IcontPtr() );
	
	for (int j=0; j<psize(c1); j++)		// for each (cooresponding) point:
	{
		vector<float> fractsZ =
      calcCardinalSplineFractsEachSlice( *getPt(c0,j), *getPt(c1,j),
                                                       *getPt(c2,j), *getPt(c3,j)  );
						// fractsZ stores a vector of fractions representing how far
            // between the lower and upper key contours  a cardinal spline between
						// the centroids crosses EACH z slice between them (in ascending order)
		
		for(int i=0; i<int(newConts.size()); i++)	// for each z between upper & lower slice:
		{
			Ipoint newPt = getPtCardinalSpline( fractsZ[i],  *getPt(c0,j), *getPt(c1,j),
                                      *getPt(c2,j), *getPt(c3,j), TENSILE_FRACT  );
                            // finds the point at which the point intersects the
                            // desired z value (according to the catmull-rom algorithm)
      
			newPt.z = getZ(c1)+1 + i;         
                            // changes the z value so it's the 
                            // correct z slice (it might be slightly off)
      
			imodPointAppend( newConts[i].cont, &newPt );
                            // add this point to the correct contour.
		}
	}
  
	for(int i=0; i<int(newConts.size()); i++  )        // for each newly generated contour:
		addInterpolatedContToObj( obj, newConts[i].cont );	// add it to the object
  
  deleteContours(newConts);  
	imodContourDelete( c0 );
	imodContourDelete( c1 );
	imodContourDelete( c2 );
	imodContourDelete( c3 );
}

//------------------------
//-- Takes two key contours and generates a new set of linearly interpolated
//-- contours between them.

vector<IcontPtr> InterpolationEvent::
getLinearInterpConts( int c1Idx, int c2Idx )
{
	vector<IcontPtr> newInterpolatedConts;
  
  bool closed = getClosed(c1Idx) || getClosed(c2Idx);
                    // typically both (or at least one) of the contours will be
                    // closed (used in modifyKeyContsForInterp) 
  
  bool c1IsLower = getCZ(c1Idx) < getCZ(c2Idx);
  int clIdx     = (c1IsLower) ? c1Idx : c2Idx;
  int cuIdx     = (c1IsLower) ? c2Idx : c1Idx;
	Icont *contL  = getC(clIdx);
	Icont *contU  = getC(cuIdx);
  
  
	int zDistApart = getZ(contU) - getZ(contL);
	if( zDistApart <= 1 )   // if conts only one slice apart, or lower is above upper:
  {				
		//wprint( "ERROR: getLinearInterpConts()\n" );      // should avoid this
		return newInterpolatedConts;                      // return empty set
	}
	
//## CREATE TWO NEW KEY CONTOURS WITH (AN EQUAL NUMBER OF) COORESPONDING
//## POINTS FOR INTERPOLATION:
	
	Icont *contLNew = imodContourNew();		// |-- version of conts with an equal number of
	Icont *contUNew = imodContourNew();		// |   points which coorspond one-to-one.
	
	modifyKeyContsForInterp( clIdx, cuIdx, contLNew, contUNew, closed, closed );
	
  
//## USE NEW KEY CONTOURS TO PERFORM LINEAR POINT INTERPOLATION AND GENERATE
//## NEW INTERPOLATED CONTOURS BETWEEN THE KEY CONTOURS:
	
	
	for(int z=getZ( contLNew )+1; z<=getZ( contUNew )-1; z++  )
	{
		float fractBetweenKeyContour = float(z - getZ( contLNew )) / float(zDistApart);
		
		Icont *contNew = imodContourNew();
		setInterpolated( contNew, 1 );
		for(int i=0; i<psize( contLNew ) && i<psize( contUNew ); i++) {
			Ipoint tmpPt = line_findPtFractBetweenPts( getPt(contLNew,i), getPt(contUNew,i),
							    fractBetweenKeyContour );
			imodPointAppend( contNew, &tmpPt);
		}
		
		newInterpolatedConts.push_back( IcontPtr(contNew) );
		imodContourDelete( contNew );
	}
	
	imodContourDelete( contLNew );
	imodContourDelete( contUNew );
	
  if( !closed && isOpenFlag( getC(c1Idx) ) && isOpenFlag( getC(c2Idx) )  )
  {
    //wprint("TWO OPEN CONTS\n");
    for (int i=0; i<newInterpolatedConts.size(); i++)
      setOpenFlag( newInterpolatedConts[i].cont, 1 );
  }
  
  
	return newInterpolatedConts;
}


//------------------------
//-- Takes two key contours and generates a new set of linearly
//-- interpolated contours between them.

int InterpolationEvent::
interp_Linear_BetweenTwoConts( int clIdx, int cuIdx )
{	
	vector<IcontPtr> newConts = getLinearInterpConts(clIdx, cuIdx);
	int numNewConts = newConts.size();
  
	for(int i=0; i<int(newConts.size()); i++)
		addInterpolatedContToObj( obj, newConts[i].cont );
	
  deleteContours( newConts );
	return ( numNewConts );
}




//------------------------
//-- Takes a number of key contours and generates a set of linearly
//-- interpolated contours using branching
//-- Returns the number of contours that result

int InterpolationEvent::
mergeAllTouchingConts( vector<IcontPtr> conts )
{
	for(int i=1; i<int(conts.size()); i++)
		if( cont_doContsTouch( conts[i-1].cont, conts[i].cont ) )
		{
      cont_getOuterUnionPolygon( conts[i-1].cont, conts[i-1].cont, conts[i].cont );
      deleteAllPts( conts[i].cont );
      eraseContour( conts, i );
			i=0;
		}
  
  return ( conts.size() );
}

//------------------------
//-- Adds interpolated contours and merges any contours
//-- which are on the same Z slice an touch

void InterpolationEvent::
addAllInterpolatedConstAndMerge( vector< vector<IcontPtr> > newConts )
{
	vector2D_transpose( newConts );
	
	for (int z=0; z<(int)newConts.size(); z++) {
		int numConts = mergeAllTouchingConts( newConts[z] );
    newConts[z].resize( numConts );   // this line shouldn't be needed, but it is.  :-/
  }
  
	for (int z=0; z<(int)newConts.size(); z++) {
		for (int b=0; b<(int)newConts[z].size(); b++)
			addInterpolatedContToObj( obj, newConts[z][b].cont );
    
    deleteContours( newConts[z] );
  }
}

//------------------------
//-- Takes a number of key contours and generates a set
//-- of linearly interpolated contours using branching

void InterpolationEvent::
interp_Linear_BetweenContsMerge( int cbIdx, vector<int> branchContIdx )
{
  if( (int)branchContIdx.size() == 1 ) {
		interp_Linear_BetweenTwoConts( cbIdx, branchContIdx[0] );
		return;
	}
	
	vector< vector<IcontPtr> > newConts;
	
	for (int b=0; b<(int)branchContIdx.size(); b++)
		newConts.push_back( getLinearInterpConts(cbIdx, branchContIdx[b]) );
	
	addAllInterpolatedConstAndMerge( newConts );
}


//------------------------
//-- Takes a single contour, then finds all finds ALL key contours
//-- which (appear to) belong to the same surface and generates
//-- a CRUDE form of smooth interpolation between each of these.
//-- The interpolation is crude in that contours themselves are
//-- interpolated linearly, but then their centroid and size is
//-- adjusted linearly using catmull rom-spline algorithm.
//-- This can be effective for simple contour shapes, but results
//-- are not ideal for more complex-shaped contours.

int InterpolationEvent::
interp_Linear( int baseContIdx, int maxDist )
{
	int numAdded = 0;
	
	switch( plug.branchingMethod )
	{
		case( BR_BRANCHING_OFF ):
			{
				if( isKeyContAbove() )
					numAdded += interp_Linear_BetweenTwoConts(sContIdx,closestAboveIdx);
				if( isKeyContBelow() )
					numAdded += interp_Linear_BetweenTwoConts(closestBelowIdx,sContIdx);
			}
			break;
		
		
		case( BR_MERGE_CONTS ):
			{
				if( isKeyContAbove() )
					interp_Linear_BetweenContsMerge( sContIdx, aboveIdxs );
				if( isKeyContBelow() )
					interp_Linear_BetweenContsMerge( sContIdx, belowIdxs );
			}
			break;
		
    /*	
		case( BR_BRIDGE_GAPS ):
		{
			if( isKeyContAbove() )
			{
				Icont joinedCont = cont_joinContsAtClosestApproach(getAboveConts(obj),true);
				interp_Linear_BetweenTwoConts( obj, getCon(obj), joinedCont );
			}
			if( isKeyContBelow() )
			{
				Icont joinedCont = cont_joinContsAtClosestApproach(getBelowConts(obj),true);
				interp_Linear_BetweenTwoConts( obj, getCon(obj), joinedCont );
			}
			
		}
		break;
		
		case( BR_PT_BELOW ):
		{
			wprint("WARNING: BR_PT_BELOW does not work for LINEAR interpolation");
		}
		break;
		*/
    
    default:
      wprint("Change branching type\n");
      break;
	}
	
	return numAdded;
}








//------------------------
//-- Takes just one key contour - the "middle contour" in an
//-- approximately spherical object - and generates interpolated contours
//-- above and below it in a sphere shape.


void InterpolationEvent::
interp_Spherical_OnSingleCont( int cMiddleIdx )
{
	Imod *imod = ivwGetModel(plug.view);
	float modScaleZ = imodGetZScale(imod);
	
	Icont *cont = imodContourDup( getC( cMiddleIdx ) );
	
//## CALCULATE CENTROID, AREA, AND SPAN FOR SPHERE:
	
	int contZ = getZ( cont );
	
  
  
	float area = imodContourArea(cont);			// will store the area of the middle contour
	Ipoint centroidPt;
	cont_getCenterOfMBR( cont, &centroidPt );
	float radius = sqrt( ABS(area) / PI);			// represents the "average radius" of the
                                            // "sphere of best fit" over the contour
	int    zSlicesRadiusSpans = (int)floor(radius/modScaleZ);
                                          // number of whole z slices the radius spans
  
  
//## DELETE INTERPOLATED CONTOURS IN SPHERE'S WAY:
	
	deleteAllSameSurfInterpContsInZRange( contZ-(zSlicesRadiusSpans+2),
                                        contZ+(zSlicesRadiusSpans+2), cMiddleIdx );
	  
  
//## GENERATE INTERPOLATED CONTOURS ABOVE AND BELOW MIDDLE CONTOUR:
	
	for(int i=1; i<=zSlicesRadiusSpans; i++)
	{
		float radiusAtThisZ = sqrt( SQ(radius) - SQ(i*modScaleZ) );		
                            // using pythagorean theorem:     adj = sqrt(hpy^2 - opp^2)
		float scaleFactor = radiusAtThisZ/radius;				
                                  // the size of the radius/area formed if the sphere
                                  // is cut at this z compared to the sphere's radius
		Icont *contNewLower = imodContourDup( cont );
		cont_scaleAboutPt( contNewLower, &centroidPt, scaleFactor, true );		
                                  // duplicate middle contour, but resize it about
                                  // its centroid so it has the correct radius to
                                  // fit the sphere's profile
		Icont *contNewUpper = imodContourDup( contNewLower );
		
		setZValue(contNewUpper,contZ+i);           //|- add this same scaled down  
    addInterpolatedContToObj(obj,contNewUpper);		//|  contour appropriate distance 
		setZValue(contNewLower,contZ-i);           //|  above and below the middle 
    addInterpolatedContToObj(obj,contNewLower);		//|  contour
    
    imodContourDelete( contNewLower );
    imodContourDelete( contNewUpper );
	}
	
	imodContourDelete( cont );
}






//------------------------
//-- Takes a single contour, and executes spherical interpolation
//-- by finding ALL key contours which (seem to) belong to the same 
//-- surface, determining a "sphere of best fit", and generating
//-- interpolated contours between all pairs of contours, as well as
//-- additional contours above and below the top-most and bottom most
//-- contours respectively (in order to "cap" the sphere).

void InterpolationEvent::
interp_Spherical( int baseContIdx, int maxDist )
{
	Imod *imod = ivwGetModel(plug.view);
	float modScaleZ = imodGetZScale(imod);
	
	int surfMethod = plug.window->getSurfaceResolveMethod();
	
	if ( surfMethod != SR_CENTER_OVERLAP ) {
		wprint( "NOTE: Am changing surface resolution method to "
            "'centroid overlap' for spherical interpolation" );
		plug.surfResolveMethod = SR_CENTER_OVERLAP;
	}
	
	vector<int> idxInSurf =
    findIdxsAllKeyContsNoBranching(baseContIdx,maxDist);
  
//## IF THERE ARE NO OTHER KEY CONTOURS BELONGING TO THIS SURFACE:
//## ASSUME IT'S THE MIDDLE CONTOUR AND DRAW SPHERE AROUND IT
	
	if( idxInSurf.size() == 1 ) {
		interp_Spherical_OnSingleCont( baseContIdx );
		return;
	}
	
  
//## (IF THERE WERE OTHER KEY CONTOURS BELONGING TO THIS SURFACE):
//## DELETE ALL INTERPOLATED CONTOURS ATTCHED TO THESE KEY CONTOUR
	
	for (int i=0; i<int(idxInSurf.size()); i++)
		deleteAllSameSurfInterpContsEitherSideOfCont( idxInSurf[i] );
	
//## FOR EACH KEY CONTOUR: CALCULATE AND STORE IT'S CENTROID AND RADIUS
	
	vector<Ipoint> contCenter;	// store centroid for each key contour in surface
	vector<float>  contRadius;		// store radius   for each key contour in surface
	
	for (int i=0; i<(int)idxInSurf.size(); i++)		// for each contour idx in same surface:
	{
		Ipoint centroid;                      // center of contour
		float area = imodContourArea( getC( idxInSurf[i] ) );
		cont_getCenterOfMBR( getC(idxInSurf[i] ), &centroid );
		float radius = sqrt(ABS(area)/PI);		// calculate radius for circle with given area
		
		contCenter.push_back( centroid );     //|- store these values (for this key contour)
		contRadius.push_back( radius );				//|  in an array
	}
  
	
//## FOR EACH PAIR OF ADJACENT CONTOURS: CALCULATE AND STORE A CENTER
//## AND RADIUS DESCRIBING A SPHERE ENCOMPASSING THEM
	
	vector<float> sphereZCentroid;		// stores the centroid for a sphere encompassing
                                    // the key contour in idxInSurf and the one above it
	vector<float> sphereRadius;       // stores the radius for the above
	
	for (int i=0; i<(int(idxInSurf.size())-1); i++)
	{
		float diffRadius = contRadius[i+1] - contRadius[i];
		float diffZ      = contCenter[i+1].z - contCenter[i].z;
		
		float radiusMid	= (contRadius[i+1] + contRadius[i]) / 2.0;
		float zMid		= (contCenter[i+1].z + contCenter[i].z) / 2.0;
		
		float zCenter = (((radiusMid/(diffZ*modScaleZ)) * diffRadius)/modScaleZ) + zMid;
    float zDist   = (contCenter[i].z-zCenter)*modScaleZ;
		float sphereRad = sqrt( SQ(contRadius[i]) + SQ(zDist) );
		
		sphereZCentroid.push_back ( zCenter   );
		sphereRadius.push_back    ( sphereRad );
	}
	
	
//## FOR EACH PAIR OF ADJACENT CONTOURS: GENERATE LINEARLY INTERPOLATED
//## CONTOURS BETWEEN THEM, AND RESIZE THEM USING SPHERE RADIUS
	
	vector<IcontPtr> newConts;
	
	for (int i=0; i<(int(idxInSurf.size())-1); i++)
	{
    int cIdxStart = idxInSurf[i];
    int cIdxEnd   = idxInSurf[i+1];
    
		Icont *contStart = getC( idxInSurf[i] );
		Icont *contEnd   = getC( idxInSurf[i+1] );
    
		newConts = getLinearInterpConts( cIdxStart, cIdxEnd );
		
    float startZ = getZ(contStart);
    float endZ   = getZ(contEnd);
    float diffInSlices = endZ - startZ;
    
    //cout << "cIdxStart=" << cIdxStart << " cIdxEnd=" << cIdxEnd << endl;
    //cout << "newConts.size()=" << newConts.size() << " diffInSlices=" << diffInSlices << endl;
    //cout << "startZ=" << startZ << " endZ=" << endZ << endl;
    
    
		for (int j=0; j<(int)newConts.size(); j++)
		{
			float interpCurrZ = getZ(newConts[j].cont);
			float fractUpFromFirst = fDiv( (interpCurrZ - startZ), diffInSlices );
			float currRadius = (contRadius[i+1]-contRadius[i])*fractUpFromFirst+contRadius[i];
      float zDist = (sphereZCentroid[i] - interpCurrZ)*modScaleZ;
			float newRadius = sqrt( SQ(sphereRadius[i]) - SQ(zDist) );
                            // using pythagorean theorem:     adj = sqrt(hpy^2 - opp^2)
			float scaleFactor = fDiv( newRadius, currRadius );
			Ipoint centroid = line_findPtFractBetweenPts( &contCenter[i], &contCenter[i+1],
                                                    fractUpFromFirst );
			cont_scaleAboutPt( newConts[j].cont, &centroid, scaleFactor, true );
      
			addInterpolatedContToObj( obj, newConts[j].cont );
		}
    
    deleteContours(newConts);
	}
  
	
//## USE SAME METOHD TO CALCULATE A SPHERE FOR THE HIGHEST AND
//## LOWEST CONTOUR (CALL THIS HIGH-LOW-SPHERE):
	
	int idxLastCont = int( idxInSurf.size() )-1;
	
	Icont *lowestCont  = getC( idxInSurf[0] );
	Icont *highestCont = getC( idxInSurf[idxLastCont] );
	
	float diffRadius = contRadius[idxLastCont] - contRadius[0];
	float diffZ      = contCenter[idxLastCont].z - contCenter[0].z;
	
	float radiusMid	= (contRadius[idxLastCont]-contRadius[0] )/2.0f + contRadius[0];
	float zMid		  = (contCenter[idxLastCont].z-contCenter[0].z)/2.0f + contCenter[0].z;
	
	float HLsphereZCenter = (((radiusMid/(diffZ*modScaleZ))*diffRadius)/modScaleZ) + zMid;
                            // stores the centroid of the "high-low sphere"
                            // (a sphere encompassing the top and bottom-most conts)
  
  float zDist = (contCenter[0].z-HLsphereZCenter)*modScaleZ;
	float HLsphereRadius = sqrt( SQ(contRadius[0]) + SQ(zDist) );		// radius
	
	float scaleZTEST = ABS(pow((-1.0 * radiusMid * diffRadius /
                              ((zMid - HLsphereZCenter) * diffZ)), (0.5)));
	
	
//## USE HIGH-LOW-SPHERE TO INTERPOLATE UPWARDS FROM HIGHEST AND
//## DOWNWARDS FROM LOWEST CONTOURS:
	
	int lowestZInt  = (int)ceil (HLsphereZCenter - (HLsphereRadius/modScaleZ));
	int highestZInt = (int)floor(HLsphereZCenter + (HLsphereRadius/modScaleZ));
                              // represents the highest and lowest slic where an
                              // interpolated contour will be generated
	
  
	for(int z=lowestZInt; z<getZ(lowestCont); z++ )				// for each slice below the
	{                                                     // lowest key contour:
		float opp = ((HLsphereZCenter - z)*modScaleZ);
		float newRadius = sqrt( (HLsphereRadius*HLsphereRadius) - (opp*opp) );
		float scaleFactor = fDiv( newRadius, contRadius[0] );
            // determines how small interpolated contour will be compared to key contour
		
		newConts.push_back( IcontPtr( getC( idxInSurf[0] ) ) );		
		cont_scaleAboutPt(newConts.back().cont, &contCenter[0], scaleFactor, true );			
                      // copy lowest contour, but scale it down to appropriate size ...
		setZValue( newConts.back().cont, z );      // then copy it to desired z slice
	}
	
	for(int z=highestZInt; z>getZ(highestCont); z-- )			// for each slice above the
	{                                                     // lowest key contour:
		float opp = ((HLsphereZCenter - z)*modScaleZ);
		float newRadius = sqrt( (HLsphereRadius*HLsphereRadius) - (opp*opp) );
		float scaleFactor = fDiv( newRadius, contRadius[idxLastCont] );			
            // determines how small interpolated contour will be compared to key contour
		
		newConts.push_back( IcontPtr( getC( idxInSurf[idxLastCont] ) ) );		
		cont_scaleAboutPt(newConts.back().cont,&contCenter[idxLastCont],scaleFactor,true);			
                      // copy lowest contour, but scale it down to appropriate size ...
		setZValue( newConts.back().cont, z );     // then copy it to desired z slice
	}
	
	for(int i=0; i<int(newConts.size()); i++)
		addInterpolatedContToObj( obj, newConts[i].cont );
	
	deleteContours(newConts);
}







//------------------------
//-- Takes a single contour, then tries to find three key contours above and below
//-- which (appear to) belong to the same surface and generates a CRUDE form of
//-- smooth interpolation between each of these. The interpolation is crude
//-- in that contours themselves are interpolated linearly, but then
//-- their centroid and size is adjusted linearly using catmull rom-spline
//-- algorithm. This can be effective for simple contour shapes, but results
//-- are not ideal for more complex-shaped contours.

void InterpolationEvent::
interp_SmoothCrude( int baseContIdx, int maxDist )
{
  if( plug.branchingMethod == BR_MERGE_CONTS && 
      ( (int)aboveIdxs.size() > 1 || (int)belowIdxs.size() > 1 ) )
  {
    if( isKeyContAbove() )
      interp_Linear_BetweenContsMerge( sContIdx, aboveIdxs );
    if( isKeyContBelow() )
      interp_Linear_BetweenContsMerge( sContIdx, belowIdxs );
    wprint("LINEAR branching instead\n");
    return;
  }
  
  if( isKeyContAbove() )
  {
		int closestAbove2Idx = findIdxNearestKeyCont(closestAboveIdx,
                                                 plug.zBridge,true);
    
    int c0 = ( isKeyContBelow() ) ? closestBelowIdx : baseContIdx;
    int c1 = baseContIdx;
    int c2 = closestAboveIdx;
    int c3 = ( closestAbove2Idx!=-1 ) ? closestAbove2Idx : closestAboveIdx;
    
    interp_SmoothCrude_BetweenConts( c0, c1, c2, c3 );
	}
  
  if( isKeyContBelow() )
  {
		int closestBelow2Idx = findIdxNearestKeyCont(closestBelowIdx,
                                                 plug.zBridge,false);
    
    int c0 = ( closestBelow2Idx!=-1 ) ? closestBelow2Idx : closestBelowIdx;
    int c1 = closestBelowIdx;
    int c2 = baseContIdx;
    int c3 = ( isKeyContAbove() ) ? closestAboveIdx : baseContIdx;
    
    interp_SmoothCrude_BetweenConts( c0, c1, c2, c3 );
	}
  
}



//------------------------
//-- Takes a single contour, then finds all finds ALL key contours which
//-- (appear to) belong to the same surface and generates a CRUDE form of
//-- smooth interpolation between each of these. The interpolation is crude
//-- in that contours themselves are interpolated linearly, but then
//-- their centroid and size is adjusted linearly using catmull rom-spline
//-- algorithm. This can be effective for simple contour shapes, but results
//-- are not ideal for more complex-shaped contours.

void InterpolationEvent::
interp_SmoothCrudeAll( int baseContIdx, int maxDist )
{
  if( plug.branchingMethod == BR_MERGE_CONTS && 
      ( (int)aboveIdxs.size() > 1 || (int)belowIdxs.size() > 1 ) )
  {
    if( isKeyContAbove() )
      interp_Linear_BetweenContsMerge( sContIdx, aboveIdxs );
    if( isKeyContBelow() )
      interp_Linear_BetweenContsMerge( sContIdx, belowIdxs );
    wprint("LINEAR branching instead\n");
    return;
  }
  
	vector<int> idxInSurf =
      findIdxsAllKeyContsNoBranching( baseContIdx, maxDist );
	
//## IF THERE ARE NO OTHER KEY CONTOURS BELONGING TO THIS SURFACE: DO NOTHING
	
	if( idxInSurf.size() == 1 ) {
		return;
	}
	
//## DELETE ALL INTERPOLATED CONTOURS EITHER SIDE OF THIS KEY CONTOUR
	
	for (int i=0; i<int(idxInSurf.size()); i++)
		deleteAllSameSurfInterpContsEitherSideOfCont( idxInSurf[i] );
  
  
//## FOR EACH PAIR OF ADJACENT CONTOURS: GENERATE CRUDE SMOOTHLY INTERPOLATED
//## CONTOURS BETWEEN THEM
	
  int numKeyConts = (int)idxInSurf.size();
  
	for (int i=0; i<=numKeyConts-2; i++)
	{
    int c0	= (i<=0) ? idxInSurf[0] : idxInSurf[i-1];
		int c1	= idxInSurf[i]; 
		int c2	= idxInSurf[i+1];
		int c3	= (i>=numKeyConts-2) ? idxInSurf[i+1] : idxInSurf[i+2];
    
    interp_SmoothCrude_BetweenConts( c0, c1, c2, c3 );
	}
}



//------------------------
//-- Takes a single contour, checks if there are other contours
//-- in the same surface, and if so, generates smooth interpolated
//-- contours between them using cardinal splines between 
//-- cooresponding points over the contours.

void InterpolationEvent::
interp_SmoothPointwise( int baseContIdx, int maxDist )
{
  if( isKeyContAbove() )	{
		interp_SmoothPointwise_BetweenTwoConts(sContIdx,closestAboveIdx);
		
		int closestAbove2Idx = findIdxNearestKeyCont(closestAboveIdx, plug.zBridge, true);
		if( closestAbove2Idx!=-1 )
			interp_SmoothPointwise_BetweenTwoConts(closestAboveIdx, closestAbove2Idx);
	}
	if(  isKeyContBelow()  )	{
		interp_SmoothPointwise_BetweenTwoConts(closestBelowIdx,sContIdx);
		
		int closestBelow2Idx = findIdxNearestKeyCont(closestBelowIdx, plug.zBridge, false);
		if( closestBelow2Idx!=-1 )
			interp_SmoothPointwise_BetweenTwoConts(closestBelow2Idx, closestBelowIdx);
	}
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
  Imod *imod = ivwGetModel(plug.view);
  return ( imodObjectGet(imod) );
}


//---------------------------------
//-- Returns a pointer to the currently selected contour.

Icont *getCurrCont()
{
  Imod *imod = ivwGetModel(plug.view);
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
//-- Returns true if the object is valid

bool isCurrObjValid()
{
  Iobj *obj = getCurrObj();
  return (obj!=NULL);
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



//----------------------------------------------------------------------------
//
//          CONTOUR EVENT FUNCTIONS:
//
//----------------------------------------------------------------------------


//------------------------
//-- Adds a new INTERPOLATED contour to the specified object

bool addInterpolatedContToObj( Iobj *obj, Icont *cont, int interpolated )
{
	//## CHECK CONTOUR IS WITHIN TOMOGRRAM'S Z RANGE:
	
  int zVal = getZ(cont);
	if( zVal < 0 || zVal >= plug.zsize ) {
		//wprint( "Contour '%d' is outside Z range {%d-%d}\n", zVal, minZ, maxZ );
		return false;
	}
	
	//## MAKE INTERPOLATED AND ADD:
	
  Icont *newCont = imodContourDup( cont );    // malloc new contour and don't delele it
  setInterpolated( newCont, interpolated );   // make contour interpolated
  
  int numConts = csize(obj);
	undoContourAdditionCO( plug.view, numConts );             // REGISTER UNDO
	int newContPos = imodObjectAddContour( obj, newCont );
	free(newCont);
  
	if( newContPos == -1 || newContPos != numConts ) {
		wprint( "ERROR - addInterpolatedContToObj\n" );
		return false;
	}							// FIGURE THIS OUT LATER
	
	return true;
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
//					POINT INTERPOLATION METHODS:
//
//----------------------------------------------------------------------------


//------------------------
//-- Takes four points for the catmull-rom equation, and through an
//-- iterative high/low guess process tries to resolve the FRACTION
//-- of the distance between p1 and p2 which has the desired Z value.
//--
//-- NOTE: All four points should have their z value in either
//--       ascending or descending order.
//--
//-- NOTE/WARNING:  There is almost certainly a way to compute
//--                this matematically, but I tried using
//--                mathimatica to solve the catmull-rom equation for
//--                "t" and it gave me gibberish

const float  ALLOWABLE_DEV = 0.01;	// allowable deviation of z value from desired value
const int    MAX_GUESSES = 50;			// limits the maximum number of guesses to 50
                                    // (although unlikely to ever perform this many)

float calcCardinalSplineFractAtZ( int zVal, Ipoint p0, Ipoint p1,
                                             Ipoint p2,  Ipoint p3 )
{
	if      ( zVal == p1.z )	return 0;		//|- if one of these opposing key points 
	else if ( zVal == p2.z )	return 1;		//|  is on the same z value: return it
	
	if( !( p1.z <= (float)zVal && (float)zVal <= p2.z) ) {		// should never happen
		wprint("ERROR: getPtAndFractWithZValueForCardinalSpline");
		return -1;
	}
	
	float lowerFract = 0;		// records previous lowest  guess (which value must be > than)
	float upperFract = 1;		// records previous highest guess (which value must be < than)
	
	float lowerZVal = MIN( p1.z, p2.z );		// Z value of previous lowest  guess
	float upperZVal = MAX( p1.z, p2.z );		// Z value of previous highest guess
	
	float nextGuessFract;		// the fraction between p1 and p2 (along the spline)
                          // of our next guess
	
  
	//## USE/RECORD HIGH-LOW GUESSES TO RESOLVE A BEST-GUESS (DOESN'T NEED TO BE EXACT)
	
	for ( int i=0; i<MAX_GUESSES; i++ )
	{
		nextGuessFract = ((upperFract-lowerFract)/2.0) + lowerFract;						
    // sets next guess halfway between the highest and lowest previous safe guesses
		
		float guessZVal = getValCardinalSpline( nextGuessFract, p0.z, p1.z, p2.z, p3.z,
                                        TENSILE_FRACT );			
    // determines z value at current guessed fraction
		
		if (((zVal-ALLOWABLE_DEV)<=guessZVal)       // if estimate is within allowable 
        && (guessZVal<=(zVal+ALLOWABLE_DEV)))   //   deviation of desired z value: 
    {		
			//cout << " NUM GUESSES:" << i << endl;	//%%%%
			return nextGuessFract;                          // return that fraction
		}
		else if ( guessZVal < zVal )		// else if guess is too low: update lowest guess
    {
			lowerFract = nextGuessFract;
			lowerZVal  = guessZVal;
		}
		else                            // else (if guess is too high):  update highest guess
		{
      upperFract = nextGuessFract;
			upperZVal  = guessZVal;
		}
	}
	
	return nextGuessFract;
}





//------------------------
//-- Takes four points for the catmull-rom equation, and through an
//-- iterative high/low guess process tries to resolve the FRACTION of all
//-- (integer/slice) z values between p1 and p2 (in ascending order)
//-- and returns this as a vector of floats.
//--
//-- Rather than call calcCardinalSplineFractAtZ (which guesses
//-- and returns a single fract value) this function records previously 
//-- guessed values so that the same guess doesn't need to be made twice.
//-- This reduces the number of times getValCardinalSpline is called by about 40%.

vector<float> calcCardinalSplineFractsEachSlice( Ipoint p0, Ipoint p1,
                                                 Ipoint p2,  Ipoint p3 )
{
	vector<float> fracts;
	
	if ( ABS(p2.z-p1.z) <=1 )   //|- if points p1 and p2 are on the same slice
    return fracts;            //|  or only one apart: return empty set
	
	int lowerZ = (int)MIN(p1.z,p2.z);				// keeps lowest  Z value of either p1 or p2
	int upperZ = (int)MAX(p1.z,p2.z);				// keeps highest Z value of either p1 or p2
	
	float lowerZVal = MIN(p1.z,p2.z);		// records Z value at the previous lowest  guess
	float upperZVal = MAX(p1.z,p2.z);		// records Z value at the previous highest guess
	
	float nextGuessFract;			// the fraction of the way along the path 
                            // between p1 and p2 of our next guess
	
	vector<IdxToSort> determinedZVals;								
  // used to store a list of previous guesses so that the same
  // values won't be fed into getValCardinalSpline twice
	determinedZVals.push_back(IdxToSort(0,lowerZVal,0));			
	determinedZVals.push_back(IdxToSort(1,upperZVal,1));			
  // we already know these values
  // NOTE: idx = NOT USED, float1 = ZVal of previous guesses,
  //        float2 = actual guess (fract between p1 and p2) )
  
	int prevLowerBoundInDeterminedList = 0;     // remembers where in the determinedZVals
                                              // list the last lowest bound was found
	
  //## FOR EACH Z VALUE BETWEEN P1 AND P2: USE AND STORE HIGH-LOW GUESSES
  //## TO RESOLVE WHERE THAT Z VALUE LIES
	
	for ( int targetZ = (lowerZ+1); (targetZ < upperZ); targetZ++ )		
    // for all previously recorded (sorted) z values:
	{
		float lowerFract = 0;			// previous lowest  guess (which value must be > than)
		float upperFract = 1;			// previous highest guess (which value must be < than)
		
		lowerZVal = lowerZ;
		upperZVal = upperZ;
		
    //## SEACH (SORTED) PREVIOUS GUESS LIST TO FIND CLOSEST GUESS ABOVE
    //## AND BELOW NEW TARGET Z VALUE
		
		for ( int d=prevLowerBoundInDeterminedList; d<int(determinedZVals.size())-1; d++ )
		{
			if( isBetweenAsc( determinedZVals[d].float1,
                        (float)targetZ,
                        determinedZVals[d+1].float1 ) )
        // if our desired z value is between these guesses:
        // set starting lowest and highest safe values
			{
				lowerZVal = determinedZVals[d].float1;				//|- (float1 is used to store 
				upperZVal = determinedZVals[d+1].float1;			//|  the ZVal of previous guesses)
				
				lowerFract = determinedZVals[d].float2;				//|- (float2 is used to store 
				upperFract = determinedZVals[d+1].float2;			//|  the guesses themselves)
				
				prevLowerBoundInDeterminedList = d;		// record where in the determinedZVals
                                              // value was found, since next z value
                                              // will be after this
				break;
			}
		}
    
    
		//## USE HIGH-LOW GUESSES TO RESOLVE A BEST-GUESS (DOESN'T NEED TO BE EXACT)
		
		for ( int i=0; i<MAX_GUESSES; i++ )
		{
			nextGuessFract = ((upperFract-lowerFract)/2.0) + lowerFract;						
      // sets next guess as halfway between highest and lowest previous safe guesses
			
			float guessZVal = getValCardinalSpline( nextGuessFract, p0.z, p1.z, p2.z, p3.z,
                                          TENSILE_FRACT );			
      // determines z value at current guessed fraction
			
			if ( isBetweenAsc((targetZ-ALLOWABLE_DEV),guessZVal,(targetZ+ALLOWABLE_DEV)) )
      {		// if estimate is within allowable deviation of desired z value: 
          //cout << " NUM GUESSES:" << i << endl;	//%%%%
				break;        // return that fraction
			}
			else if ( guessZVal < targetZ )		// else if guess is too low:  
      {			
				lowerFract = nextGuessFract;      // update lowest guess
				lowerZVal  = guessZVal;
			}
			else                              // else (if guess is too high):
			{
        upperFract = nextGuessFract;      // update highest guess
				upperZVal  = guessZVal;
        // and add to recorded guesses:
				determinedZVals.push_back( IdxToSort((int)guessZVal,guessZVal,nextGuessFract) );	
        // NOTE:  (float1 is used to store the ZVal of previous guesses)
        //        (float2 is used to store the guesses themselves)
			}
		}
		fracts.push_back( nextGuessFract );		// add this best guess to the list of fractions
		determinedZVals = vector_sort( determinedZVals );
	}
  
	return fracts;
}


















//----------------------------------------------------------------------------
//
//					TESTING FUNCTIONS:
//
//----------------------------------------------------------------------------

/*
//------------------------
//-- Prompts the user with a list of test functions he can run

void test_selector()
{
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int selectedTest = 8;
  
  CustomDialog ds("PERFORM TEST", NULL);
  ds.addRadioGrp( "test to run:",
                  "show interpolation between contours|"
                  "show interpolated lines for publication|"
                  "make contour set number of points (crude)|"
                  "cut surface through Z|"
                  "show concave points|"
                  "randomly shift contours|"
                  "copy contour to nearest end|"
                  "output analysis number of concave points|"
                  "output analysis of branches|"
                  "output analysis of tubes|"
                  "TEST: performance of tiling methods|"
                  "TEST: performance over rand contours",
                  &selectedTest );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  
  switch(selectedTest)
  {
    case(0):
      test_showInterpolationBetweenConts();
     break;
      
    case(1):
      test_showInterpolatedContoursNicely();
      break;
      
    case(2):
      test_makeContSetNumPoints();
      break;
      
    case(3):
      test_cutSurfThroughZ();
      break;
      
    case(4):
      test_showCurrContConcaveRegions();
      break;
      
    case(5):
      test_randomlyOffsetContours();
      break;
      
    case(6):
      test_copyContourToEnd();
      break;
      
    case(7):
      test_outputAnalysisOfConcavePoints();
      break;
    
    case(8):
      test_outputAnalysisOfBranches();
      break;
      
    case(9):
      test_outputAnalysisOfTubes();
      break;
      
    case(10):
      test_testTimesTilingMethodsNPoints();
      break;
      
    case(11):
      test_testTimesTilingMethodsDiffConts();
      break;
  }
  
}

//------------------------
//-- Determines which points are connected during interpolation between two
//-- contours chosen by the user (from the current object). These connections
//-- are then turned into lines, which are added to an object of the users
//-- choice allowing him to see the connections.

void test_showInterpolationBetweenConts()
{
  Imod *imod  = ivwGetModel(plug.view);
	Iobj *obj   = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
  
  int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
	if( !isContValid(cont) )
	{
		wprint("\aHave not selected valid contour\n");
		return;
	}
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int clIdx          = contIdx+1;
  int cuIdx          = contIdx+2;
  bool makeNewObj    = true;
  int objForLinesIdx = osize(imod);
  bool startSpheres  = true;
  bool flattenLines  = false;
  int zToFlattenTo   = 1;
  
  CustomDialog ds("TEST", NULL);
  ds.addSpinBox ( "LOWER cont idx:", 1, csize(obj), &clIdx, 1 );
  ds.addSpinBox ( "UPPER cont idx:", 1, csize(obj), &cuIdx, 1 );
	ds.addCheckBox ( "add connecting lines into obj", &makeNewObj );
  ds.addSpinBox  ( "object for lines:", 1, osize(imod), &objForLinesIdx, 1 );
  ds.addCheckBox ( "show first line with spheres",  &startSpheres );
	ds.addCheckBox ( "flatten all lines in this obj", &flattenLines );
  ds.addSpinBox  ( "slice to flatten lines to:", 1, plug.zsize, &zToFlattenTo, 1 );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  clIdx--;
  cuIdx--;
  objForLinesIdx--;
  zToFlattenTo--;
  
  //## PERFORM LINE CONNECTION STUFF:
  
  Icont *contLNew = imodContourNew();		// |-- version of conts with an equal number of
	Icont *contUNew = imodContourNew();		// |   points which coorspond one-to-one.
	
  ie.obj = obj;
  ie.modifyKeyContsForInterp( clIdx, cuIdx, contLNew, contUNew, true, true );
  
  printCont(contLNew);
  printCont(contUNew);
  
  //## ADD CONNECTING LINES:
  
  if( makeNewObj )
  {
    Iobj *objLines = getObj(imod,objForLinesIdx);
    
    for( int p=0; p<psize(contLNew) && psize(contUNew); p++ )
    {
      Icont *lineNew = imodContourNew();
      imodPointAppend(lineNew, getPt(contLNew,p) );
      imodPointAppend(lineNew, getPt(contUNew,p) );
      if(startSpheres && p==0)
      {
        imodPointSetSize(lineNew,0,5);
        imodPointSetSize(lineNew,1,5);
      }
      imodObjectAddContour( objLines, lineNew );
    }
  }
  
  //## FLATTEN LINES:
  
  if(flattenLines )
  {
    Iobj *objLines = getObj(imod,objForLinesIdx);
    for( int c=0; c<csize(objLines); c++ )
      setZValue( getCont( objLines, c) , zToFlattenTo  );
  }
  
  
  imodContourDelete(contLNew);
  imodContourDelete(contUNew);
}



//------------------------
//-- Takes all the interpolated contours in the current object and
//-- artificially stipples these contours by creating a series of small
//-- lines (stipples) of the chosen length and adding them to a nomiated object.
//-- The purpose of doing this is that the openGL stipple pattern looks very
//-- unnattractive and hard to see on thick lines.


//## NOTE: glLineStipple is called under "utilEnableStipple" in 
//         imod/imodutilities

// tired:   glLineStipple(3, 0xAAAA);
//          #include <qgl.h>
//          #include "../../3dmod/b3dgfx.h"


void test_showInterpolatedContoursNicely()
{
  Imod *imod  = ivwGetModel(plug.view);
	Iobj *obj   = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
  
  int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
	if( !isContValid(cont) )
	{
		wprint("\aHave not selected valid contour\n");
		return;
	}
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool stipple           = false;
  static bool stippleAlongX     = false;
  static int stippleLenOn       = 5;
  static int stippleLenOff      = 5;
  int objForLinesIdx            = osize(imod);
  static bool clearLineObj      = true;
  static bool flattenLines      = false;
  static int zToFlattenTo       = 0;
  static bool interLinesOnly    = false;
  zToFlattenTo += 1;
  
	CustomDialog ds("TEST", NULL);
  ds.addSpinBox ( "object for lines:", 1, osize(imod),  &objForLinesIdx, 1 );
	ds.addCheckBox( "clear destination obj first", &clearLineObj );
	ds.addCheckBox( "copy interpolated lines only", &interLinesOnly );
  ds.addLabel   ( "-----\n"
                  "Flattening Options:" );
	ds.addCheckBox( "flatten all lines in this obj", &flattenLines);
  ds.addSpinBox ( "slice to flatten lines to:", 1, plug.zsize, &zToFlattenTo, 1 );
  ds.addLabel   ( "-----\n"
                  "Stipple Options:" );
  ds.addCheckBox( "artificially stipple lines", &stipple );
  ds.addCheckBox( "stipple along X only", &stippleAlongX );
  ds.addSpinBox ( "stippled on length:", 1, 200, &stippleLenOn, 1 );
  ds.addSpinBox( "stippled off length:", 1, 200, &stippleLenOff, 1 );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  objForLinesIdx    -= 1;
  zToFlattenTo      -= 1;
    
  if( objIdx == objForLinesIdx )
    return;
  
  
  //## FOR EACH LINE IN CURRENT OBJECT, CREATE STIPPLED VERSION IN NEW:
  
  Iobj *objLines = getObj(imod,objForLinesIdx);
  
  if(clearLineObj)
    while( csize(objLines) > 0 )
      imodObjectRemoveContour( objLines, 0 );
  
  
  float totStippleLen = stippleLenOn + stippleLenOff;
  float fractStippleOn = fDiv( stippleLenOn, totStippleLen );
  
  
  const float denseSpacing = 0.1f;
  int numDensePtsOn = stippleLenOn / denseSpacing;
  int numDensePtsTot = totStippleLen / denseSpacing;
  
  
  for( int c=0; c<csize(obj); c++ )
  {
    Icont *cont = getCont( obj, c);
    bool closed = isContClosed(obj,cont);
    float length = imodContourLength( cont, closed );
    
    if( interLinesOnly && !isInterpolated(cont) )
      continue;
    
    if( !stipple )
    {
      Icont *newCont = imodContourDup(cont);
      imodObjectAddContour( objLines, newCont );
      continue;
    }
    
    
    Icont *contDensePts = imodContourDup( cont );
    cont_addPtsCrude(contDensePts, denseSpacing, true );
    
    if( stippleAlongX )
    {
      Ipoint ll, ur;
      imodContourGetBBox( cont, &ll, &ur );
      
      int edgeCountdown = 0;
      bool isLineOn = false;
      
      for( int p=0; p<psize(contDensePts); p++ )
      {
        Ipoint *pt = getPt(contDensePts,p);
        
        bool isPtOn = fMod( pt->x, totStippleLen) < (float)stippleLenOn;
        bool isOnEdge = (pt->x == ll.x) || (pt->x == ur.x);
        if( isOnEdge )
          edgeCountdown = numDensePtsOn;
        
        if( isPtOn || edgeCountdown > 0 )
        {
          if( isLineOn )
          {
            imodPointAppend( getLastCont(objLines), pt );
          }
          else
          {
            Icont *lineNew = imodContourNew();
            imodPointAppend( lineNew, pt );
            imodObjectAddContour( objLines, lineNew );
            isLineOn = true;
          }
        }
        else
        {
          isLineOn = false;
        }
        
        edgeCountdown--;
      }
      
      for( int c=0; c<csize(objLines); c++ )
        cont_reducePtsMinArea( getCont(objLines,c), 0.1f, true );
    }
    
    else
    {
      for( int p=0; p<psize(contDensePts); p+=numDensePtsTot )
      {
        Icont *lineNew = imodContourNew();
        imodPointAppend(lineNew, getPt(contDensePts,p) );
        imodPointAppend(lineNew, getPt(contDensePts,p+numDensePtsOn) );
        imodObjectAddContour( objLines, lineNew );
      }
    }
    
    imodContourDelete( contDensePts );
  }
  
  
  //## FLATTEN LINES:
  
  if(flattenLines )
  {
    for( int c=0; c<csize(objLines); c++ )
      setZValue( getCont( objLines, c) , zToFlattenTo  );
  }
}



//------------------------
//-- Finds and displays the concave regions and/or concave
//-- points within the curren contour.

void test_showCurrContConcaveRegions()
{
  Imod *imod  = ivwGetModel(plug.view);
	Iobj *obj   = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
  
	if( !isContValid(cont) )
	{
		wprint("\aHave not selected valid contour\n");
		return;
	}
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int objIdxConPts        = osize(imod);
  int objIdxConRegions    = osize(imod) - 1;
  int objIdxConvexHull    = osize(imod) - 2;
  static bool showConPts      = true;
  static bool showConRegions  = true;
  static bool showConvexHull  = true;
  static bool printContInfo   = true;
  static bool showUnits       = true;
  
  CustomDialog ds("TEST", NULL);;
  ds.addCheckBox( "show concave points", &showConPts );
  ds.addSpinBox ( " ...  (scattered) object for concave points:",
                  1, osize(imod), &objIdxConPts, 1 );
  ds.addCheckBox( "show concave regions", &showConRegions );
  ds.addSpinBox ( " ... (open) object for concave regions:",
                  1, osize(imod), &objIdxConRegions, 1 );
  ds.addCheckBox( "show convex hull", &showConvexHull );
  ds.addSpinBox ( " ... (closed) object for convex hull:",
                  1, osize(imod), &objIdxConvexHull, 1 );
  ds.addCheckBox( "print contour info", &printContInfo );
  ds.addCheckBox( "show units", &printContInfo );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  objIdxConRegions--;
  objIdxConvexHull--;
  objIdxConPts--;
  
  
  if( cont_isConvex(cont) )
  {
    wprint("Contour is convex\n");
    return;
  }
  
  //## PERFORM ACTION:
  
  int zCont = getZInt(cont);
  Icont *concaveMarkedVer = imodContourDup(cont);
  cont_markConvexPtsNegOne(concaveMarkedVer);
  
  if( showConvexHull )
  {
    Iobj *objConvexHull = getObj(imod,objIdxConvexHull);
    
    Icont *convexHull = imodContourDup(cont);
    cont_makeConvex(convexHull);
    addInterpolatedContToObj( objConvexHull, convexHull, 0 );
    imodContourDelete( convexHull );
  }
  
  if( showConRegions )
  {
    Iobj *objConcaveRegions = getObj(imod,objIdxConRegions);

    vector<IcontPtr> convexSegs;
    cont_breakContByZValue( concaveMarkedVer, convexSegs, zCont, true );
    
    for (int i=0; i<(int)convexSegs.size(); i++)
      addInterpolatedContToObj( objConcaveRegions, convexSegs[i].cont, 0 );
    
    deleteContours( convexSegs );
  }
  
  if( showConPts )
  {
    Iobj *objConcavePts = getObj(imod,objIdxConPts);
    
    for( int p=psize(concaveMarkedVer)-1; p>=0; p-- )
      if( getPt(concaveMarkedVer,p)->z != zCont )
        imodPointDelete( concaveMarkedVer, p );
    
    addInterpolatedContToObj( objConcavePts, concaveMarkedVer, 0 );
  }
  
  
  if( printContInfo )
  {
    bool closed = isContClosed(obj,cont);
    
    int   convexPts;
    float convexLength, hullLength, hullArea;
    
    cont_calcConvexProperties( cont, closed, &convexPts,
                               &convexLength, &hullLength, &hullArea );
    
    int pts             = psize(cont);
    int concavePts      = pts - convexPts;
    float length        = imodContourLength( cont, closed );
    float area          = imodContourArea( cont );
    float concaveLength = length - convexLength;
    float concaveArea   = hullArea - area;
    float compactness   = fDiv( length*length, area);
    
    float percentConcavePts     = calcPercent( concavePts, pts );
    float percentConvexPts      = calcPercent( convexPts,  pts );
    float percentConvexHullLen  = calcPercent( hullLength,    length );
    float percentConvexLength   = calcPercent( convexLength,  length );
    float percentConcaveLength  = calcPercent( concaveLength, length );
    float percentContourArea    = calcPercent( area,          hullArea );
    float percentConcaveArea    = calcPercent( concaveArea,   hullArea );
    
    if(showUnits)
    {
      float pixSize   = imodGetPixelSize(imod);
      float sqPixSize = pixSize * pixSize;
      
      length        *= pixSize;
      hullLength    *= pixSize;
      convexLength  *= pixSize;
      concaveLength *= pixSize;
      
      area        *= sqPixSize;
      hullArea    *= sqPixSize;
      concaveArea *= sqPixSize;
    }
    
    wprint("CONTOUR INFO:\n");
    wprint("points:     \t%d\n", pts );
    wprint(" convex  pts:  \t %d\t(%4.2f%%)\n", convexPts,  percentConvexPts );
    wprint(" concave pts:  \t %d\t(%4.2f%%)*\n", concavePts, percentConcavePts );
    wprint("length:     \t%f\n", length );
    wprint(" convex  hull: \t %f\t(%4.2f%%)\n", hullLength,    percentConvexHullLen );
    wprint(" convex  len: \t %f\t(%4.2f%%)\n",  convexLength,  percentConvexLength );
    wprint(" concave len: \t %f\t(%4.2f%%)*\n", concaveLength, percentConcaveLength );
    wprint("area hull:     \t%f\n", hullArea );
    wprint(" contour area: \t %4.2f\t(%4.2f%%)\n",  area,         percentContourArea );
    wprint(" concave area: \t %4.2f\t(%4.2f%%)*\n", concaveArea,  percentConcaveArea );
    wprint(" compactness:  \t %4.2f\n", compactness );
    
    wprint("NOTE: all units in %s\n", (showUnits) ? imodUnits(imod) : "pixels" );
  }
  
  
  imodContourDelete( concaveMarkedVer );
  undoFinishUnit( plug.view );                      // FINISH UNDO
}



//------------------------
//-- Changes the number of points in the current contour to the
//-- number of points specified by the user by calling "test_makeContSetNumPointsCrude"

void test_makeContSetNumPoints()
{
  Imod *imod  = ivwGetModel(plug.view);
	Iobj *obj   = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
  
	if( !isContValid(cont) )
	{
		wprint("\aHave not selected valid contour\n");
		return;
	}
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int nPtsBefore = psize(cont);
  int numPts = nPtsBefore;
  bool copy  = false;
  
  CustomDialog ds("TEST", NULL);
  ds.addSpinBox ( "number of points to make:", 1, 10000, &numPts, 1 );
  ds.addCheckBox( "make a copy", &copy );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  
  if( nPtsBefore == numPts )
  {
    wprint("Nothing done\n");
    return;
  }
  
  //## PERFORM ACTION:
  
  if( copy )
  {
    Icont *newCont = imodContourDup( cont );
    test_makeContSetNumPointsCrude( newCont, numPts );
    undoContourAdditionCO( plug.view, csize(obj) );   // REGISTER UNDO
    imodObjectAddContour( obj, newCont );
    undoFinishUnit( plug.view );                      // FINISH UNDO
  }
  else
  {
    undoContourDataChgCC( plug.view );                // REGISTER UNDO
    test_makeContSetNumPointsCrude( cont, numPts );
    undoFinishUnit( plug.view );                      // FINISH UNDO
  }
}

//------------------------
//-- Changes the number of points for a range of contours int the current object
//-- so that the distance between points is almost exactly the specified distance

void test_makeContSegmentsEqualDistance()
{
  if( !isCurrObjValid() )
  {
    MsgBox("Current object is not valid");
    return;
  }
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  Iobj *obj  = getCurrObj();
  int nConts = csize(obj);
  
  int contMin             = 1;
  int contMax             = nConts;
  static int distBetweenPts  = 5.0f;
  static int minAreaForConts = 0.0f;
  
  CustomDialog ds("TEST", NULL);
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour \n(inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour \n(inclusive) will be changed" );
  ds.addSpinBox ( "distance between conts:", 1, 10000, &distBetweenPts, 1 );
  ds.addSpinBox ( "min area for conts, to reduce pts", 1, 10000, &minAreaForConts, 1 );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  contMin--;
  contMax--;
  
  
  //## PERFORM ACTION:
  
  //for(int i=contMin; i<contMax && i<csize(obj); i++)
  //{
  //  undoContourDataChgCC( plug.view );                // REGISTER UNDO
  //  test_makeContSetNumPointsCrude( cont, numPts );
  //}
  //undoFinishUnit( plug.view );                      // FINISH UNDO
}


//------------------------
//-- 
//-- 
//-- 
//-- 

void test_moveCurrContToObjAndInterpolate( bool specifyObj )
{
  Imod *imod  = ivwGetModel(plug.view);
	Iobj *obj   = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
  
  int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
	if( !isContValid(cont) )
	{
		wprint("\aHave not selected valid contour\n");
		return;
	}
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int  objDestIdx = -1;
  static bool copy       = true;
  
  if( specifyObj || objDestIdx < 0 || objDestIdx >= osize(imod) )
  {
    objDestIdx = MAX(objDestIdx+1,1);
    CustomDialog ds("TEST", NULL);;
    ds.addSpinBox ( "object to move contour to:", 1, osize(imod), &objDestIdx, 1 );
    ds.addCheckBox( "make a copy", &copy );
    ds.exec();
    objDestIdx -= 1;
    if( ds.wasCancelled() )
      return;
  }
  
  if( objIdx == objDestIdx )
  {
    wprint("\aCurrent object and destination object are the same\n");
    return;
  }
  
  
  //## PERFORM ACTION:
  
  Icont *newCont = imodContourDup( cont );
  setInterpolated( newCont, 0 );
  
  if(!copy)
  {
    undoContourRemovalCO( plug.view, contIdx );         // REGISTER UNDO
    imodObjectRemoveContour( obj, contIdx );
  }
  
  imodSetIndex(imod, objDestIdx, 0, 0);
  Iobj  *objDest = getObj(imod, objDestIdx);
  undoContourAdditionCO( plug.view, csize(objDest) );   // REGISTER UNDO
  int newContIdx = imodObjectAddContour( objDest, newCont );
  wprint("Contour added to object %d\n", objDestIdx+1 );
  
  ie.performInterpolationOnCont(objDest, newContIdx, plug.interType);
  undoFinishUnit( plug.view );                          // FINISH UNDO
}

//------------------------
//-- Changes the number of points in the contour by either iteratively
//-- adding a point in the middle of the biggest gap, or iteratively
//-- deleting the point with the smallest distance to the point before and after it.
//-- 
//-- NOTE: No clever form of shape preservation (druing point removal)
//--       or smoothing (during addition) is applied.


void test_makeContSetNumPointsCrude( Icont *cont, int targetPts )
{
  while( psize(cont) < targetPts )   // need to add points:
  {
    int   idxMax           = 0;
    float maxSegSpanLength = 0;
    
    for(int p=0; p<psize(cont); p++ )
    {
      float segLen = line_distBetweenPts2D( getPt(cont,p), getPt(cont,p+1) );
      if( segLen > maxSegSpanLength )
      {
        maxSegSpanLength = segLen;
        idxMax = (p%psize(cont));
      }
    }
    
    Ipoint halfWayPt = line_getPtHalfwayBetween( getPt(cont,idxMax),
                                                 getPt(cont,idxMax+1) );
    imodPointAdd( cont, &halfWayPt, idxMax+1 );
  }
  
  
  while( psize(cont) > targetPts )   // need to remove points:
  {
    int   idxMin = 0;
    float minSpanEitherSide    = FLOAT_MAX;
    
    for(int p=0; p<psize(cont); p++ )
    {
      float spanBefore = line_distBetweenPts2D( getPt(cont,p-1), getPt(cont,p) );
      float spanAfter  = line_distBetweenPts2D( getPt(cont,p), getPt(cont,p+1) );
      float spanEitherSide = spanBefore + spanAfter;
      if( spanEitherSide < minSpanEitherSide )
      {
        minSpanEitherSide = spanEitherSide;
        idxMin = p;
      }
    }
    
    imodPointDelete( cont, idxMin );
  }
}



//------------------------
//-- Allows you to specify a line along X-Y to act like a vertical "slicing plane".
//-- For all contours in the object (across all slices) will try and determine
//-- the intersections and generate an contour in another object which 
//-- "connects the dots" and shows the shape of surfaces along the vertical axis.
//-- I created this function to help me show visualize the shape of single surfaces
//-- through a cross-section of slices, howver it is NOT at all robust! 

void test_cutSurfThroughZ()
{
  Imod *imod  = ivwGetModel(plug.view);
	Iobj *obj   = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
  
  Ipoint *pt;
  
  int ix, iy,iz;
  ivwGetLocation(plug.view, &ix, &iy, &iz);
  
	if( !isCurrObjValid() )
	{
		wprint("Have not selected valid object");
		return;
	}
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int nPtsBefore = psize(cont);
  int x1           = 0;
  int x2           = plug.xsize;
  int y1           = iy;
  int y2           = iy;
  int objLinesIdx  = osize(imod);
  bool join        = false;
  
	CustomDialog ds("TEST", NULL);
  ds.addSpinBox ( "X start:", -100, 100000, &x1, 1 );
  ds.addSpinBox ( "X end:",   -100, 100000, &x2, 1 );
  ds.addSpinBox ( "Y start:", -100, 100000, &y1, 1 );
  ds.addSpinBox ( "Y end:",   -100, 100000, &y2, 1 );
  ds.addSpinBox ( "obj new conts:", 1, osize(imod), &objLinesIdx, 1 );
	ds.addCheckBox( "try to join", &join );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  objLinesIdx -= 1;
    
  
  //## FIND INTERSECTIONS OF LINE WITH CONTOURS:
  
  Icont *contMin = imodContourNew();
  Icont *contMax = imodContourNew();
  
  Ipoint linePt1;  setPt( &linePt1, (float)x1, (float)y1, 0 );
  Ipoint linePt2;  setPt( &linePt2, (float)x2, (float)y2, 0 );
  
  int numIntercepts = 0;
  
  Ipoint intercept;
  
  for( int c=0; c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj, c);
    
    for (int p=0; p<psize(cont); p++)                // for each point/line in cont:
    {
      if( line_doLinesCrossAndWhere( getPt(cont,p), getPt(cont,p+1),
                                     &linePt1,&linePt2, &intercept ) )
                  // if lines cross: add intercept point
      {
        numIntercepts++;
        intercept.x += 1;
        
        if( !imodPointInsideCont( cont, &intercept ) )
          imodPointAppendXYZ( contMin, intercept.x - 1, intercept.y, intercept.z );
        else
          imodPointAppendXYZ( contMax, intercept.x - 1, intercept.y, intercept.z );
      }
    }
  }
  
  imodel_contour_sortz( contMin, 0, psize(contMin)-1 );
  imodel_contour_sortz( contMax, 0, psize(contMax)-1 );
  
  imodContourUnique( contMin );
  imodContourUnique( contMax );
  
  if( isEmpty(contMin) && isEmpty(contMax) )
  {
    imodContourDelete( contMin );
    imodContourDelete( contMax );
    return;
  }
  
  //## ADD CONTOURS TO SPECIFIED OBJECT:
  
  Iobj *objNew = getObj( imod, objLinesIdx );
  
  imodSetIndex(imod, objLinesIdx, csize(objNew), 0);
  
  
  if(join)
  {
    imodel_contour_invert( contMax );
    cont_copyPts( contMax, contMin, false );
    imodContourDelete( contMax );
    undoContourAdditionCO( plug.view, csize(objNew) );    // REGISTER UNDO
    imodObjectAddContour( objNew, contMin );
    
    wprint( "%d intercepts found - one contour added\n", numIntercepts );
    return;
  }
  
  
  undoContourAdditionCO( plug.view, csize(objNew) );    // REGISTER UNDO
  imodObjectAddContour( objNew, contMin );
  undoContourAdditionCO( plug.view, csize(objNew) );    // REGISTER UNDO
  imodObjectAddContour( objNew, contMax );
  undoFinishUnit( plug.view );                          // FINISH UNDO
  
  wprint( "%d intercepts found - two contours added\n", numIntercepts );
}



//------------------------
//-- Translates each contour in the current object by a random amount
//-- in X and Y. I have used this to CRUDE function to similate
//-- and test the rippling effect.

void test_randomlyOffsetContours()
{
  Imod *imod  = ivwGetModel(plug.view);
	Iobj *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
	if( !isCurrObjValid() )
	{
		wprint("Have not selected valid object");
		return;
	}
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool  seed = false;
  if(!seed)
    seedRandom();
  seed = true;
  
  static float xMaxOffset        = 2.0f;
  static float yMaxOffset        = 2.0f;
  static int   minArea           = 2000;
  static bool  dontShiftKey      = false;
  static bool  outputValsToCon   = true;
  
	CustomDialog ds("Randomly Shift Contours", NULL);
  ds.addLineEditF ( "max offset X:", 0, 10, &xMaxOffset, 3 );
  ds.addLineEditF ( "max offset Y:", 0, 10, &yMaxOffset, 3 );
  ds.addSpinBox ( "min area:", 1, 999999999, &minArea, 1,
                  "contours with < this area will NOT be randomly offset" );
	ds.addCheckBox( "don't shift key contours", &dontShiftKey );
  ds.addCheckBox( "print offsets to console", &outputValsToCon );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  //## FIND INTERSECTIONS OF LINE WITH CONTOURS:
  
  int contsShifted = 0;
  
  for( int c=0; c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj, c);
    
    if( dontShiftKey && !isInterpolated(cont) )
      continue;
    
    float area = ABS( imodContourArea( cont ) );
    if( area > minArea )
    {
      float xRand = randDbl( -xMaxOffset, xMaxOffset ); //randCoef() * xMaxOffset;
      float yRand = randDbl( -yMaxOffset, yMaxOffset ); //randCoef() * yMaxOffset;
      if( outputValsToCon )
      {
        cout << "cont " << c+1 << " \t";
        cout << "offset x:" << xRand << ", y:" << yRand << endl;   //%%%%%%%%%
      }
      
      undoContourDataChg( plug.view, objIdx, c );                // REGISTER UNDO
      cont_translate( cont, xRand, yRand );
      
      contsShifted++;
    }
  }
  
  undoFinishUnit( plug.view );                          // FINISH UNDO
  
  wprint( "%d contours shifted\n", contsShifted );
}


//------------------------
//-- Takes the current contour, and add interpolated duplicates of it
//-- upwards or downwards to the top-most slice or bottom-most slice,
//-- depending which is closer.
//-- You can use this VERY CRUDE function to quickly fill in missing slices at the
//-- top and bottom of tomograms where it's difficult to see the membrane by eye.

void test_copyContourToEnd()
{
  Imod *imod  = ivwGetModel(plug.view);
	Iobj *obj   = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
	if( !isCurrContValid() )
	{
		wprint("Have not selected valid contour");
		return;
	}
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static float scaleRate     = 0.9f;
  static bool  reduceScale   = false;
  
	CustomDialog ds("Copy Contour To End", NULL);;
  ds.addLineEditF ( "scale rate:", 0, 10, &scaleRate, 3 );
	ds.addCheckBox  ( "reduce scale subsequent slices", &reduceScale );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  //## FIND INTERSECTIONS OF LINE WITH CONTOURS:
  
  Ipoint centerMBR;
  cont_getCenterOfMBR(cont, &centerMBR);
  int contZ = getZInt(cont);
  bool copyUp = contZ > (plug.zsize / 2);
  int copyDir = (copyUp) ? 1 : -1;
  
  int numContAdded = 0;
  for( int z = contZ + copyDir; z>=0 && z<plug.zsize; z += copyDir )
  {
    Icont *contNew = imodContourDup( cont );
    float scale = pow( scaleRate, (numContAdded+1) );
    if( reduceScale )
      cont_scaleAboutPtXY( contNew, &centerMBR, scale, scale );
    setZValue( contNew, z );
    addInterpolatedContToObj( obj, contNew );
    numContAdded++;
  }
  
  if(numContAdded)
    undoFinishUnit( plug.view );                          // FINISH UNDO
  
  wprint( "%d contours added\n", numContAdded );
}


//------------------------
//-- Outputs the number of concave points for each contour
//-- over the user-specified range of objects

void test_outputAnalysisOfConcavePoints()
{
  Imod *imod = ivwGetModel(plug.view);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool printAllConts     = true;
  int minObj         = 1;
  int maxObj         = osize(imod);
  
	CustomDialog ds("TEST", NULL);;
  ds.addSpinBox ( "min object:",1,osize(imod),&minObj,1 );
  ds.addSpinBox ( "max object:",1,osize(imod),&maxObj,1 );
 	ds.addCheckBox( "print all contours", &printAllConts );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  minObj--;
  maxObj--;
  
  
  //## COUNT NUMBER OF CONCAVE POINTS FOR EACH CONTOUR:
  
  cout << endl << "ASSESSMENT OF CONTOUR COMPLEXITY:" << endl;
  cout << endl << "OBJ,CONT,PTS,CONCAVE_PTS,FRACT_CONCAVE,"
                  "LENGTH,CONVEX_LENGTH,CONVEX_HULL_LENGTH,"
                  "FRACT_CONCAVE_LENGTH,COMPACTNESS,"
                  "AREA,CONVEX_HULL_AREA,FRACT_CONCAVE_AREA" << endl;
  
  for (int o=minObj; o<MIN(maxObj+1,osize(imod)); o++ )   // for each object
  {
    Iobj *obj = getObj(imod,o);
    
    int totPts             = 0;
    int totConcavePts      = 0;
    float totLength        = 0;
    float totConvexLength  = 0;
    float totHullArea      = 0;
    float totConcaveArea   = 0;
    
    cout << endl << "OBJECT " << o+1 << ": " << imodObjectGetName(obj) << endl;
    
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
        cout << o+1 << "," << c+1 << ","
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
      
      cout << endl;
      cout << "TOTAL POINTS:         \t" << totPts << endl;
      cout << "TOTAL CONCAVE PTS:    \t" << totConcavePts << endl;
      cout << "FRACTION CONCAVE PTS:    \t" << totPercentConcavePts << "%"<< endl;
      cout << "FRACTION CONCAVE LENGTH: \t" << totPercentConcaveLen << "%"<< endl;
      cout << "FRACTION CONCAVE AREA  : \t" << totPercentConcaveArea << "%"<< endl<< endl;
    }
    else
    {
      cout << "EMPTY" << endl;
    }
  }
  
  cout << "TESTS FINISHED" << endl;  
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

vector<int> test_findBranches( Iobj *obj, Icont *contRoot, float maxDistBranch,
                               float zScale )
{
  Ipoint scale;
  scale.x = 1.0f;
  scale.y = 1.0f;
  scale.z = zScale;
  
  vector<int> branches;
  
  for(int c=1; c<csize(obj); c++)             // for each contour in objB:
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



void test_outputAnalysisOfBranches()
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
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int objMainLen         = 10;
  int objBranches        = 11;
  static int  branchDepth       = 2;
  static int  maxDistBranch     = 10;
  static bool addRadiusToLen    = true;
  static bool convertUnits      = true;
  static bool showMatches       = true;
  static int  outputToFile      = 0;
  
	CustomDialog ds("TEST - BRANCH ANALYSIS", NULL);
  ds.addSpinBox ( "object to analyse:",1,osize(imod),&objMainLen,1,
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
  ds.addCheckBox( "convert to appropriate units", &convertUnits,
                  "Will show results in nm (or whatever you've entered \n"
                  "in the model header) instead of pixels" );
  ds.addCheckBox( "show matches", &showMatches,
                  "Prints a list of all contours in 'object containing branches' \n"
                  "showing how many times each was detected as a branch" );
  ds.addRadioGrp( "output to:",
                  "console|"
                  "3dmod window",
                  &outputToFile );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  objMainLen--;
  objBranches--;
  bool  sameObj     = (objMainLen == objBranches);
  float scaleValues = (convertUnits) ? pixelSize : 1;
  
  Iobj *objM = getObj(imod,objMainLen);
  Iobj *objB = getObj(imod,objBranches);
  
  if( csize(objM)==0 || csize(objB)==0 )
  {
    MsgBox( "Specified object must have contours!" );
    return;
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
        vector<int> contBranches = test_findBranches(objB,cont,maxDistBranch,zScale);
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
  double totalRTimeLength = 0;
  
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
    return;
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

  
  string bigOutputString = out.str();
  if( outputToFile==0 )
  {
    cout << bigOutputString;
  }
  else if ( outputToFile==1 )
  {
    wprint("%s", bigOutputString.c_str() );
    wprint("---");
  }
  
  return;
}




//------------------------
//-- Outputs information regarding the length, volume and surface area of
//-- variable width tubes.
//-- NOTE: Such tubes can be rendered with:
//--         imodmesh -o 1 -t 1 -d -1 model_file.mop

void test_outputAnalysisOfTubes()
{
  Imod *imod = ivwGetModel(plug.view);
  int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  float pixelSize = imodGetPixelSize(imod);
  float zScale    = imodGetZScale(imod);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int minObj         = 9;
  int maxObj         = 11;
  static bool printAllConts     = true;
  static bool convertUnits      = true;
  static int  outputToFile      = 0;
  
	CustomDialog ds("TEST - TUBE ANALYSIS", NULL);
  ds.addSpinBox ( "min object:",1,osize(imod),&minObj,1 );
  ds.addSpinBox ( "max object:",1,osize(imod),&maxObj,1 );
 	ds.addCheckBox( "print all open contours", &printAllConts );
  ds.addCheckBox( "convert to appripriate units", &convertUnits );
  ds.addRadioGrp( "output to:",
                  "console|"
                  "csv file",
                  &outputToFile );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  minObj--;
  maxObj--;
  
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
  
  string bigOutputString = out.str();
  if( outputToFile==0 )
  {
    cout << bigOutputString;
  }
  else if ( outputToFile==1 )
  {
    //## PROMPT USER FOR .CSV FILE TO SAVE OUTPUT
    QString qname = QFileDialog::getSaveFileName(plug.window,"Save File",
                                                "","CSV file (*.csv)");
    if ( qname==NULL )
      return;
    
    string filePath = qStringToString(qname);
    
    ofstream out_file( filePath.c_str() );								//open/create text file
    if (out_file.fail())						// ERROR CHECKING
    {
      MsgBox("ERROR: Could not create and/or open file");
      return;
    }
    out_file << bigOutputString;
    out_file.close();										//close text file
  }
  
  return;
}


//------------------------
//-- Erases the previous line and prints over the top a progress
//-- indicating the percentage along 'numDone' is towards 'totNum'

inline void printProgressLine( string prefix, int numDone, int totNum, int modIncr=1 )
{
  if(numDone % modIncr == 0) {
    int percentDone = calcPercentFloor(numDone, totNum);
    wprint("%s %d / %d  (%d%%)\r", prefix.c_str(), numDone, totNum, percentDone);
    QApplication::flush();
  }
}


//------------------------
//-- Records and outputs the time elapsed in milliseconds for the chosen
//-- tiling method ('tilingMethod') and point resolution method ('ptResolveMethod')
//-- to connect points between the two given contours over a given number of
//-- 'iterations'.... or timeout period in seconds - whichever comes first

inline void test_printTimesTilingMethods( int clIdx, int cuIdx, Icont *cL, Icont *cU,
                                         int iterations, int timoutSecs,
                                         int tilingMethod, int ptResolveMethod,
                                         string methStr, int printProg, bool outWindow,
                                         bool outConsole=true )
{
  int numPtsL = psize(cL);
  int numPtsU = psize(cU);
  int numPtsTot = numPtsL + numPtsU;
  
  int timeoutMSecs = timoutSecs * 1000;
  
  plug.ptResolveMethod = ptResolveMethod;
  
  //## EXECUTE AND TIME TEST OVER THE GIVEN NUMBER OF "ITERATIONS":
  
  QTime time;
  time.restart();
  
  for(int i=1; i<=iterations; i++)
  {
    Icont *contLNew = imodContourNew();    // NOTE: If I just default the contour
    Icont *contUNew = imodContourNew();    //   each iteration it leads to a crash.
    
    if( tilingMethod == TM_CONSERVE_LENGTH )
      ie.modifyKeyContsForInterp_LineConservation(cL,cU,contLNew,contUNew,true,true);
    else
      ie.modifyKeyContsForInterp_MinimumAreaCost(clIdx,cuIdx,contLNew,contUNew);
        
    if(printProg)
      printProgressLine(methStr + ": ",i,iterations,100);
    
    imodContourDelete(contLNew);
    imodContourDelete(contUNew);
    
    if (time.elapsed() > timeoutMSecs)
    {
      iterations = i;
      break;
    }
  }
  
  int msecsElap = time.elapsed();
  float msecsPerIt = fDiv( msecsElap, iterations );
  
  
  //## CALCULATE THE LENGTH AREA AND OTHER STATISTICS:
  
  Icont *contLNew = imodContourNew();
  Icont *contUNew = imodContourNew();
  if( tilingMethod == TM_CONSERVE_LENGTH )
    ie.modifyKeyContsForInterp_LineConservation(cL,cU,contLNew,contUNew,true,true);
  else
    ie.modifyKeyContsForInterp_MinimumAreaCost(clIdx,cuIdx,contLNew,contUNew);
  
  Icont *contMiddle = imodContourDup(contLNew);
  for( int p=0; p<psize(contMiddle) && p<psize(contUNew); p++ )
    *getPt(contMiddle,p) = line_getPtHalfwayBetween( getPt(contLNew,p),
                                                     getPt(contUNew,p) );
  
  int resultingPts = psize(contLNew);
    
  float lengthP = imodContourLength( cL, true );
  float lengthQ = imodContourLength( cU, true );
  float lengthMiddle = imodContourLength( contMiddle, true );
  
  float areaP = imodContourArea( cL );
  float areaQ = imodContourArea( cU );
  float areaMiddle = imodContourArea( contMiddle );
  
  imodContourDelete(contLNew);
  imodContourDelete(contUNew);
  imodContourDelete(contMiddle);
  
  //## OUTPUT RESULTS:
  
  if( outConsole )
  {
    cout << numPtsTot <<","<< numPtsL <<","<< numPtsU <<","<< resultingPts <<","<<","
      << methStr <<","<< iterations <<","<< msecsElap <<","<< msecsPerIt <<","<<","
      << lengthP <<","<< lengthQ <<","<< lengthMiddle <<","
      << areaP << "," << areaQ <<","<< areaMiddle << ","<<","
      << clIdx <<","<< cuIdx << endl;
    cout.flush();
  }
  if( outWindow )
  {
    wprint("%s:\n", methStr.c_str());
    wprint("   > pts: n=%d, m=%d, resulting=%d\n", numPtsL, numPtsU, resultingPts);
    wprint("   > time: elap=%ds, its=%d, ms/it=%f\n",
           int(msecsElap/1000), iterations, msecsPerIt);
    QApplication::flush();
  }
}



//------------------------
//-- Test the performance in CPU time of three different tiling methods to
//-- connect points between two user specified key contours. 
//-- Amoung other options the user can change the number of iterations per test,
//-- the number of points between each contour, and the results are outputted
//-- to the console window.


void test_testTimesTilingMethodsNPoints()
{
  Imod *imod  = ivwGetModel(plug.view);
	Iobj *obj   = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
  
  int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
	if( !isContValid(cont) )
	{
		wprint("\aHave not selected valid contour\n");
		return;
	}
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int clIdxOrig    = contIdx+1;
  int cuIdxOrig    = contIdx+2;
  static int iterations      = 1000;
  static bool randomStartPts = false;
  static bool omitSA         = false;
  static int  timeoutS       = 60;
  static int  testStrategy   = 0;
  static int minPts          = 4;
  static int maxPts          = 100;
  static int incrementPts    = 1;  
  
	CustomDialog ds("TEST", NULL);
  ds.addSpinBox ( "LOWER cont idx:",1,csize(obj),&clIdxOrig,1);
  ds.addSpinBox ( "UPPER cont idx:",1,csize(obj),&cuIdxOrig,1 );
  ds.addSpinBox ( "iterations:",1,1000000000,&iterations,100);
  ds.addSpinBox ( "timeout in seconds:",1,6000,&timeoutS,10 );
 	ds.addCheckBox( "select start points randomly", &randomStartPts );
 	ds.addCheckBox( "omit surface area metric", &omitSA );
  ds.addRadioGrp( "test contours:",
                  "as they are|"
                  "adjust # of points using preset|"
                  "adjust # of points as below:",
                  &testStrategy );
  ds.addLabel   ( "-----\n"
                  "TEST USING NUMBER OF POINTS:");
  ds.addSpinBox ( "min # points each key", 1,10000, &minPts, 1 );
  ds.addSpinBox ( "max # points each key", 1,10000, &maxPts, 1 );
  ds.addSpinBox ( "# points increment",    1,10000, &incrementPts,1);
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  clIdxOrig    -= 1;
  cuIdxOrig    -= 1;
  
  
  //## VERIFY CONTOURS:
  
  Icont *contLOrig = getCont(obj,clIdxOrig);
  Icont *contUOrig = getCont(obj,cuIdxOrig);
  
  if( isEmpty(contLOrig) || isEmpty(contUOrig) ) {
    wprint("\aOne or both contours are empty\n");
    return;
  }
  if( clIdxOrig == cuIdxOrig ) {
    wprint("\aContours must have different index values\n");
    return;
  }
  if( getZ(contLOrig) >= getZ(contUOrig) ) {
    wprint("\aThe lower contour does not have a lower Z value\n");
    return;
  }
  
  
  wprint("PERFORMING TESTS...\n");
  cout << endl << "RUNNING TESTS ON CONTS: " 
    << clIdxOrig+1 << " and " << cuIdxOrig+1 << endl;
  cout << "N" <<","<< "n" <<","<< "m" <<","<< "nOut" <<","<<","
    << "method" <<","<< "iterations" <<","<< "msecsTot" <<","<< "msPerIt" <<","<<","
    << "lengthP" <<","<< "lengthQ" <<","<< "lengthMiddle" <<","
    << "areaP" << "," << "areaQ" <<","<< "areaMiddle" <<","<<","
    << "clIdx" <<","<< "cuIdx" << endl;
  
  
  //## PERFORM TESTS ON CONTOURS AS THEY AREA:
  
  if( testStrategy == 0 )        // test contours "as they are"
  {
    ie.obj = obj;
    
    test_printTimesTilingMethods( clIdxOrig,cuIdxOrig,contLOrig,contUOrig,
                                  iterations*5, timeoutS, TM_CONSERVE_LENGTH,PT_FOUR_PTS,
                                  "line_four_pts", 500, true );
    
    test_printTimesTilingMethods( clIdxOrig,cuIdxOrig,contLOrig,contUOrig,
                                  iterations*5, timeoutS, TM_CONSERVE_LENGTH,PT_CONVEX_PTS,
                                  "line_convex_pts", 500, true );
    
    if(!omitSA)
    test_printTimesTilingMethods( clIdxOrig,cuIdxOrig,contLOrig,contUOrig,
                                  iterations, timeoutS, TM_MIN_SA,0,
                                  "area", 100, true );
    
    wprint("TESTS FINISHED\n");
    cout << endl << "TESTS FINISHED" << endl;
    
    return;
  }
  
  
  //## PERFORM TESTS ON CONTOURS BY MODIFYING NUMBER OF POINTS EACH ROUND:
  
  vector<int> numberPtsForTest;
  
  if( testStrategy == 1 )          // test contours using "preset"
  {
    for (int n=4; n<100; n+=1 )
      numberPtsForTest.push_back( n );
    //for (int n=100; n<200; n+=2 )
    //  numberPtsForTest.push_back( n );
    for (int n=100; n<=500; n+=10 )
      numberPtsForTest.push_back( n );
  }
  else if( testStrategy == 2 )     // test contours using options specified by user
  {
    for (int n=minPts; n<=maxPts; n+=incrementPts )
      numberPtsForTest.push_back( n );
  }
  int numTests   = (int)numberPtsForTest.size();
  
  
  QTime time;
  QTime timeTot;
  timeTot.start();
  
  for (int t=0; t<numTests; t++)
  {
    int numPts = numberPtsForTest[t];  // number of points to use in both key contours
    
    //## CREATE NEW VERSION OF CONTOURS IN THE NEW OBJECT WITH N POINTS EACH:
    
    Icont *contL = imodContourDup(contLOrig);
    Icont *contU = imodContourDup(contUOrig);
    
    test_makeContSetNumPointsCrude( contL, numPts );
    test_makeContSetNumPointsCrude( contU, numPts );
    
    Iobj* objNew = imodObjectNew();
    
    int clIdx = imodObjectAddContour(objNew,contL);
    int cuIdx = imodObjectAddContour(objNew,contU);
    
    int numPtsL = psize(contL);
    int numPtsU = psize(contU);
    
    if( randomStartPts )
    {
      seedRandom();
      int newStartIdxContL = randIntInclusive( 0, numPtsL-1 );
      int newStartIdxContU = randIntInclusive( 0, numPtsU-1 );
      cont_reorderPtsToStartAtIdx( contL, newStartIdxContL );
      cont_reorderPtsToStartAtIdx( contU, newStartIdxContU );
    }
    
    
    //## RUN TESTS:
    
    ie.obj = objNew;
    
    test_printTimesTilingMethods( clIdx,cuIdx,contL,contU,
                                  iterations, timeoutS, TM_CONSERVE_LENGTH,PT_FOUR_PTS,
                                  "line_four_pts", 0, false );
    
    test_printTimesTilingMethods( clIdx,cuIdx,contL,contU,
                                  iterations, timeoutS, TM_CONSERVE_LENGTH,PT_CONVEX_PTS,
                                  "line_convex_pts", 0, false );
    
    if(!omitSA)
    test_printTimesTilingMethods( clIdx,cuIdx,contL,contU,
                                  iterations, timeoutS, TM_MIN_SA,0,
                                  "area", 0, false );
    
    imodObjectDelete(objNew);
    
    printProgressLine( "Tests done: ", t+1, (int)numberPtsForTest.size() );
  }
  
  cout << endl << "TESTS FINISHED" << endl;
  
  int secsElapsedTot = floor( timeTot.elapsed() / 1000.0 );
  int minsElapsed = floor( secsElapsedTot / 60.0 );
  int secsElapsed = secsElapsedTot - ( minsElapsed*60 );
  
  wprint("TESTS FINISHED\n");
  wprint(" NOTE: results printed to console \n");
  wprint(" > contours sets tested: %d\n", numTests);
  wprint(" > iterations per test: %d\n", iterations);
  wprint(" > total time elapsed: %d mins %d secs\n", minsElapsed, secsElapsed );
}


//------------------------
//-- Test the performance in CPU time of three different tiling methods to
//-- connect points between two user specified key contours. 
//-- Amoung other options the user can change the number of iterations per test,
//-- the number of points between each contour, and the results are outputted
//-- to the console window.


void test_testTimesTilingMethodsDiffConts()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  int objIdx, contIdx, ptIdx;
	imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
	if( !isCurrObjValid() )
	{
		wprint("Have not selected valid object");
		return;
	}
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int iterations      = 100;
  static bool useAllObjs     = true;
  static int  timeoutS       = 60;
  static int minSepZ         = 10;
  static int maxSepZ         = 15;
  static int numTests        = 10;
  static int MINPTS          = 5;
  
	CustomDialog ds("TEST", NULL);
  ds.addSpinBox ( "iterations:",1,1000000000,&iterations,100);
  ds.addSpinBox ( "timeout in seconds:",1,6000,&timeoutS,10 );
 	ds.addCheckBox( "use all objects", &useAllObjs );
  ds.addLabel   ( "-----\n"
                  "CONTOURS TO USE:");
  ds.addSpinBox ( "min sep Z", 1,10000, &minSepZ, 1 );
  ds.addSpinBox ( "max sep Z", 1,10000, &maxSepZ, 1 );
  ds.addSpinBox ( "# test (contour pairs)",    1,10000, &numTests,1);
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  //## PERFORM TESTS ON CONTOURS BY MODIFYING NUMBER OF POINTS EACH ROUND:
  
  vector<IntAndInt> candidateConts;
  
  for (int o=0; o<osize(imod); o++ )
  {
    if( !useAllObjs && o!=objIdx )
      continue;
    Iobj *objO = getObj(imod,o);
    if( !isObjClosed(objO) )
      continue;
    for (int c=0; c<csize(objO); c++)
    {
      if( psize(getCont(objO,c)) >= MINPTS )
        candidateConts.push_back( IntAndInt(o,c) );
    }
  }
  
  //for(int i=0; i<(int)candidateConts.size();i++)
  //  cout << "i=" << i << " obj=" << candidateConts[i].idx1 << " cont=" << candidateConts[i].idx2 << endl;    //%%%%%%
  
  cout << "NUMBER CANDIDATE CONTOURS = " << (int)candidateConts.size() << endl;
  
  
  //## PERFORM TESTS:
  
  wprint("PERFORMING TESTS...\n");
  
  seedRandom();
  
  int numFailedMatches = 0;
  
  QTime time;
  QTime timeTot;
  timeTot.start();
  
  for (int t=0; t<numTests && (int)candidateConts.size() > 1; t++)
  {
    //## SELECT RANDOM CONTOUR, AND TRY TO FIND A RANDOM MATCH:
    
    int randCandIdx = randIntInclusive(0, (int)candidateConts.size()-1 );
    int objOIdx = candidateConts[randCandIdx].idx1;
    Iobj *objO = getObj(imod,objOIdx);
    
    int c1Idx  = candidateConts[randCandIdx].idx2;
    int c2Idx  = -1;
    int c1Z    = getZInt( getCont(objO, c1Idx) );
    int c2Z    = -1;
    Icont *cont1 = getCont(objO,c1Idx);
    
    int randStartCont  = randIntInclusive(0, csize(objO)-1 );
    bool matchFound = false;
    
    for (int c=0; c<csize(objO); c++)
    {
      c2Idx =  (c+randStartCont) % csize(objO);
      Icont *cont2 = getCont(objO,c2Idx);
      if( psize(cont2) >= MINPTS )
      {
        c2Z = getZInt( cont2 );
        int sepZ = ABS( c2Z - c1Z );
        if( minSepZ <= sepZ  && sepZ <= maxSepZ && c2Idx != c1Idx )
        {
          if( ie.contoursSameSurfSlow( cont1, cont2 ) )
          {
            matchFound = true;
            break;
          }
        }
      }
    }
    if( !matchFound )   // if no matching c2 was found: try again (try new c1Idx)
    {
      cout << "No match for obj " << objOIdx+1 << " cont "  << c1Idx+1 << endl;
      numFailedMatches++;
      candidateConts.erase( candidateConts.begin() + randCandIdx );
      t--;
      continue;
    }
    
    
    ie.obj = objO;
    int clIdx = (c1Z < c2Z) ? c1Idx : c2Idx;
    int cuIdx = (c1Z < c2Z) ? c2Idx : c1Idx;
    Icont* contL = getCont( objO, clIdx );
    Icont* contU = getCont( objO, cuIdx );
    
    //## RUN TESTS:
    
    cout << endl << "RUNNING TESTS ON OBJ " << objOIdx+1 << " CONTS: " 
      << clIdx+1 << " (z=" << MIN(c1Z,c2Z) << ") and "
      << cuIdx+1 << " (z=" << MAX(c1Z,c2Z) << ")" << endl;
    cout << "N" <<","<< "n" <<","<< "m" <<","<< "nOut" <<","<<","
      << "method" <<","<< "iterations" <<","<< "msecsTot" <<","<< "msPerIt" <<","<<","
      << "lengthP" <<","<< "lengthQ" <<","<< "lengthMiddle" <<","
      << "areaP" << "," << "areaQ" <<","<< "areaMiddle" <<","<<","
      << "clIdx" <<","<< "cuIdx" << endl;
    
    test_printTimesTilingMethods( clIdx,cuIdx,contL,contU,
                                  iterations, timeoutS, TM_CONSERVE_LENGTH,PT_FOUR_PTS,
                                  "line_four_pts", 0, false );
    
    test_printTimesTilingMethods( clIdx,cuIdx,contL,contU,
                                  iterations, timeoutS, TM_CONSERVE_LENGTH,PT_CONVEX_PTS,
                                  "line_convex_pts", 0, false );
    
    test_printTimesTilingMethods( clIdx,cuIdx,contL,contU,
                                  iterations, timeoutS, TM_MIN_SA,0,
                                  "area", 0, false );
    
    printProgressLine( "Tests done: ", t+1, numTests );
  }
  
  cout << endl << "TESTS FINISHED" << endl;
  
  int secsElapsedTot = floor( timeTot.elapsed() / 1000.0 );
  int minsElapsed = floor( secsElapsedTot / 60.0 );
  int secsElapsed = secsElapsedTot - ( minsElapsed*60 );
  
  wprint("\nTESTS FINISHED\n");
  wprint(" NOTE: results printed to console \n");
  wprint(" > candidate contours: %d\n", (int)candidateConts.size() );
  wprint(" > disqualified contours: %d\n", numFailedMatches );
  wprint(" > contours sets tested: %d\n", numTests);
  wprint(" > iterations per test: %d\n", iterations);
  wprint(" > total time elapsed: %d mins %d secs\n", minsElapsed, secsElapsed );
  
  if( (int)candidateConts.size() <= 1 )
  {
    wprint("\aALL CONTOURS WERE DISQUALIFIED\n");
    cout << "TOO MANY CONTOURS DISQUALIFIED" << endl;
  }
}

 */

