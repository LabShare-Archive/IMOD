/*
 *  beadhelper.c -- Special plugin for fiducial tracking helper tools
 *                  to compliment those in BeadFixer
 */

/*****************************************************************************
 *   Copyright (C) 2007 by Andrew Noske from the Institute for Molecular     *
 *   Bioscience at the University of Queensland (Australia)                  *
 *****************************************************************************/

/*  $Author$

    $Date$

    $Revision$

    Revision 0.0  2008/2/25 15:45:41  noske
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
#include <qvbuttongroup.h>
#include <qradiobutton.h>
#include <qdialog.h>
#include <qspinbox.h>
#include <qlayout.h>
#include <qgroupbox.h>
#include <qtooltip.h>
#include <qstringlist.h>
#include <qmessagebox.h>
#include <qinputdialog.h>

#include "_common_functions.h"
#include "qt_dialog_customizable.h"
#include "imodplugin.h"
#include "dia_qtutils.h"
#include "beadhelper.h"

//############################################################

static BeadHelperData plug = { 0, 0 };

//############################################################



//----------------------------------------------------------------------------
//
//          MAPPED FUNCTIONS:
//
//----------------------------------------------------------------------------



//------------------------
//-- MAPPED FUNCTION: Called by the imod plugin load function

char *imodPlugInfo(int *type)
{
  if (type)
    *type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS + IMOD_PLUG_MESSAGE + 
      IMOD_PLUG_MOUSE + IMOD_PLUG_EVENT;
    
  return("Bead Helper");
}

//------------------------
//-- MAPPED FUNCTION: Grab hotkey input. return 1 if we handle the key.
 
int imodPlugKeys(ImodView *vw, QKeyEvent *event)
{
  if (!plug.view)          // if plugin window isn't open: don't grab keys
    return 0;
  
  if( plug.disableHotKeys )
    return 0;
  
  int keysym = event->key();            // key value (Key_A, Key_Space... etc)
  int ctrl    = event->state() & Qt::ControlButton;   // ctrl modifier
  int shift   = event->state() & Qt::ShiftButton;     // shift modifier
  
  switch(keysym)
  {
    case Qt::Key_E:
      if (!shift)
      {
        plug.window->moveCurrPtToEstimatedPos();
        plug.window->drawExtraObject(true);
      }
      else
      {
        plug.window->movePtsToEstimatedPosCurrCont();
        plug.window->drawExtraObject(true);
      }
      break;
      
    case Qt::Key_F:                  // fill points in current contour
      plug.window->fillMissingPtsCurrCont( shift );
      break;
      
    case Qt::Key_D:                  // delete points current contour to nearest end
      if (shift)
        return 0;
      plug.window->deletePtsUsingDAction();
      break;
      
    case Qt::Key_R:                  // reduce current point to seed
      if (shift)
        return 0;
      else
        plug.window->reduceCurrContToSeed();
      break;
      
    case Qt::Key_M:                  // move contour
      if (shift)
        plug.window->moveContour();
      else
        return plug.window->applyMAction();
      break;
      
    case Qt::Key_H:                  // go to next largest hole
      bead_goToNextBiggestHole( !shift );
      break;
      
    case Qt::Key_Y:                 // go to next largest y jump
      if(ctrl)
        return 0;
      bead_goToNextBiggestYJump( !shift );
      break;
      
    case Qt::Key_W:                  // go to next biggest weighted deviation
      if(ctrl)
        return 0;
      bead_goToNextBiggestWeightedDev( !shift );
      break;
      
    case Qt::Key_B:                  // go to next biggest deviation from estimated
      if(ctrl)
        return 0;
      bead_goToNextBiggestDev( !shift );
      break;
      
    case Qt::Key_O:                  // go to contour with next biggest sort value
      bead_goToContNextBiggestSortVal( !shift );
      break;
      
    case Qt::Key_U:                  // toggle contour as checked/unchecked
      if(shift)
        plug.window->printContourCheckedInfo();
      else
        plug.window->toggleStippled();
      break;
      
    case Qt::Key_Enter:
    case Qt::Key_Return:
      plug.window->enterActionIterateConts( shift );
      break;
      
    //## LIST OF KEYS THAT REQUIRE A REDRAW OF THE EXTRA OBJECT:
      
    case Qt::Key_PageUp:
      edit_changeSelectedView(1,true);
      break;
    case Qt::Key_PageDown:
      edit_changeSelectedView(-1,true);
      break;
      
    case Qt::Key_Insert:               // doesn't work
      plug.window->goToSeedView();
      break;
      
    
    case Qt::Key_T:                  // temporary testing purposes - comment out %%%%
      if (shift)
        plug.window->test();
      else
        return 0;
      break;
    
      
    default:
      return 0;       // let imod deal with action
  }
  
  return 1;         // have dealt with action
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
  
  plug.view = inImodView;
  ivwTrackMouseForPlugs(plug.view, 1);
  ivwEnableStipple( plug.view, 1 );
  
  if( !plug.initialized )
  {
    ivwGetImageSize(inImodView, &plug.xsize, &plug.ysize, &plug.zsize);
    plug.middlePt.x = plug.xsize / 2;
    plug.middlePt.y = plug.ysize / 2;
    plug.middlePt.z = plug.zsize / 2;
    
    //** MAIN OPTIONS:
    
    plug.viewMin           = 1;
    plug.viewMax           = plug.zsize;
    plug.contMin            = 5;
    plug.contMax            = 9999;
    
    plug.showExpectedPos    = 1;
    plug.showSpheres        = true;
    plug.sphereSize         = 3;
    plug.lineDisplayType    = LD_ALL;
    plug.tiltDisplayType    = TD_OFF;
    plug.estPosMethod       = EM_NEARESTTWO;
        
    //** MORE SETTINGS:
    
    plug.seedView              = int(plug.zsize / 2);
    plug.tiltIncrement         = 1.0;
    plug.tiltAxisAngle         = -11.7;
    plug.tiltOffsetX           = 0;
    plug.biggestHoleGrid       = 20;
    plug.biggestHoleInset      = -100;
    
    plug.expPtDisplayType      = ED_CROSS;
    plug.expPtSize             = 6;
    plug.sizeLineSpheres       = 0;
    plug.lineDisplayWidth      = 1;
    plug.sizePurpleSpheres     = 10;
    
    plug.selectedAction        = 0;
    plug.sortCriteria          = 0;
    plug.autoSaveSettings      = true;
    
    //** KEYBOARD AND MOUSE OPTIONS:
    
    plug.wheelBehav            = WH_SMART;
    plug.wheelResistance       = 100;
    plug.disableHotKeys        = false;
    plug.includeEndsResid      = false;
    plug.searchRangeOnly       = false;
    plug.contsToSearch         = SC_ALL;
    plug.dKeyBehav             = DK_OPPOSITEMIDDLE;
    plug.mKeyBehav             = MK_GOTOMIDDLE;
    plug.wCurrContOnly         = true;
    plug.wWeightedDiv          = 15;
    plug.enterAction           = EN_PREVUNCHECKED;
    plug.minPtsEnter           = 5;
    plug.maxPtsEnter           = plug.zsize;
    plug.enterPrint            = false;
    
    //** SMOOTHING OPTIONS:
    
    plug.smoothCurrContOnly    = 1;
    plug.smoothFillGaps        = true;
    plug.smoothBigResidFirst   = false;
    plug.smoothMoveYOnly       = false;
    plug.smoothLeaveSeed       = true;
    plug.smoothLeaveEnds       = true;
    plug.smoothLeaveCurrV      = false;
    plug.smoothMoveFract       = 1.0;
    plug.smoothMinResid        = 0;
    plug.smoothIterations      = 1;
    plug.smoothAdjacentV       = false;
    plug.smoothNumViews        = 5;
    
    //** OTHER:
    
    plug.window->loadSettings();
    plug.initialized        = true;
  }
  
  
  //## SET UP EXTRA OBJECTS:
  
  plug.extraObjExtra       = ivwGetFreeExtraObjectNumber(plug.view);
  plug.extraObjTiltAxis    = ivwGetFreeExtraObjectNumber(plug.view);
  plug.extraObjContDisp    = ivwGetFreeExtraObjectNumber(plug.view);
  plug.extraObjExpPos      = ivwGetFreeExtraObjectNumber(plug.view);
  
  Iobj *xobjE = ivwGetAnExtraObject(plug.view, plug.extraObjExpPos);
  imodObjectSetColor(xobjE, 0.8, 0, 0);
  imodObjectSetValue(xobjE, IobjFlagClosed, 0);
  imodObjectSetValue(xobjE, IobjFlagExtraInModv, 1);
    
  Iobj *xobjC = ivwGetAnExtraObject(plug.view, plug.extraObjContDisp);
  imodObjectSetColor(xobjC, 0, 0.4, 0);
  imodObjectSetValue(xobjC, IobjPointSize, plug.sizeLineSpheres);
  imodObjectSetValue(xobjC, IobjLineWidth2, plug.lineDisplayWidth);
  imodObjectSetValue(xobjC, IobjFlagClosed, 0);
  
  Iobj *xobjT = ivwGetAnExtraObject(plug.view, plug.extraObjTiltAxis);
  imodObjectSetColor(xobjT, 1, 1, 0);
  imodObjectSetValue(xobjT, IobjFlagClosed, 0);
  imodObjectSetValue(xobjT, IobjFlagExtraInModv, 1);
  
  Iobj *xobjX = ivwGetAnExtraObject(plug.view, plug.extraObjExtra);
  imodObjectSetColor(xobjX, 1, 0, 1);
  imodObjectSetValue(xobjX, IobjPointSize, plug.sizePurpleSpheres);
  imodObjectSetValue(xobjX, IobjFlagClosed, 0);
  
  
  //## CREATE THE PLUGING WINDOW:
  
  plug.window  = new BeadHelper(imodDialogManager.parent(IMOD_DIALOG),"Bead Helper");
  
  imodDialogManager.add((QWidget *)plug.window, IMOD_DIALOG);
  plug.window->show();
  
  //## REDRAW:
  
  plug.window->changeSphereSize( plug.sphereSize );
  plug.window->verifyTiltIncrement(true, true);
  plug.window->verifySeedViewIsSeeded();
  ivwRedraw( plug.view );
}


//------------------------
//-- MAPPED FUNCTION: Called when initialization is complete
//-- with inReason = IMOD_REASON_STARTUP, or need to update for change
//-- in model with inReason = IMOD_REASON_MODUPDATE.

void imodPlugExecuteType(ImodView *inView, int inType, int inReason)
{
  if( inReason == IMOD_REASON_MODUPDATE
     && ivwGetModel(plug.view)
      && plug.window )
  {
      plug.window->drawExtraObject(false);
  }
}


//------------------------
//-- MAPPED FUNCTION: Process wheel events

int imodPlugEvent(ImodView *vw, QEvent *event, float imx, float imy)
{
  if( plug.window == NULL )
    return (0);
  
  if ( event->type() == QEvent::Wheel && plug.wheelBehav!=WH_NONE )
  {
    QWheelEvent *wheelEvent = static_cast<QWheelEvent*>(event);
    
    int change = floor( (wheelEvent->delta() / plug.wheelResistance) + 0.5 );
    
    switch( plug.wheelBehav )
    {
      case( WH_POINTS ):
      {
        plug.window->advanceSelectedPointInCurrCont( change );
        plug.window->drawExtraObject(false);
        ivwRedraw( plug.view );
        return 1;
      }
      case( WH_SLICES ):
      {
        edit_changeSelectedView( change, true );
        plug.window->drawExtraObject(false);
        return 0;
      }
      case( WH_SMART ):
      {
        if( isCurrContValid() )
        {
          int newView = edit_getZOfTopZap() + change;
          keepWithinRange( newView, 0, plug.zsize-1 );
          
          if( bead_isPtOnView( getCurrCont(), newView ) )
          {
            Imod *imod = ivwGetModel(plug.view);
            int objIdx, contIdx, ptIdx;
            imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
            int newPtIdx = bead_getPtIdxOnView( getCurrCont(), newView );
            imodSetIndex(imod, objIdx, contIdx, newPtIdx);
            
            plug.window->drawExtraObject(false);
            ivwRedraw( plug.view );
            return 1;
          }
        }
        edit_changeSelectedView( change, true );
        plug.window->drawExtraObject(true);
        return 1;
      }
    }
  }
  return 0;
}


//------------------------
//-- MAPPED FUNCTION: Process a mouse event: An example of a circular cursor  
//-- with radius specified in image coordinates

/*
     Mouse event callback function to be defined by plugins with the
     IMOD_PLUG_MOUSE bit set.
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
*/

int imodPlugMouse(ImodView *vw, QMouseEvent *event, float imx, float imy,
                  int but1, int but2, int but3)
{
  plug.mouse.x = imx;
  plug.mouse.y = imy;
  
  return (0);
}





//############################################################

//----------------------------------------------------------------------------
//
//          BeadHelper METHODS:
//
//----------------------------------------------------------------------------



//## WINDOW CLASS CONSTRUCTOR:

static char *buttonLabels[] = {"Done", "Help"};
static char *buttonTips[] = {"Close Bead Helper", "Open help window"};

BeadHelper::BeadHelper(QWidget *parent, const char *name) :
      DialogFrame(parent, 2, buttonLabels, buttonTips, true, "Bead Helper", "", name)
{
  const int LAYOUT_MARGIN   = 4;
  const int LAYOUT_SPACING  = 4;
  const int GROUP_MARGIN    = 1;
  const int SPACER_HEIGHT   = 15;
  
  QString toolStr;
  
  
  //## Range:
  
  grpRange = new QGroupBox("Range:", this);
  grpRange->setFocusPolicy(QWidget::NoFocus);
  grpRange->setMargin(GROUP_MARGIN);
  
  gridLayout1 = new QGridLayout(grpRange);
  gridLayout1->setSpacing(LAYOUT_SPACING);
  gridLayout1->setMargin(LAYOUT_MARGIN);
  gridLayout1->addItem( new QSpacerItem(1,SPACER_HEIGHT), 0, 0);
  
  lblViews = new QLabel("views: ", grpRange);
  lblViews->setFocusPolicy(QWidget::NoFocus);
  gridLayout1->addWidget(lblViews, 1, 0);
  
  viewMinSpinner = new QSpinBox(grpRange);
  viewMinSpinner->setFocusPolicy(QWidget::ClickFocus);
  viewMinSpinner->setMinValue(1);
  viewMinSpinner->setMaxValue(plug.zsize);
  viewMinSpinner->setValue( plug.viewMin );
  QToolTip::add(viewMinSpinner, "Minimum view value (inclusive)");
  gridLayout1->addWidget(viewMinSpinner, 1, 1);
  
  lblViewsTo = new QLabel(" to ", grpRange);
  lblViewsTo->setFocusPolicy(QWidget::NoFocus);
  gridLayout1->addWidget(lblViewsTo, 1, 2);
  
  viewMaxSpinner = new QSpinBox(grpRange);
  viewMaxSpinner->setFocusPolicy(QWidget::ClickFocus);
  viewMaxSpinner->setMinValue(1);
  viewMaxSpinner->setMaxValue(plug.zsize);
  viewMaxSpinner->setValue( plug.viewMax );
  QToolTip::add(viewMaxSpinner, "Maximum view value (inclusive)");
  gridLayout1->addWidget(viewMaxSpinner, 1, 3);
  
  lblContours = new QLabel("contours: ", grpRange);
  lblContours->setFocusPolicy(QWidget::NoFocus);
  gridLayout1->addWidget(lblContours, 2, 0);
  
  contMinSpinner = new QSpinBox(grpRange);
  contMinSpinner->setFocusPolicy(QWidget::ClickFocus);
  contMinSpinner->setMinValue(1);
  contMinSpinner->setMaxValue(1000);
  contMinSpinner->setValue( plug.contMin );
  QToolTip::add(contMinSpinner, "Minimum contour value (inclusive)");
  gridLayout1->addWidget(contMinSpinner, 2, 1);
  
  lblContoursTo = new QLabel(" to ", grpRange);
  lblContoursTo->setFocusPolicy(QWidget::NoFocus);
  gridLayout1->addWidget(lblContoursTo, 2, 2);
  
  contMaxSpinner = new QSpinBox(grpRange);
  contMaxSpinner->setFocusPolicy(QWidget::ClickFocus);
  contMaxSpinner->setMinValue(1);
  contMaxSpinner->setMaxValue(9999);
  contMaxSpinner->setValue( plug.contMax );
  QToolTip::add(contMaxSpinner, "Maximum contour value (inclusive)"
                "\n\nNOTE: This value defaults to 9999 - which means all contours "
                "\n     from the min value to the last contour in the "
                "\n     object are included in the range");
  gridLayout1->addWidget(contMaxSpinner, 2, 3);
  
  mLayout->addWidget(grpRange);
  

  //## Actions:
  
  grpActions = new QGroupBox("Actions:", this);
  grpActions->setFocusPolicy(QWidget::NoFocus);
  
  vboxLayout1 = new QVBoxLayout(grpActions);
  vboxLayout1->setSpacing(LAYOUT_SPACING);
  vboxLayout1->setMargin(LAYOUT_MARGIN);
  vboxLayout1->addItem( new QSpacerItem(1,SPACER_HEIGHT) );
  
  deletePtsButton = new QPushButton("Delete Points in Range [d]", grpActions);
  connect(deletePtsButton, SIGNAL(clicked()), this, SLOT(deletePtsInRange()));
  QToolTip::add(deletePtsButton,
                "Deletes all points on the specified views from the specified "
                "range of contours");
  vboxLayout1->addWidget(deletePtsButton);
  
  reduceContsToSeedButton = new QPushButton("Reduce Contours to Seed  [r]", grpActions);
  QObject::connect(reduceContsToSeedButton, SIGNAL(clicked()), this,
                   SLOT(reduceContsToSeed()));
  QToolTip::add(reduceContsToSeedButton,
                "Deletes all points in the current contour except on the seed view");
  vboxLayout1->addWidget(reduceContsToSeedButton);
  
  movePtsToEstButton = new QPushButton("Move Pts to Estimated Pos [e]", grpActions);
  QObject::connect(movePtsToEstButton, SIGNAL(clicked()), this,
                   SLOT(movePtsToEstimatedPosOptions()));
  QToolTip::add(movePtsToEstButton,
                "Smooths contour(s) by moving points towards their estimated position "
                "(provides several different methods to achieve this)");
  vboxLayout1->addWidget(movePtsToEstButton);
  
  fillMissingPtsButton = new QPushButton("Fill Missing Points   [f]", grpActions);
  QObject::connect(fillMissingPtsButton, SIGNAL(clicked()), this,
                   SLOT(fillMissingPts()));
  QToolTip::add(fillMissingPtsButton,
                "Fills in missing points");
  vboxLayout1->addWidget(fillMissingPtsButton);
  
  reorderContsButton = new QPushButton("Reorder Contours   [o]", grpActions);
  connect(reorderContsButton, SIGNAL(clicked()), this, SLOT(reorderContours()));
  QToolTip::add(reorderContsButton,
                "Reorders the specified range of contours by one of several crititeria");
  vboxLayout1->addWidget(reorderContsButton);
  
  mLayout->addWidget(grpActions);
  
  
  //## Display:
  
  grpDisplay = new QGroupBox("Visual Aids:", this);
  grpDisplay->setFocusPolicy(QWidget::NoFocus);
  grpDisplay->setMargin(GROUP_MARGIN);
  
  gridLayout2 = new QGridLayout(grpDisplay);
  gridLayout2->setSpacing(LAYOUT_SPACING);
  gridLayout2->setMargin(LAYOUT_MARGIN);
  gridLayout2->addItem( new QSpacerItem(1,SPACER_HEIGHT), 0, 0);
  
  showEstimatedPosCheckbox = new QCheckBox("show estimated position", grpDisplay);
  showEstimatedPosCheckbox->setChecked( plug.showExpectedPos );
  QObject::connect(showEstimatedPosCheckbox,SIGNAL(clicked()),this,
                   SLOT(changeShowExpectedPos()));
  QToolTip::add(showEstimatedPosCheckbox, 
                "Shows the estimated position of the current point based on "
                "position of points either side");
  gridLayout2->addMultiCellWidget(showEstimatedPosCheckbox, 1, 1, 0, 1);
  
  showSpheresCheckbox = new QCheckBox("sphere size:", grpDisplay);
  showSpheresCheckbox->setChecked( plug.showSpheres );
  QObject::connect(showSpheresCheckbox,SIGNAL(clicked()),this,
                   SLOT(changeShowSpheres()));
  QToolTip::add(showSpheresCheckbox,
                "Show/hide spheres on the current object");
  gridLayout2->addWidget(showSpheresCheckbox, 2, 0);
  
  sphereSizeSpinner = new QSpinBox(grpDisplay);
  sphereSizeSpinner->setMinValue(1);
  sphereSizeSpinner->setMaxValue(50);
  sphereSizeSpinner->setFocusPolicy(QWidget::NoFocus);
  sphereSizeSpinner->setValue( plug.sphereSize );
  QObject::connect(sphereSizeSpinner,SIGNAL(valueChanged(int)),this,
                   SLOT(changeSphereSize(int)));
  QToolTip::add(sphereSizeSpinner,
                "The size of the spheres you wish to display");
  gridLayout2->addWidget(sphereSizeSpinner, 2, 1);
  
  lblLineDisplay = new QLabel("line display: ", grpDisplay);
  lblLineDisplay->setFocusPolicy(QWidget::NoFocus);
	QToolTip::add(lblLineDisplay, 
              "Visual aid to let you see the trajectory of contours in the ZAP window");
  gridLayout2->addWidget(lblLineDisplay, 3, 0);
  
  lineDisplayCombo = new QComboBox(grpDisplay);
	lineDisplayCombo->setFocusPolicy(QWidget::NoFocus);
	lineDisplayCombo->insertItem("off");
	lineDisplayCombo->insertItem("all contours");
	lineDisplayCombo->insertItem("curr contour");
  lineDisplayCombo->insertItem("missing pts");
  lineDisplayCombo->insertItem("result smooth");
  lineDisplayCombo->insertItem("pt residuals");
  lineDisplayCombo->insertItem("line best fit");
  lineDisplayCombo->setCurrentItem( plug.lineDisplayType );
	connect(lineDisplayCombo, SIGNAL(activated(int)), this,
          SLOT(changeLineDisplayType(int)));
  toolStr = "Visual aid to let you see the trajectory of contours. "
    "\n"
    "\n > off             - don't display any lines/contours "
    "\n > all contours        - shows ALL contours on current object "
      "with any checked contours as stippled "
    "\n > curr contour   - shows only the current (selected) contour "
    "\n > missing pts    - shows the selected contour as solid, with the expected "
      " position of any missing points as crosshairs "
    "\n > result smooth - shows the selected contour as solid and what will happen "
      " if the current contour is smoothed by pressing [E] as stippled "
    "\n > pt residuals    - shows the displacement of each point in the current "
      " contour with solid arrows "
    "\n > line best fit  - shows a 'line of best fit'; a straight line which "
      "best approximates all the points in the slected contour";
  QToolTip::add(lineDisplayCombo, toolStr );
	//QToolTip::add(lineDisplayCombo, "Visual aid to let you see the trajectory of contours");
	gridLayout2->addWidget(lineDisplayCombo, 3, 1);
  
  lblTiltDisplay = new QLabel("tilt display: ", grpDisplay);
  lblTiltDisplay->setFocusPolicy(QWidget::NoFocus);
	QToolTip::add(lblTiltDisplay, 
                "Visual aid to show the axis about which subsequent views are tilted");
  gridLayout2->addWidget(lblTiltDisplay, 4, 0);
  
  tiltDisplayCombo = new QComboBox(grpDisplay);
	tiltDisplayCombo->setFocusPolicy(QWidget::NoFocus);
	tiltDisplayCombo->insertItem("off");
  tiltDisplayCombo->insertItem("tilt axis");
  tiltDisplayCombo->insertItem("tilt and seed");
  tiltDisplayCombo->insertItem("tilt and pt");
  tiltDisplayCombo->insertItem("tilt segments");
  tiltDisplayCombo->insertItem("[h] grid");
  tiltDisplayCombo->setCurrentItem( plug.tiltDisplayType );
	connect(tiltDisplayCombo, SIGNAL(activated(int)), this,
          SLOT(changeTiltDisplayType(int)));
  toolStr = "Visual aid to show the tilt axis. "
    "\n"
    "\n > off             - don't display tilt axis "
    "\n > tilt axis        - shows tilt axis only - you can change this by clicking: "
      "'More Settings' > 'tilt axis angle'"
    "\n > tilt and seed - shows tilt axis plus a perpendicular dotted line through the "
      "seed point... useful for highlighting changes in the angle of contour lines "
    "\n > tilt and pt   - shows tilt axis, seed line and also a line from the "
      "intersection of these lines through the currently selected point"
    "\n > tilt segments - shows tilt axis and four parallel dotted lines which move "
      "according to the tilt increment (under 'More Settings')"
    "\n > [h] grid    - shows the grid used to find the 'next biggest hole' when [h] "
      "is pressed (see 'More Settings')";
	QToolTip::add(tiltDisplayCombo, toolStr);
	gridLayout2->addWidget(tiltDisplayCombo, 4, 1);
  
  mLayout->addWidget(grpDisplay);
  
  
  //## Display:
  
  grpOptions = new QGroupBox("Options:", this);
  grpOptions->setFocusPolicy(QWidget::NoFocus);
  grpOptions->setMargin(GROUP_MARGIN);
  
  gridLayout3 = new QGridLayout(grpOptions);
  gridLayout3->setSpacing(LAYOUT_SPACING);
  gridLayout3->setMargin(LAYOUT_MARGIN);
  gridLayout3->addItem( new QSpacerItem(1,SPACER_HEIGHT), 0, 0);
  
  lblEstMethod = new QLabel("estimation meth:", grpOptions);
	QToolTip::add(lblEstMethod, 
                "The method used to determine the estimated position of points");
  gridLayout3->addWidget(lblEstMethod, 1, 0);
  
  estPosMethodCombo = new QComboBox(grpOptions);
  estPosMethodCombo->setFocusPolicy(QWidget::NoFocus);
  estPosMethodCombo->insertItem("best 2 pts");
  estPosMethodCombo->insertItem("smart 2 pts");
	estPosMethodCombo->insertItem("nearest 2 pts");
  estPosMethodCombo->insertItem("prev 2 pts");
	estPosMethodCombo->insertItem("curve");
	estPosMethodCombo->insertItem("local curve");
  estPosMethodCombo->insertItem("prev 6 pts");
  estPosMethodCombo->insertItem("prev 3 pts");
  estPosMethodCombo->setCurrentItem( plug.estPosMethod );
	connect(estPosMethodCombo, SIGNAL(activated(int)), this,
          SLOT(changeEstPosMethod(int)));
  toolStr = "The method used to determine the estimated position of points. "
    "\n... personally I recommend the 'best 2 pts' method - especially for smoothing"
    "\n"
    "\n > best 2 pts      - uses closest pt above and below and calculates a simple "
    "linear path between them... great for filling small gaps"
    "\n > smart 2 pts     - same as above, but uses the 'tilt increment' value "
    "set within 'More Settings' to estimate the position"
    "\n > nearest 2 pts - uses closest 2 pts to given view and linear trajectory"
    "\n > prev 2 pts      - uses prev 2 points towards the seed pt and linear trajectory"
    "\n > curve              - calculates a quadratic curve using the first, last and "
    "seed pt in the contour... useful for filling points at high tilt, but bad at "
    "filling small gaps"
    "\n > local curve    - calculates a quadratic using the closest 3 pts spaced "
    "at least 10 views apart... better at dealing with local shifts"
    "\n > prev 6 pts     - uses the line of best fit, plus increasing distance along "
    "X over the previous 3 points towards the seed"
    "\n > prev 3 pts     - as above using previous 3 points";
  QToolTip::add(estPosMethodCombo, toolStr );
  gridLayout3->addWidget(estPosMethodCombo, 1, 1);
  
  mLayout->addWidget(grpOptions);
  
  keyboardSettingsButton = new QPushButton("Mouse and Keyboard", grpOptions);
  connect(keyboardSettingsButton, SIGNAL(clicked()), this, SLOT(keyboardSettings()));
  QToolTip::add(keyboardSettingsButton,
                "Contains several other settings I didn't want to sqeeze "
                "into this window");
  gridLayout3->addMultiCellWidget(keyboardSettingsButton, 2, 2, 0, 1);
  
  moreActionsButton = new QPushButton("More Actions", grpOptions);
  connect(moreActionsButton, SIGNAL(clicked()), this, SLOT(moreActions()));
  QToolTip::add(moreActionsButton,
                "Contains several other actions I didn't want to sqeeze "
                "into this window");
  gridLayout3->addWidget(moreActionsButton, 3, 0);
  
  moreSettingsButton = new QPushButton("More Settings", grpOptions);
  connect(moreSettingsButton, SIGNAL(clicked()), this, SLOT(moreSettings()));
  QToolTip::add(moreSettingsButton,
                "Contains several other settings I didn't want to sqeeze "
                "into this window");
  gridLayout3->addWidget(moreSettingsButton, 3, 1);
  
  
  mLayout->addStretch();
  this->adjustSize();
  
  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
}


//## SLOTS:



//------------------------
//-- Adds points in the shape of a crosshair to contour

void cont_addCrosshair( Icont *cont, Ipoint center, float size, int view )
{
  float h = size/2.0;
  imodPointAppendXYZ( cont, center.x,     center.y,      view );
  imodPointAppendXYZ( cont, center.x,     center.y+h,    view );
  imodPointAppendXYZ( cont, center.x,     center.y-h,    view );
  imodPointAppendXYZ( cont, center.x,     center.y,      view );
  imodPointAppendXYZ( cont, center.x+h,   center.y,      view );
  imodPointAppendXYZ( cont, center.x-h,   center.y,      view );
  imodPointAppendXYZ( cont, center.x,     center.y,      view );
}

//------------------------
//-- Adds points in the shape of a cross to contour

void cont_addCross( Icont *cont, Ipoint center, float size, int view )
{
  float h = size/2.0;
  imodPointAppendXYZ( cont, center.x,     center.y,      view );
  imodPointAppendXYZ( cont, center.x+h,   center.y+h,    view );
  imodPointAppendXYZ( cont, center.x-h,   center.y-h,    view );
  imodPointAppendXYZ( cont, center.x,     center.y,      view );
  imodPointAppendXYZ( cont, center.x+h,   center.y-h,    view );
  imodPointAppendXYZ( cont, center.x-h,   center.y+h,    view );
  imodPointAppendXYZ( cont, center.x,     center.y,      view );
}

//------------------------
//-- Adds points in the shape of a cross to contour

void cont_addDiamond( Icont *cont, Ipoint center, float size, int view )
{
  float h = size/2.0;
  imodPointAppendXYZ( cont, center.x-h,   center.y,      view );
  imodPointAppendXYZ( cont, center.x,     center.y-h,    view );
  imodPointAppendXYZ( cont, center.x+h,   center.y,      view );
  imodPointAppendXYZ( cont, center.x,     center.y+h,    view );
  imodPointAppendXYZ( cont, center.x-h,   center.y,      view );
}

//------------------------
//-- Adds points in the shape of a cross to contour

void cont_addArrow( Icont *cont, Ipoint from, Ipoint to, float size, int view )
{
  float h = size/2.0;
  imodPointAppendXYZ( cont, from.x,   from.y,     view );
  imodPointAppendXYZ( cont, to.x,     to.y,         view );
  
  float lineLen = line_distBetweenPts2D( &to, &from );
  if(lineLen == 0)
    return;
  
  float fractAlong = size / lineLen;
  Ipoint arrow = line_findPtFractBetweenPts2D( &to, &from, fractAlong );
  point_rotatePointAroundPoint2D( &arrow, &to, 40*DEGS_TO_RADS );
  imodPointAppendXYZ( cont, arrow.x,   arrow.y,      view );
  imodPointAppendXYZ( cont, to.x,      to.y,         view );
  point_rotatePointAroundPoint2D( &arrow, &to, -80*DEGS_TO_RADS );
  imodPointAppendXYZ( cont, arrow.x,   arrow.y,      view );
}

//------------------------
//-- Adds points in the shape of a cross to contour

void cont_makeContShowingMissingPoints( Icont *to, Icont *from, int view, float radius )
{
  for(int p=0; p<psize(from); p++)  // for each point: draw little verticle line
  {
    imodPointAppendXYZ( to, getPt(from,p)->x, getPt(from,p)->y, view );
    imodPointAppendXYZ( to, getPt(from,p)->x, getPt(from,p)->y+radius, view );
    imodPointAppendXYZ( to, getPt(from,p)->x, getPt(from,p)->y-radius, view );
    imodPointAppendXYZ( to, getPt(from,p)->x, getPt(from,p)->y, view );
    
    if( getPtZInt(from,p+1) != getPtZInt(from,p) + 1  )
      imodPointAppendXYZ( to, getPt(from,p)->x, getPt(from,p)->y, -1 );
  }
  imodPointAppendXYZ( to, 0, 0, -1 );
  
  for( int z=0; z<plug.zsize; z++)    // for each view:
  {
    if( !bead_isPtOnView(from,z) )     // if missin point:
    {                                     // draw a cross at it's estimated poistion
      Ipoint estPt;
      if ( bead_getExpectedPosOfPoint( from, z, &estPt ) )
      {
        cont_addCross( to, estPt, radius*3.0, view );
        imodPointAppendXYZ( to, estPt.x, estPt.y, -1 );
      }
    }
  }
}


//------------------------
//-- Adds a contour with two points to the given object

void cont_addLineToObj( Iobj *obj,
                        float x1, float y1, float z1,
                        float x2, float y2, float z2,
                        bool drawAllZ, bool interploated )
{
  Icont *cont = imodContourNew();
  imodPointAppendXYZ( cont, x1, y1, z1 );
  imodPointAppendXYZ( cont, x2, y2, z2 );
  if(drawAllZ)
    imodContourSetFlag( cont, ICONT_DRAW_ALLZ, 1 );
  if(interploated)
    imodContourSetFlag( cont, ICONT_STIPPLED, 1 );
  imodObjectAddContour( obj, cont );
  free(cont);
}

//------------------------
//-- Adds a contour with one points to the given object

void cont_addPtToObj( Iobj *obj,
                        float x, float y, float z,
                        bool drawAllZ )
{
  Icont *cont = imodContourNew();
  imodPointAppendXYZ( cont, x, y, z );
  if(drawAllZ)
    imodContourSetFlag( cont, ICONT_DRAW_ALLZ, 1 );
  imodObjectAddContour( obj, cont );
  free(cont);
}


//------------------------
//-- Accesses the extra object and draw a red deform circle and/or other
//-- reference contour at the last recorded position of the mouse. What is
//-- drawn depends on what drawing mode is selected.

bool BeadHelper::drawExtraObject( bool redraw )
{
  Iobj *xobjE = ivwGetAnExtraObject(plug.view, plug.extraObjExpPos);
  Iobj *xobjC = ivwGetAnExtraObject(plug.view, plug.extraObjContDisp);
  Iobj *xobjT = ivwGetAnExtraObject(plug.view, plug.extraObjTiltAxis);
  
  if ( !plug.window || !xobjC || !xobjE || !xobjT )
    return 0;
  
  //## CLEAR EXTRA OBJECTS:
  
  ivwClearAnExtraObject( plug.view, plug.extraObjContDisp );
  ivwClearAnExtraObject( plug.view, plug.extraObjExpPos );
  ivwClearAnExtraObject( plug.view, plug.extraObjTiltAxis );
  
  
  //## GET Z VALUE:
  
  int ix, iy,iz;
  ivwGetLocation(plug.view, &ix, &iy, &iz);
  plug.mouse.z = iz;
  
  float x = plug.mouse.x;
  float y = plug.mouse.y;
  float z = plug.mouse.z;
  
  float zapZoom = 1.0f;
  ivwGetTopZapZoom(plug.view, &zapZoom);
  float sc = fDivide( 1.0f, zapZoom);
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  Icont *cont = getCurrCont();
  
  
  //## IF SHOW EXPECTED POSITION: DRAW CROSSHAIR AT EXPECTED POSITION
  
  if( plug.showExpectedPos && isCurrContValid() )
  {
    Ipoint ptEst;
    bool success = bead_getExpectedPosOfPoint( cont, z, &ptEst );
    
    if(success)
    {
      Icont *xcont = imodContourNew();
      
      if( bead_isPtOnView(cont, z) )
      {
        if( plug.expPtDisplayType == ED_DIAMOND )
          cont_addDiamond( xcont, ptEst, plug.expPtSize*sc, z );
        else if( plug.expPtDisplayType == ED_CROSS )
          cont_addCross( xcont, ptEst, plug.expPtSize*sc, z );
        else {
          Ipoint *pt = bead_getPtOnView(cont, z);
          cont_addArrow( xcont, *pt, ptEst, plug.expPtSize*sc, z );
        }
      }
      else
      {
        if( plug.expPtDisplayType == ED_DIAMOND )
          cont_addDiamond( xcont, ptEst, 2*plug.expPtSize*sc, z );
        else
          cont_addCross( xcont, ptEst, 2*plug.expPtSize*sc, z );
      }
      imodContourSetFlag(xcont, ICONT_DRAW_ALLZ, 1);
      imodObjectAddContour(xobjE, xcont);
      free(xcont);
    }
  }
  
  //## DRAW APPROPRIATE LINE DISPLAY:
  
  switch ( plug.lineDisplayType )
  {
    case( LD_ALL ):
    {
      for(int c=0; c<csize(obj);c++)
      {
        Icont *xcont = imodContourDup( getCont(obj,c) );
        changeZValue( xcont, z );
        imodContourSetFlag(xcont, ICONT_DRAW_ALLZ | ICONT_MMODEL_ONLY, 1);
        imodObjectAddContour(xobjC, xcont);
        free(xcont);
      }
    }break;
    
    case( LD_CURRENT ):
    {
      if( isCurrContValid() )
      {
        Icont *xcont = imodContourDup( getCurrCont() );
        changeZValue( xcont, z );
        imodContourSetFlag(xcont, ICONT_DRAW_ALLZ, 1);
        imodObjectAddContour(xobjC, xcont);
        free(xcont);
      }
    }break;
    
    case( LD_CURRMISSING ):
    {
      if( isCurrContValid() )
      {
        Icont *xcont = imodContourNew();
        cont_makeContShowingMissingPoints( xcont, getCurrCont(), z, sc*2 );
        
        imodContourSetFlag(xcont, ICONT_DRAW_ALLZ, 1);
        imodObjectAddContour(xobjC, xcont);
        free(xcont);
      }
    }break;
    
    case( LD_RESULTSMOOTH ):
    {
      if( isCurrContValid() )
      {
        Icont *xcont  = imodContourNew();
        cont_makeContShowingMissingPoints( xcont, getCurrCont(), z, sc*2 );
        imodContourSetFlag(xcont, ICONT_DRAW_ALLZ | ICONT_STIPPLED, 1);
        imodObjectAddContour(xobjC, xcont);
        free(xcont);
        
        Icont *xcontS = imodContourDup( getCurrCont() );
        int ptsMoved, ptsAdded;
        ivwDraw( plug.view, 0 );          // redraw to ensure correct z is selected
        bead_smoothPtsUsingPlugSettings( xcontS, ptsMoved, ptsAdded );
        changeZValue( xcontS, z );
        imodContourSetFlag(xcontS, ICONT_DRAW_ALLZ, 1);
        imodObjectAddContour(xobjC, xcontS);
        free(xcontS);
      }
    }break;
    
    case( LD_SLICE_RESID ):
    {
      if( isCurrContValid() )
      {
        for(int p=0; p<psize(cont); p++)
        {
          Ipoint *pt    = getPt(cont,p);
          Ipoint ptEst;
          if( bead_getExpectedPosOfPoint(cont, getPtZInt(cont,p), &ptEst) )
          {
            Ipoint resid  = line_findPtFractBetweenPts2D( pt, &ptEst, 1.0 );
            cont_addLineToObj( xobjC, pt->x,pt->y,z, resid.x,resid.y,z, true, false);
          }
        }
      }
    } break;
    
    case( LD_BEST_FIT ):
    {
      if( isCurrContValid() )
      {
        float gradient, offset;
        bool success = bead_calcLineOfBestFit( cont, &gradient, &offset, 4 );
        if(success)
        {
          cont_addLineToObj( xobjC, 0,(offset),z,
                             plug.xsize,(gradient*plug.xsize+offset),z, true, true);
        }
      }
    } break;
  }
  
  //## DRAW TILT AXIS:
  
  if ( plug.tiltDisplayType != TD_OFF )
  {
    float gradientP = tan( (plug.tiltAxisAngle)*DEGS_TO_RADS );
                  // calculate gradient perpendicular to the tilt axis
    float offsetX = plug.middlePt.x + plug.tiltOffsetX;
    float startX   = offsetX + ((plug.ysize/2.0)*gradientP);
    float endX     = offsetX - ((plug.ysize/2.0)*gradientP);
    
    cont_addLineToObj( xobjT, startX,0,z, endX,plug.ysize,z, true, false);
    
    if( plug.tiltDisplayType == TD_TILTAXISSEED
        || plug.tiltDisplayType == TD_TILTAXISPT )
    {
      if( cont && bead_isPtOnView( cont, plug.seedView) )
      {
        Ipoint *ptS = bead_getPtOnView( cont, plug.seedView );
        float startSYP   = ptS->y - ((ptS->x - 0)*gradientP);
        float endSYP     = ptS->y + ((plug.xsize - ptS->x)*gradientP);
        
        cont_addLineToObj( xobjT, 0,startSYP,z, plug.xsize,endSYP,z, true, true);
        
        if( plug.tiltDisplayType == TD_TILTAXISPT && isCurrPtValid() )
        {
          Ipoint *pt = getCurrPt();
          Ipoint tiltTop, tiltBottom, perpTop, perpBottom, intercept;
          setPt( &tiltBottom, startX, 0, z );
          setPt( &tiltTop,    endX,  plug.ysize, z );
          setPt( &perpBottom, 0, startSYP, z );
          setPt( &perpTop,    plug.xsize, endSYP, z );
          line_doLinesCrossAndWhere( &perpBottom, &perpTop,
                                     &tiltBottom, &tiltTop, &intercept );
          
          float lineDist = line_distBetweenPts2D( &intercept, pt );
          
          if(lineDist > 1)
          {
            Ipoint end = line_findPtFractBetweenPts2D( &intercept, pt, 2.0 );
            cont_addLineToObj( xobjT, intercept.x,intercept.y,z,
                               end.x,end.y,z, true, true);
          }
        }
      }
    }
    else if( plug.tiltDisplayType == TD_TILTSEGS )
    {
      float tiltAngle = bead_getTiltAngleAtZ( z );
      float segWidth  = (plug.xsize / 5.0f) * cosf( DEGS_TO_RADS * tiltAngle );
      float ym = plug.ysize;
      
      cont_addLineToObj( xobjT, startX+1*segWidth,0,z, endX+1*segWidth,ym,z, true, true);
      cont_addLineToObj( xobjT, startX+2*segWidth,0,z, endX+2*segWidth,ym,z, true, true);
      cont_addLineToObj( xobjT, startX-1*segWidth,0,z, endX-1*segWidth,ym,z, true, true);
      cont_addLineToObj( xobjT, startX-2*segWidth,0,z, endX-2*segWidth,ym,z, true, true);
    }
    else if( plug.tiltDisplayType == TD_HGRID )
    {
      int colsX = int(plug.xsize / plug.biggestHoleGrid);
      int rowsY = int(plug.ysize / plug.biggestHoleGrid);
      float sideLenX = fDivide(plug.xsize, colsX);
      float sideLenY = fDivide(plug.ysize, rowsY);
      
      float inset = plug.biggestHoleInset;
      float maxX  = plug.xsize-inset;
      float maxY  = plug.ysize-inset;
      
      for ( float x=sideLenX/2.0f; x<=plug.xsize; x+=sideLenX )
        if( x>inset && x<plug.xsize-inset )
          cont_addLineToObj( xobjT, x,0,z, x,plug.ysize,z, true,true );
      
      for ( float y=sideLenY/2.0f; y<=plug.ysize; y+=sideLenY )
        if( y>inset && y<plug.ysize-inset )
          cont_addLineToObj( xobjT, 0,y,z, plug.xsize,y,z, true,true );
      
      cont_addLineToObj( xobjT, inset, inset,z,  inset, maxY,z, true, false);
      cont_addLineToObj( xobjT, inset, maxY,z,   maxX,  maxY,z, true, false);
      cont_addLineToObj( xobjT, maxX,  maxY,z,   maxX,  inset,z, true, false);
      cont_addLineToObj( xobjT, maxX,  inset,z,  inset, inset,z, true, false);
    }
  }
  
  if( redraw )
    ivwDraw( plug.view, 0 );
  return true;
}



  
//------------------------
//-- Clears all the contents of the extra object.

void BeadHelper::clearExtraObj()
{
  Iobj *obj = ivwGetExtraObject(plug.view);
  int ncont = csize(obj);
  if (!ncont)
    return;
  
  Icont *cont = getCont(obj, 0);
  for (int co = ncont - 1; co >= 0; co--)
    imodObjectRemoveContour(obj, co);
  imodContoursDelete(cont, ncont);
}



//------------------------
//-- Loads most of the settings for BeadHelperData from user preferences

void BeadHelper::loadSettings()
{

  double savedValues[NUM_SAVED_VALS];
  
  int nvals = prefGetGenericSettings("BeadHelper", savedValues, NUM_SAVED_VALS);
  
  if(nvals!=NUM_SAVED_VALS)
  {
    wprint("\aBeadHelper: Could not load saved values\n\n");
    wprint("\a  If this is your first time using BeadHelper");
    wprint("\a  it is HIGHLY recommended you click 'Help'");
    wprint("\a  to learn about how it works!");
    return;
  }
  
  plug.showExpectedPos      = savedValues[0];
  plug.showSpheres          = savedValues[1];
  plug.sphereSize           = savedValues[2];
  plug.lineDisplayType      = savedValues[3];
  plug.tiltDisplayType      = savedValues[4];
  plug.estPosMethod         = savedValues[5];
  
  plug.tiltIncrement        = savedValues[6];
  plug.tiltAxisAngle        = savedValues[7];
  plug.biggestHoleGrid      = savedValues[8];
  plug.biggestHoleInset     = savedValues[9];
  plug.expPtDisplayType     = savedValues[10];
  plug.expPtSize            = savedValues[11];
  plug.sizeLineSpheres      = savedValues[12];
  plug.lineDisplayWidth     = savedValues[13];
  plug.sizePurpleSpheres    = savedValues[14];
  plug.selectedAction       = savedValues[15];
  plug.sortCriteria         = savedValues[16];
  plug.autoSaveSettings     = savedValues[17];
  
  plug.wheelBehav           = savedValues[18];
  plug.wheelResistance      = savedValues[19];
  plug.includeEndsResid     = savedValues[20];
  plug.contsToSearch        = savedValues[21];
  plug.dKeyBehav            = savedValues[22];
  plug.mKeyBehav            = savedValues[23];
  plug.wCurrContOnly        = savedValues[24];
  plug.wWeightedDiv         = savedValues[25];
  plug.enterAction          = savedValues[26];
  plug.minPtsEnter          = savedValues[27];
  plug.enterPrint           = savedValues[28];
  
  plug.smoothCurrContOnly   = savedValues[29];
  plug.smoothFillGaps       = savedValues[30];
  plug.smoothBigResidFirst  = savedValues[31];
  plug.smoothMoveYOnly      = savedValues[32];
  plug.smoothLeaveSeed      = savedValues[33];
  plug.smoothLeaveEnds      = savedValues[34];
  plug.smoothLeaveCurrV     = savedValues[35];
  plug.smoothMoveFract      = savedValues[36];
  plug.smoothMinResid       = savedValues[37];
  plug.smoothIterations     = savedValues[38];
  plug.smoothAdjacentV      = savedValues[39];
  plug.smoothNumViews       = savedValues[40];
}


//------------------------
//-- Saves most of the settings within BeadHelperData in user preferences
//-- so they will load next time Bead Helper is started

void BeadHelper::saveSettings()
{
  double saveValues[NUM_SAVED_VALS];
  
  saveValues[0]  = plug.showExpectedPos;
  saveValues[1]  = plug.showSpheres;
  saveValues[2]  = plug.sphereSize;
  saveValues[3]  = plug.lineDisplayType;
  saveValues[4]  = plug.tiltDisplayType;
  saveValues[5]  = plug.estPosMethod;
  
  saveValues[6]  = plug.tiltIncrement;
  saveValues[7]  = plug.tiltAxisAngle;
  saveValues[8]  = plug.biggestHoleGrid;
  saveValues[9]  = plug.biggestHoleInset;
  saveValues[10] = plug.expPtDisplayType;
  saveValues[11] = plug.expPtSize;
  saveValues[12] = plug.sizeLineSpheres;
  saveValues[13] = plug.lineDisplayWidth;
  saveValues[14] = plug.sizePurpleSpheres;
  saveValues[15] = plug.selectedAction;
  saveValues[16] = plug.sortCriteria;
  saveValues[17] = plug.autoSaveSettings;
  
  saveValues[18] = plug.wheelBehav;
  saveValues[19] = plug.wheelResistance;
  saveValues[20] = plug.includeEndsResid;
  saveValues[21] = plug.contsToSearch;
  saveValues[22] = plug.dKeyBehav;
  saveValues[23] = plug.mKeyBehav;
  saveValues[24] = plug.wCurrContOnly;
  saveValues[25] = plug.wWeightedDiv;
  saveValues[26] = plug.enterAction;
  saveValues[27] = plug.minPtsEnter;
  saveValues[28] = plug.enterPrint;
  
  saveValues[29] = plug.smoothCurrContOnly;
  saveValues[30] = plug.smoothFillGaps;
  saveValues[31] = plug.smoothBigResidFirst;
  saveValues[32] = plug.smoothMoveYOnly;
  saveValues[33] = plug.smoothLeaveSeed;
  saveValues[34] = plug.smoothLeaveEnds;
  saveValues[35] = plug.smoothLeaveCurrV;
  saveValues[36] = plug.smoothMoveFract;
  saveValues[37] = plug.smoothMinResid;
  saveValues[38] = plug.smoothIterations;
  saveValues[39] = plug.smoothAdjacentV;
  saveValues[40] = plug.smoothNumViews;
  
  prefSaveGenericSettings("BeadHelper",NUM_SAVED_VALS,saveValues);
}




//------------------------
//-- Deletes all points within the specified range of views from the specified range
//-- of contours.

void BeadHelper::deletePtsInRange()
{
  if( !updateAndVerifyRanges() )
    return;
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int numConts  = (plug.contMax - plug.contMin) + 1;
  int numViews = (plug.viewMax - plug.viewMin) + 1;
  int numPts = numConts * numViews;
  string incSeedView = isBetweenAsc(plug.viewMin,plug.seedView,plug.viewMax) ?
    "\nTHIS INCLUDES THE MIDDLE SLICE!\n" : "";
  string msg = "This operation will delete ALL points \n  on "
    + toString(numConts) + " contours ("
    + toString(plug.contMin+1) + "-" + toString(plug.contMax+1)
    + ") \n  on " + toString(numViews) + " views ("
    + toString(plug.viewMin+1) + "-" + toString(plug.viewMax+1) + ")"
    + "\n  Potential # points = " + toString(numPts)
    + incSeedView;
  
  static bool skipCheckedConts = true;
  static bool skipSeedView     = true;
  
  CustomDialog ds;
  int ID_DUMMY         = ds.addLabel   ( msg.c_str() );
  int ID_SKIPCHECKEDC  = ds.addCheckBox( "skip checked contours",
                                         skipCheckedConts,
                                         "Will not delete any points from checked "
                                         "(stippled) contours " );
  int ID_SKIPSEEDVIEW  = ds.addCheckBox( "skip middle (seed) view",
                                         skipSeedView,
                                         "Will not delete any seed points (points on "
                                         "the seed view)" );
  GuiDialogCustomizable dlg(&ds, "Delete Points", this);
  dlg.exec();
  if( ds.cancelled )
    return;
  skipCheckedConts     = ds.getResultCheckBox  ( ID_SKIPCHECKEDC );
  skipSeedView         = ds.getResultCheckBox  ( ID_SKIPSEEDVIEW );
  
  
  //## DELETE ALL POINTS IN RANGE:
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int numPtsDeleted = 0;
  
  for( int c=MAX(plug.contMin,0); c<=plug.contMax && c<csize(obj); c++)
  {
    imodSetIndex(imod, objIdx, c, 0);
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    
    Icont *cont = getCont( obj, c );
    
    if( skipCheckedConts && isInterpolated(cont) )
      continue;
    
    for( int p=0; p<psize(cont); p++ )
    {
      int z = getPtZInt(cont,p);
      if( isBetweenAsc( plug.viewMin, z, plug.viewMax ) 
          && ( !skipSeedView || z != plug.seedView ) )
      {
        imodPointDelete( cont, p );
        p--;
        numPtsDeleted++;
      }
    }
  }
  
  if(numPtsDeleted)
    undoFinishUnit( plug.view );          // FINISH UNDO
  
  imodSetIndex(imod, objIdx, contIdx, ptIdx);
  
  wprint("Deleted %d points\n", numPtsDeleted);
  ivwRedraw( plug.view );
}

//------------------------
//-- Deletes all points within the current contour from the specified range
//-- of views.

void BeadHelper::deletePtsCurrContInRange()
{
  if( !updateAndVerifyRanges() || !isCurrContValid() )
    return;
  
  int numPtsDeleted = 0;
  
  undoContourDataChgCC( plug.view );      // REGISTER UNDO
  Icont *cont = getCurrCont();
  
  for( int p=0; p<psize(cont); p++ )
  {
    if( isBetweenAsc( plug.viewMin, getPtZInt(cont,p), plug.viewMax ) )
    {
      imodPointDelete( cont, p );
      p--;
      numPtsDeleted++;
    }
  }
  
  if(numPtsDeleted)
   undoFinishUnit( plug.view );           // FINISH UNDO
    
  wprint("Deleted %d points\n", numPtsDeleted);
  ivwRedraw( plug.view );
}

//------------------------
//-- Deletes all points within the current contour from the current point to the
//-- nearest end inclusive

bool BeadHelper::deletePtsUsingDAction()
{
  if( !updateAndVerifyRanges() || !isCurrPtValid() || plug.dKeyBehav == DK_NONE )
    return false;
  
  //## GET INFORMATION:
  
  Imod *imod = ivwGetModel(plug.view);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  Icont *cont = getCurrCont();
  int currPtZ = floor(getCurrPt()->z + 0.5);
  undoContourDataChgCC( plug.view );            // REGISTER UNDO
  
  //## DETERMINE WHAT Z RANGE TO DELETE ACCORDING TO "dKeyBehav" VALUE:
  
  int minZ = plug.viewMin;
  int maxZ = plug.viewMax;
  
  switch( plug.dKeyBehav )
  {
    case( DK_OPPOSITEMIDDLE ):
    {
      bool deleteUp = currPtZ >= plug.seedView;      // point is after seed view
      minZ = ( deleteUp ) ? currPtZ+1  : 0;
      maxZ = ( deleteUp ) ? plug.zsize : currPtZ-1;
      break;
    }
    case( DK_NEARESTEND ):
    {
      int numPts = psize( cont );
      int middlePt = numPts / 2.0f;
      bool deleteUp = ptIdx >= middlePt;    // point is closest to end pt
      
      minZ = ( deleteUp ) ? currPtZ+1  : 0;
      maxZ = ( deleteUp ) ? plug.zsize : currPtZ-1;
      break;
    }
  }
  
  //## DELETE POINTS FROM GIVEN RANGE:
  
  int numPtsDeleted = bead_deletePtsInZRange( cont, minZ, maxZ, true );
  
  if(numPtsDeleted)
  {
    undoFinishUnit( plug.view );        // FINISH UNDO
    
    ptIdx = bead_getClosestPtIdxToView( cont, currPtZ );
    imodSetIndex( imod, objIdx, contIdx, ptIdx );
  }
  
  wprint("Deleted %d points\n", numPtsDeleted);
  ivwRedraw( plug.view );
  return true;
}


//------------------------
//-- Deletes all points from each contour in range except the point in
//-- the seed view - effectively reducing each contour to its "seed"

void BeadHelper::reduceContsToSeed()
{
  if( !updateAndVerifyRanges() )
    return;
  

  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int numConts  = (plug.contMax - plug.contMin) + 1;
  string msg = "This operation will delete all but the seed point from \n  "
    + toString(numConts) + " contours ("
    + toString(plug.contMin+1) + "-" + toString(plug.contMax+1)
    + ")";
  
  static bool skipCheckedConts = true;
  CustomDialog ds;
  int ID_DUMMY         = ds.addLabel   ( msg.c_str() );
  int ID_SKIPCHECKEDC  = ds.addCheckBox( "skip checked contours",
                                         skipCheckedConts,
                                         "Will not delete any points from checked "
                                         "(stippled) contours " );
  GuiDialogCustomizable dlg(&ds, "Reduce Contours to Seed Point", this);
  dlg.exec();
  if( ds.cancelled )
    return;
  skipCheckedConts     = ds.getResultCheckBox  ( ID_SKIPCHECKEDC );
  
  
  //## REDUCE RANGE OF CONTOURS TO SEED:
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int numContsReduced = 0;
  int numPtsDeleted = 0;
  
  for( int c=MAX(plug.contMin,0); c<=plug.contMax && c<csize(obj); c++)
  {
    imodSetIndex(imod, objIdx, c, 0);
    Icont *cont = getCont( obj, c );
    
    if( skipCheckedConts && isInterpolated(cont) )
      continue;
    
    if( psize(cont) > 1 && bead_isPtOnView(cont,plug.seedView) )
    {
      Ipoint *pt = bead_getPtOnView(cont,plug.seedView);
      Ipoint seedPt = *pt;
      numPtsDeleted += psize(cont) - 1;
      undoContourDataChgCC( plug.view );      // REGISTER UNDO
      deleteAllPts(cont);
      imodPointAppend( cont, &seedPt );
      numContsReduced++;
    }
  }
  if(numContsReduced)
    undoFinishUnit( plug.view );                // FINISH UNDO
  
  //## OUTPUT RESULTS:
  
  wprint( "Reduced %d contours to seed - %d points deleted\n",
          numContsReduced, numPtsDeleted );
  imodSetIndex(imod, objIdx, contIdx, 0);
  ivwRedraw( plug.view );
}


//------------------------
//-- Deletes all points from the current contour except the point in the seed view - 
//-- effectively reducing it to its "seed" - and returns the number of points removed

void BeadHelper::reduceCurrContToSeed()
{
  if( !isCurrContValid() || isEmpty(getCurrCont()) )
    return;
  
  Icont *cont = getCurrCont();
  
  if( psize(cont) == 1 )
  {
    wprint("ERROR: Contour only has one point\n");
  }
  if ( bead_isPtOnView(cont,plug.seedView) )
  {
    int objIdx, contIdx, ptIdx;
    imodGetIndex( ivwGetModel(plug.view) , &objIdx, &contIdx, &ptIdx);
    
    Ipoint *pt = bead_getPtOnView(cont,plug.seedView);
    Ipoint seedPt = *pt;
    int numPtsToDel = psize(cont) - 1;
    
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    deleteAllPts(cont);
    imodPointAppend( cont, &seedPt );
    undoFinishUnit( plug.view );            // FINISH UNDO
    
    //## OUTPUT RESULT:
    wprint( "Reduced contour %d to seed - %d points deleted\n",
            contIdx+1, numPtsToDel );
    imodSetIndex( ivwGetModel(plug.view) , objIdx, contIdx, 0);
    ivwRedraw( plug.view );
  }
  else
  {
    wprint("ERROR: Current contour is missing middle point\n");
  }
}


//------------------------
//-- Allows user to select options for smoothing contours by moving points
//-- towards thier estimated position.

void BeadHelper::movePtsToEstimatedPosOptions()
{
  if( !updateAndVerifyRanges() )
    return;
  
  //## DISPLAY OPTIONS FOR SMOOTHING / MOVING POINTS:
  
  CustomDialog ds;
  int ID_CURRCONTONLY  = ds.addRadioGrp( "apply smoothing to:",
                                         "specified range of contours,"
                                         "current contour only [E],"
                                         "update values only (no action)",
                                         plug.smoothCurrContOnly );
  int ID_FILLGAPS      = ds.addCheckBox( "fill gaps", plug.smoothFillGaps,
                                         "Any missing points between existing points "
                                         "will be added" );
  int ID_BIGRESFIRST   = ds.addCheckBox( "move big residuals first",
                                         plug.smoothBigResidFirst,
                                         "If ticked: reorders points in order of "
                                         "descending distance from expected - which "
                                         "works well at correcting isolated bad points. "
                                         "If unticked: reorders point from the middle "
                                         "point upwards and then downwards.");
  int ID_YAXISONLY     = ds.addCheckBox( "change y value only", plug.smoothMoveYOnly,
                                         "All points will only be moved up/down along "
                                         "the Y (not left/right along the X)" );
  int ID_LEAVESEED     = ds.addCheckBox( "leave seed", plug.smoothLeaveSeed,
                                         "No points on the seed view will "
                                         "be moved" );
  int ID_LEAVEENDS     = ds.addCheckBox( "leave end points", plug.smoothLeaveEnds,
                                         "The first and last point in each contour will "
                                         "not be moved" );
  int ID_MOVEFRACT     = ds.addSpinBox ( "move fraction (X/10):", 1, 10,
                                         plug.smoothMoveFract*10.0f, 1,
                                         "If 10: points will be moved entire way to "
                                         "estimated positions" );
  int ID_MINRESID      = ds.addSpinBox ( "min residual to move (X/10):", 0, 1000, 
                                         plug.smoothMinResid*10.0f, 1,
                                         "Only points further than this from their "
                                         "estimated point will be moved (measured in "
                                         "tenths of a pixel)" );
  int ID_ITERATIONS    = ds.addSpinBox ( "iterations:", 1, 10, plug.smoothIterations, 1,
                                         "The more iterations, the further points "
                                         "will be moved" );
  
                         ds.addLabel("--------");
  int ID_ADJACENTV     = ds.addCheckBox( "only smooth adjacent views",
                                         plug.smoothAdjacentV,
                                         "Only the specified number of view above and "
                                         "below the current view will be smoothed" );
  int ID_NUMVEITHERS   = ds.addSpinBox ( "num views either side:", 1, 20,
                                         plug.smoothNumViews, 1,
                                         "How many views above and below the current "
                                         "view will be moved" );
  int ID_LEAVECURRZ    = ds.addCheckBox( "leave point on current view",
                                         plug.smoothLeaveCurrV, 
                                         "will not shift any points in the current view "
                                         "in the top most ZAP window" );
  
	GuiDialogCustomizable dlg(&ds, "Smoothing Options", this);
	dlg.exec();
	if( ds.cancelled )
		return;
	plug.smoothCurrContOnly       = ds.getResultRadioGrp ( ID_CURRCONTONLY );
  plug.smoothFillGaps           = ds.getResultCheckBox  ( ID_FILLGAPS );
  plug.smoothBigResidFirst      = ds.getResultCheckBox  ( ID_BIGRESFIRST );
  plug.smoothMoveYOnly          = ds.getResultCheckBox  ( ID_YAXISONLY );
  plug.smoothLeaveSeed          = ds.getResultCheckBox  ( ID_LEAVESEED );
  plug.smoothLeaveEnds          = ds.getResultCheckBox  ( ID_LEAVEENDS );
  plug.smoothMoveFract          = float( ds.getResultSpinBox( ID_MOVEFRACT ) ) * 0.1;
  plug.smoothMinResid           = float( ds.getResultSpinBox( ID_MINRESID  ) ) * 0.1;
  plug.smoothIterations         = ds.getResultSpinBox   ( ID_ITERATIONS );
  plug.smoothAdjacentV          = ds.getResultCheckBox ( ID_ADJACENTV );
  plug.smoothNumViews           = ds.getResultSpinBox   ( ID_NUMVEITHERS );
  plug.smoothLeaveCurrV         = ds.getResultCheckBox  ( ID_LEAVECURRZ );
  
  //## APPLY SMOOTHING:
  
  if ( plug.smoothCurrContOnly==0 )
    movePtsToEstimatedPosRange();
  else if ( plug.smoothCurrContOnly==1 )
    movePtsToEstimatedPosCurrCont();
}


//------------------------
//-- Applies smoothing options to a range of contours

void BeadHelper::movePtsToEstimatedPosRange()
{
  //## INTIALIZE VARIABLES: 
  
  int numConts  = (plug.contMax - plug.contMin) + 1;
  int numViews = (plug.viewMax - plug.viewMin) + 1;
  int numPts = numConts * numViews;
  
  string msg = "This operation will move ALL points "
    "\n  on " + toString(numConts) + "  contours ("
    + toString(plug.contMin+1) + "-" + toString(plug.contMax+1)
    + ")\n  on " + toString(numViews) + " views ("
    + toString(plug.viewMin+1) + "-" + toString(plug.viewMax+1) + ")."
    + "\n  Total points = " + toString(numPts)
    + "\nAre you sure you want to continue ?";
  
  if( !MsgBoxYesNo(this, msg) )
    return;
  
  int currView = edit_getZOfTopZap();
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
    
  int numPtsMoved = 0;
  int numPtsAdded = 0;
  
  //## ITERATE THROUGH SPECIFIED RANGE OF CONTOURS AND MOVE ALL POINTS WITHIN
  //## SPECIFIED VIEWS TO THEIR EXPECTED POSITION: 
  
  for( int c=MAX(plug.contMin,0); c<=plug.contMax && c<csize(obj); c++)
  {
    imodSetIndex(imod, objIdx, c, 0);
    Icont *cont = getCont( obj, c );
    
    int ptsMoved = 0;
    int ptsAdded = 0;
    
    undoContourDataChg( plug.view, objIdx, c );       // REGISTER UNDO
    
    bead_smoothPtsUsingPlugSettings( cont, ptsMoved, ptsAdded );
    
    numPtsMoved += ptsMoved;
    numPtsAdded += ptsAdded;
  }
  
  if( numPtsMoved || numPtsAdded )                      // FINISH UNDO
    undoFinishUnit( plug.view );
  
  //## OUTPUT RESULTS:
  
  wprint( "Moved %d points and added %d points\n", numPtsMoved, numPtsAdded );
  imodSetIndex(imod, objIdx, contIdx, ptIdx);
  ivwRedraw( plug.view );
}


//------------------------
//-- Applies smoothing options to a current contour only

void BeadHelper::movePtsToEstimatedPosCurrCont()
{
  if( !isCurrContValid() || isEmpty(getCurrCont()) )
  {
    MsgBox("ERROR: Have not selected a valid contour");
    return;
  }
  
  //## SMOOTH CURRENT CONTOUR USING SMOOTHING SETTING:
  
  int ptsMoved = 0;
  int ptsAdded = 0;
  
  Icont *cont = getCurrCont();
  undoContourDataChgCC( plug.view );       // REGISTER UNDO
  
  bead_smoothPtsUsingPlugSettings( cont, ptsMoved, ptsAdded );
  
  if( ptsMoved || ptsAdded )                      // FINISH UNDO
    undoFinishUnit( plug.view );
  
  //## OUTPUT RESULTS:
  
  wprint( "Moved %d points and added %d points to contour\n", ptsMoved, ptsAdded );
  ivwRedraw( plug.view );
}


//------------------------
//-- Moves the point in the current contour on the current view to it's 
//-- estimated position, based on the position of the points before and/or after it.
//-- Note that if there is no point on the current view a new point is added.

void BeadHelper::moveCurrPtToEstimatedPos()
{
  if( !isCurrObjValid() )
    return;
  
  Ipoint estPt;
  int currView = edit_getZOfTopZap();
  Imod *imod = ivwGetModel(plug.view);
  Icont *cont = getCurrCont();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  if( bead_getExpectedPosOfPoint( cont, currView, &estPt ) )
  {
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    ptIdx = bead_insertOrOverwritePoint( cont, &estPt );
    undoFinishUnit( plug.view );            // FINISH UNDO
  }
  
  imodSetIndex(imod, objIdx, contIdx, ptIdx);
  ivwRedraw( plug.view );
}

//------------------------
//-- Fills any missing points in the specified range of views for the specified
//-- contours.

void BeadHelper::fillMissingPts()
{
  if( !updateAndVerifyRanges() )
    return;
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int numConts  = (plug.contMax - plug.contMin) + 1;
  int numViews = (plug.viewMax - plug.viewMin) + 1;
  string msg = "This operation will fill missing points on "
    + toString(numConts) + " contours ("
    + toString(plug.contMin+1) + "-" + toString(plug.contMax+1)
    + ") on " + toString(numViews) + " views ("
    + toString(plug.viewMin+1) + "-" + toString(plug.viewMax+1) + ")";
  
  static bool fillPastEnds = false;
  
	CustomDialog ds;
  int ID_DUMMY         = ds.addLabel   ( msg.c_str() );
  int ID_FILLPASTENDS  = ds.addCheckBox( "fill past ends",
                                         fillPastEnds,
                                         "Will add points past the start and end "
                                         "point of each contour based on their "
                                         "estimated postions" );
	GuiDialogCustomizable dlg(&ds, "Fill Missing Points", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  fillPastEnds        = ds.getResultCheckBox  ( ID_FILLPASTENDS );
  
  
  //## FILL MISSING POINTS:
  
  int numContsChanged = 0;
  int numPtsAddedTotal = 0;
  
  for( int c=MAX(plug.contMin,0); c<=plug.contMax && c<csize(obj); c++)
  {
    imodSetIndex(imod, objIdx, c, 0);
    Icont *cont = getCont( obj, c );
    
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    int numPtsAdded = bead_fillMissingPtsOnCont( getCurrCont(),
                                                 plug.viewMin, plug.viewMax,
                                                 fillPastEnds );
    
    numPtsAddedTotal += numPtsAdded;
    if( numPtsAdded )
      numContsChanged++;
  }
  
  if( numPtsAddedTotal )
    undoFinishUnit( plug.view );            // FINISHED UNDO
  
  //## OUTPUT RESULTS:
  
  imodSetIndex(imod, objIdx, contIdx, ptIdx);
  wprint("Added %d points to %d contours\n", numPtsAddedTotal, numContsChanged);
  ivwRedraw( plug.view );
}


//------------------------
//-- Fills any missing points in the current contour over all views.

void BeadHelper::fillMissingPtsCurrCont( bool fillPastEnds )
{
  if( !isCurrContValid() )
    return;
  
  undoContourDataChgCC( plug.view );      // REGISTER UNDO
  int numPtsAdded = bead_fillMissingPtsOnCont( getCurrCont(), 0, plug.zsize-1,
                                               fillPastEnds );
  if( numPtsAdded )
    undoFinishUnit( plug.view );          // FINISHED UNDO
  
  wprint("Added %d points to contour\n", numPtsAdded);
  ivwRedraw( plug.view );
}


//------------------------
//-- Gives a choice of several other options for the user.

void BeadHelper::moreActions()
{
  updateAndVerifyRanges();
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds;
  int ID_ACTION = ds.addRadioGrp( "action (for current object):",
                                  "calculate tilt axis angle,"
                                  "show fiducials on bottom as purple,"
                                  "show contour turning points,"
                                  "show grid,"
                                  "clear purple object,"
                                  "move contours between objects,"
                                  "verify contours"
                                     "\n(reorders pts and removes duplicates),"
                                  "mark contours as checked/unchecked,"
                                  "print contour info,"
                                  "load tilt angles",
                                  plug.selectedAction,
                                  "",
                                  "Estimates the angle of the tilt axis by averaging the "
                                    "\nangles for the 'line of best' for each contour,"
                                  "Uses the direction of movement of fiducials to "
                                    "\nguess which fidicials are on the bottom and "
                                    "\nshows these in purple using the 'purple object',"
                                  "Uses the 'purple object' to show the point in each "
                                    "\ncontour where the line changes direction.... "
                                    "\nwhich turns out to a fairly useless feature,"
                                  "Uses the 'purple object' to display a grid "
                                    "\nwith your desired number of rows and columns,"
                                  "Clears the infamous 'purple object' after you've "
                                    "\napplied one of three actions above,"
                                  "Use this to move/copy a range of contours/seeds in "
                                    "\nthe current object to another object. "
                                    "\nThis is useful because each object is tracked "
                                    "\nseperately and you'll often yeild better results "
                                    "\nfor a set of points if you first track a subset "
                                    "\nand then later move the other points back,"
                                  "Can be used to quickly fix the common problems "
                                    "whereby you've accidently added two points "
                                    "in the same view or they are not in order,"
                                  "Use this to change the checked/unchecked value of "
                                    "your contours,"
                                  "Prints some basic numbers including the number of "
                                    "\ncomplete contours; incomplete contours; "
                                    "\nchecked contours and also number of "
                                    "\nmissing points,"
                                  "Loads the calculated tilt angle of all views from "
                                    "\nthe appropriate a .tlt file");
	GuiDialogCustomizable dlg(&ds, "Perform Action", this);
	dlg.exec();
	if( ds.cancelled )
		return;
	plug.selectedAction = ds.getResultRadioGrp	( ID_ACTION );
  
  switch(plug.selectedAction)
  {
    case(0):      // calculate tilt axis angle
    {
      float tiltAxisAngleEst = bead_estimateTiltAngle();
      if(tiltAxisAngleEst == 0)
      {
        MsgBox( "Was unable to measure any angles based on contours provided" );
      }
      else
      {
        string msg = "Current tilt axis angle = " + toString( plug.tiltAxisAngle, 4) 
        + DEGREE_SIGN + "\nEstimated tilt axis angle = " + toString(tiltAxisAngleEst, 4)
        + DEGREE_SIGN + "\n... Change tilt axis angle to this value?";
        if( MsgBoxYesNo( this, msg ) )
          plug.tiltAxisAngle = tiltAxisAngleEst;
      }
    } break;
    
    case(1):      // show fiducials on bottom as purple
    {
      CustomDialog dsB;
      
      int static minViewR = plug.seedView;
      int static maxViewR = plug.seedView + 5;
      
      int ID_DUMMY = dsB.addLabel   ( "... select range of views where top \n"
                                     "and bottom fiducials appear to move in \n"
                                     "opposite directions (suggest a range of ~5):" );
      int ID_MIN   = dsB.addSpinBox ( "min view:", 1, plug.xsize, minViewR, 1 );
      int ID_MAX   = dsB.addSpinBox ( "max view:", 1, plug.xsize, maxViewR, 1 );
      
      GuiDialogCustomizable dlgB(&dsB, "Show Bottom Contours", this);
      dlgB.exec();
      if( dsB.cancelled )
        return;
      minViewR      = dsB.getResultSpinBox	( ID_MIN );
      maxViewR      = dsB.getResultSpinBox	( ID_MAX );
      
      if( minViewR >= maxViewR )
      {
        MsgBox( "Bad range given !" );
        return;
      }
      
      bead_showBottomContoursInPurple( minViewR, maxViewR );
    } break;
    
    case(2):      // show contour turning points
    {
      bead_showContourTurningPts();
    } break;
    
    case(3):      // show grid
    {
      bead_showGrid();
    } break;
      
    case(4):      // clear purple object
    {
      ivwClearAnExtraObject(plug.view, plug.extraObjExtra);
    } break;
    
    case(5):      // move points between objects
    {
      moveMultipleContours();
    } break;
      
    case(6):      // remove duplicate pts from object
    {
      correctCurrentObject();
    } break;
    
    case(7):      // mark all contours as stippled/unchecked
    {
      markRangeAsStippled();
    } break;
    
    case(8):      // print contour info
    {
      printContourCheckedInfo();
    } break;
    
    case(9):      // load tilt angles
    {
      openTiltAngleFile();
    } break;
  }
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Allows user to change other plugin values/settings.

void BeadHelper::moreSettings()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds;
  
  
  ds.addLabel   ("----- TILT STACK: -----");
  
  int ID_MIDDLESLICE    = ds.addSpinBox ( "seed view:",
                                          1, plug.zsize+1, plug.seedView+1, 1,
                                          "The view used to seed points - this is "
                                          "usually the middle-most view, but not "
                                          "in all cases (NOTE: This settings is not "
                                          "saved on exit)"
                                          "\n\nNOTE: You'll often have to reduce "
                                          "this value by 1 on b axis stacks" );
  int ID_TILTINCREMENT  = ds.addLineEdit( "tilt increment:           ",
                                          toString(plug.tiltIncrement).c_str(),
                                          "The angle (in degrees) the specimen "
                                          "was tilted between subsequent views"
                                          "\nNOTE: This value becomes irrelevant after "
                                          "loading the calculated tilt angles via "
                                          "'More Actions' > 'load tilt angles'" );
  int ID_TILTANGLE      = ds.addLineEdit( "tilt axis angle:             ",
                                          toString(plug.tiltAxisAngle).c_str(),
                                          "The angle (in degrees) clockwise from "
                                          "vertical about which the views rotate"
                                          "\nNOTE: This can be calculated in "
                                          "'More Actions' > 'calculate tilt axis angle'");
  int ID_TILTOFFSET     = ds.addSpinBox ( "tilt x offset:", -200, 200,
                                          plug.tiltOffsetX, 1,
                                          "How far the tilt axis is shifted along X "
                                          "from passing through the center" );
  int ID_BIGHOLEGRID   = ds.addSpinBox (  "biggest hole grid size:",
                                          1, 1000, plug.biggestHoleGrid, 5,
                                          "The distance between grid points used to "
                                          "search for the point furthest from any "
                                          "seed points and the edge of the biggest "
                                          "hold grid"
                                          "\n\nSet 'tilt display' to '[h] grid' to "
                                          "see the effect");
  int ID_BIGHOLEOFFSET  = ds.addSpinBox ( "biggest hole grid inset:",
                                          -10000, (plug.xsize/2)+24,
                                          plug.biggestHoleInset, 10,
                                          "The distance (in pixels) to inset the "
                                          "biggest hole grid [h] such that: "
                                          "\n  a large value will concentrate "
                                          "points in the middle and "
                                          "\n  a negative value encourages points "
                                          "towards the edge as you press 'h'."
                                          "\n\nSet 'tilt display' to '[h] grid' to "
                                          "see the effect");
  
  ds.addLabel   ("----- DISPLAY: -----");
  
  int ID_EXPPTDISPLAY   = ds.addComboBox( "estimated pt display:",
                                          "cross,"
                                          "diamond,"
                                          "arrow", plug.expPtDisplayType,
                                          "Symbol used to display the estimated point" );
  int ID_EXPPTSIZE      = ds.addSpinBox ( "estimated pt size:",
                                           1, 200, plug.expPtSize, 1,
                                          "The size of the estimated point symbol in "
                                          "screen pixels" );
  int ID_LINESPHERE     = ds.addSpinBox ( "line display spheres:",
                                          0, 50, plug.sizeLineSpheres, 1,
                                          "The size of points used to display contours "
                                          "as lines" );
  int ID_LINEWIDTH      = ds.addSpinBox ( "line display width:",
                                          1, 50, plug.lineDisplayWidth, 1,
                                          "The thickness of lines used to display "
                                          "contours as lines" );
  int ID_PURPLESPHERE   = ds.addSpinBox ( "size purple spheres:",
                                          1, 200, plug.sizePurpleSpheres, 1,
                                          "The size of points in the extra purple object "
                                          "(used to show bottom fiducials etc)" );
  
  ds.addLabel   ("----- OTHER: -----");
  
  int ID_AUTOSAVE       = ds.addCheckBox( "save settings on close", 
                                          plug.autoSaveSettings,
                                          "Automatically saves all your Bead Helper "
                                          "settings to 'beadhelpersettings.txt' "
                                          "when you close 3dmod, so they will load "
                                          "next time you open 3dmod");
  
	GuiDialogCustomizable dlg(&ds, "More Settings", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  
  plug.seedView              = ds.getResultSpinBox  ( ID_MIDDLESLICE ) - 1;
  string tiltIncrAngleStr    = ds.getResultLineEdit	( ID_TILTINCREMENT );
	string tiltAxisAngleStr    = ds.getResultLineEdit	( ID_TILTANGLE );
  plug.tiltOffsetX           = ds.getResultSpinBox  ( ID_TILTOFFSET );
  plug.biggestHoleGrid       = ds.getResultSpinBox  ( ID_BIGHOLEGRID );
  plug.biggestHoleInset      = ds.getResultSpinBox  ( ID_BIGHOLEOFFSET );
  
  plug.expPtDisplayType      = ds.getResultComboBox ( ID_EXPPTDISPLAY );
  plug.expPtSize             = ds.getResultSpinBox  ( ID_EXPPTSIZE );
  plug.sizeLineSpheres       = ds.getResultSpinBox  ( ID_LINESPHERE );
  plug.lineDisplayWidth      = ds.getResultSpinBox  ( ID_LINEWIDTH );
  plug.sizePurpleSpheres     = ds.getResultSpinBox  ( ID_PURPLESPHERE );
  
  plug.autoSaveSettings      = ds.getResultCheckBox ( ID_AUTOSAVE );
  
  
  float newTiltAxisAngle = string_getFloatFromString( tiltAxisAngleStr );
  if( newTiltAxisAngle < -200 || newTiltAxisAngle >200 )
    wprint("\aERROR: Invalid tilt axis angle entered"); 
  else
    plug.tiltAxisAngle = newTiltAxisAngle;
  
  float newTiltIncrAngle = string_getFloatFromString( tiltIncrAngleStr );
  if( newTiltIncrAngle <= 0 || newTiltIncrAngle >20 )
    wprint("\aERROR: Invalid tilt increment entered"); 
  else
  {
    if(  plug.tiltIncrement != newTiltIncrAngle);
    {
      plug.tiltIncrement = newTiltIncrAngle;
      verifyTiltIncrement(true, true);
    }
  }
  
  Iobj *xobjC = ivwGetAnExtraObject(plug.view, plug.extraObjContDisp);
  imodObjectSetValue(xobjC, IobjPointSize, plug.sizeLineSpheres);
  imodObjectSetValue(xobjC, IobjLineWidth2, plug.lineDisplayWidth);
  
  Iobj *xobjX = ivwGetAnExtraObject(plug.view, plug.extraObjExtra);
  imodObjectSetValue(xobjX, IobjPointSize, plug.sizePurpleSpheres);
  
  ivwRedraw( plug.view );
}



//------------------------
//-- Allows user to change other plugin values/settings.

void BeadHelper::keyboardSettings()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds;
  
  ds.addLabel   ("----- MOUSE: -----");
  
  int ID_MOUSEBEHAV     = ds.addComboBox( "wheel behavior:",
                                          "none,"
                                          "scroll points,"
                                          "scroll views,"
                                          "smart scroll", plug.wheelBehav,
                                          "Allows you to use the mouse wheel to "
                                          "scroll through points and/or views" ); 
  int ID_WHEELRESIST    = ds.addSpinBox ( "wheel resistance:",
                                          10, 1000, plug.wheelResistance, 10,
                                          "The higher the value, the slower "
                                          "mouse scrolling works (useful if you have "
                                          "a touchy mouse wheel)" );
  
  ds.addLabel   ("----- KEYBOARD: -----");
  
  
  int ID_DISABLEHOTKEYS = ds.addCheckBox( "disable all hot keys", 
                                          plug.disableHotKeys,
                                          "Disables all BeadHelper hot keys in case "
                                          "they conflict with another key you "
                                          "need to press \n\nNOTE: this setting "
                                          "defaults to off and is not saved on exit");
  int ID_CHECKENDS      = ds.addCheckBox( "include seed && end pts on [y],[b]&&[w]", 
                                          plug.includeEndsResid,
                                          "Includes end points when searching for the "
                                          "point with the next biggest y jump "
                                          "or deviation from expected when "
                                          "[y], [b] or [w] is pressed"
                                          "\n\nRECOMMENDED VALUE: off");
  int ID_SEARCHRANGE    = ds.addCheckBox( "on [y]&&[b] search view range only", 
                                          plug.searchRangeOnly,
                                          "Only searches points within the specified "
                                          "range of views (in the 'Range' box above) "
                                          "when searching for the "
                                          "point with the next biggest y jump [y] "
                                          "or deviation from expected [b] "
                                          "\n\nNOTE: this value is not saved on exit "
                                          "and defaults to off");
  int ID_CONTSTOSEARCH  = ds.addComboBox( "[y],[b]&[o] searches: ",
                                          "all contours,"
                                          "unchecked only,"
                                          "checked only", plug.contsToSearch,
                                          "Which contours are searched when [y] "
                                          "(biggest y jump), [b] (biggest deviation "
                                          "from expected) or [o] (worst contour) "
                                          "keys are pressed");
  
  int ID_WCURRCONT      = ds.addCheckBox( "[w] checks current contour only",
                                          plug.wCurrContOnly,
                                          "If true: finds biggest weighted value "
                                          "in current contour. If false: searches "
                                          "all contours in current object.");
  int ID_WDIVISOR       = ds.addSpinBox ( "[w] divisor number:",
                                          1, 500, plug.wWeightedDiv, 5,
                                          "The lower this number, the more likely "
                                          "to select small shifts \namoungs closely "
                                          "placed points.... \nuses the formula: "
                                          "weighted_dev = distance_to_expected_pt / "
                                          "(distance_nearest_pts + wWeightedDiv)"
                                          "\n\nRECOMMEDED VALUE: 15" );
  
  int ID_DKEYACTION     = ds.addComboBox( "on [d] delete pts:",
                                          "do nothing,"
                                          "opposite seed,"
                                          "to nearest end,"
                                          "specified range", plug.dKeyBehav,
                                          "Action performed when [d] is pressed."
                                          "\n"
                                          "\n > do nothing - as it sounds"
                                          "\n > opposite seed - if selected point is "
                                            "above the seed point will delete all "
                                            "above it; else deletes all points below"
                                          "\n > to nearest end - deletes all points in "
                                            "the current contour from the selected "
                                            "point to the closest end"
                                          "\n > specified range - deletes the range of "
                                            "views speified in the 'Range' group box "
                                            "above from the current contour");
  
  int ID_MKEYACTION     = ds.addComboBox( "on [m]:",
                                          "normal,"
                                          "go to middle pt,"
                                          "smooth local,"
                                          "smooth y local",
                                          plug.mKeyBehav,
                                          "Action performed when [m] is pressed."
                                          "\n"
                                          "\n > normal    - toggles movie/model mode"
                                          "\n > go to middle pt - goes to seed point "
                                          "and/or middle (seed) view"
                                          "\n > smooth local  - applies smoothing "
                                          "to the pts in the current contour within "
                                          "8 views of the current view"
                                          "\n > smooth y local    - as above but "
                                          "moves points in Y only" );
  
  ds.addLabel   ("");
  
  int ID_ENTERACTION    = ds.addComboBox( "on [Enter] go to:",
                                          "do nothing,"
                                          "next unchecked,"
                                          "prev uncheced,"
                                          "next checked,"
                                          "next contour", plug.enterAction,
                                          "Action performed when [Enter] is pressed "
                                          "\n\nNOTE: If you don't use enter you may, "
                                          "wish to leave this on 'do nothing' because "
                                          "it can interfer when you press enter in "
                                          "other windows/plugins");
  int ID_MINPTSENTER   = ds.addSpinBox  ( "min points for [Enter]:", 
                                          0, plug.zsize+10, plug.minPtsEnter, 1,
                                          "The minimum number of points a contour "
                                          "must have to be jumped to when [Enter] "
                                          "is pressed" );
  int ID_MAXPTSENTER   = ds.addSpinBox  ( "max points for [Enter]:", 
                                          1, plug.zsize+10, plug.maxPtsEnter, 1,
                                          "The maximum number of points a contour "
                                          "must have to be jumped to when [Enter] "
                                          "is pressed"
                                          "\n\nNOTE: Unlike 'min points' this value is "
                                          "not saved on exit and instead defaults to "
                                          "the number of views" );
  int ID_ENTERPRINT    = ds.addCheckBox ( "print results on [Enter]", plug.enterPrint,
                                          "Prints the number of contours matching the "
                                          "above criteria each time enter is pressed" );
  
  
	GuiDialogCustomizable dlg(&ds, "Mouse and Keyboard Settings", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  plug.wheelBehav            = ds.getResultComboBox ( ID_MOUSEBEHAV );
  plug.wheelResistance       = ds.getResultSpinBox  ( ID_WHEELRESIST );
  
  plug.disableHotKeys        = ds.getResultCheckBox ( ID_DISABLEHOTKEYS );
  plug.includeEndsResid      = ds.getResultCheckBox ( ID_CHECKENDS );
  plug.searchRangeOnly       = ds.getResultCheckBox ( ID_SEARCHRANGE );
  plug.contsToSearch         = ds.getResultComboBox ( ID_CONTSTOSEARCH );
  plug.dKeyBehav             = ds.getResultComboBox ( ID_DKEYACTION );
  plug.mKeyBehav             = ds.getResultComboBox ( ID_MKEYACTION );
  plug.wCurrContOnly         = ds.getResultCheckBox ( ID_WCURRCONT );
  plug.wWeightedDiv          = ds.getResultSpinBox  ( ID_WDIVISOR );
  plug.enterAction           = ds.getResultComboBox ( ID_ENTERACTION );
  plug.minPtsEnter           = ds.getResultSpinBox  ( ID_MINPTSENTER );
  plug.maxPtsEnter           = ds.getResultSpinBox  ( ID_MAXPTSENTER );
  plug.enterPrint            = ds.getResultCheckBox ( ID_ENTERPRINT );
  
  if( plug.minPtsEnter > plug.maxPtsEnter )
  {
    MsgBox("WARNING: 'min points on [Enter]' value is "
           "\n   greater than 'max point on [Enter]. Please fix this.");
  }
  
  ivwRedraw( plug.view );
}



//------------------------
//-- Prompts for a criteria for reordering, and reorderes specified range of contours
//-- using this criteria.

void BeadHelper::reorderContours()
{
  if( !updateAndVerifyRanges() )
    return;
  
  int nConts = csize(getCurrObj());
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool reverseOrder = false;
  static bool printVals    = true;
  static bool calcValsOnly = false;
  
	CustomDialog ds;
  int ID_DUMMY         = ds.addLabel   ( "contours to sort (inclusive):" );
  int ID_CONTMIN       = ds.addSpinBox ( "min:", 1, nConts, plug.contMin+1, 1,
                                         "Only contours after this contour "
                                         "(inclusive) will be reordered" );
  int ID_CONTMAX       = ds.addSpinBox ( "max:", 1, nConts, nConts, 1,
                                         "Only contours BEFORE this contour "
                                         "(inclusive) will be reordered" );
	int ID_SORTCRITERIA  = ds.addRadioGrp( "sort by:         (sort criteria)",
                                         "y jumps (asc),"
                                         "deviation (asc),"
                                         "avg grey value (desc),"
                                         "dist from middle (asc),"
                                         "num missing pts (asc),"
                                         "checked first,"
                                         "random,",
                                         plug.sortCriteria,
                                         "",
                                         "Sorts based on how far points jump in Y,"
                                         "Uses a weighted score of how far points are "
                                           "from their estimated positions,"
                                         "Average the grey value closest to each point,"
                                         "The distance of the seed point from the "
                                            "dead center of the tomogram in X an Y,"
                                         "Contours with the least points will be moved "
                                            "to the end,"
                                         "Moves checked (stippled) contours to the start "
                                            "and unchecked to the end,"
                                         "Uses a random number for each contour" );
  int ID_CALVALS       = ds.addCheckBox( "calc values only (don't reorder)",
                                         calcValsOnly,
                                         "No contours will be reordered, but "
                                         "values will be calculated and you "
                                         "can iterate from largest to smallest "
                                         "by pressing 'o'" );
	int ID_REVERSE       = ds.addCheckBox( "reverse order", reverseOrder );
  int ID_PRINTVALS     = ds.addCheckBox( "print values", printVals );
	GuiDialogCustomizable dlg(&ds, "Sorting Options", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  calcValsOnly        = ds.getResultCheckBox  ( ID_CALVALS );
  int contMin         = ds.getResultSpinBox   ( ID_CONTMIN ) - 1;
  int contMax         = ds.getResultSpinBox   ( ID_CONTMAX ) - 1;
  reverseOrder        = ds.getResultCheckBox  ( ID_REVERSE );
  printVals           = ds.getResultCheckBox  ( ID_PRINTVALS );
	plug.sortCriteria		= ds.getResultRadioGrp	( ID_SORTCRITERIA );
  
  bead_reorderConts( plug.sortCriteria, contMin, contMax, calcValsOnly,
                     reverseOrder, printVals );
  
  ivwRedraw( plug.view );
}


//------------------------
//-- Moves the current contour to a different index within the current object.
//-- Prompts the user what position to move the contour.

void BeadHelper::moveContour()
{
  if( !isCurrContValid() )
    return;
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
  Icont *cont = getCurrCont();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int lastCont = csize( obj );
  bool ok;
  int newContIdx = QInputDialog::getInteger( "Move contour", "Move to:",
                                             lastCont, 1, lastCont, 1, &ok, this ) - 1;
  if ( ok && contIdx != newContIdx )
  {
    Icont *contCopy = imodContourDup( cont );
    undoContourRemovalCO( plug.view, contIdx );                 // REGISTER UNDO
    imodObjectRemoveContour( obj, contIdx );
    undoContourAddition( plug.view, objIdx, newContIdx );       // REGISTER UNDO
    imodObjectInsertContour( obj, contCopy, newContIdx );
    undoFinishUnit( plug.view );                                // FINISH UNDO
    imodSetIndex(imod, objIdx, newContIdx, ptIdx);
    ivwRedraw( plug.view );
  }
}



//------------------------
//-- Moves the current contour to a different index within the current object.
//-- Prompts the user what position to move the contour.

void BeadHelper::moveMultipleContours()
{
  if ( !updateAndVerifyRanges() || !isCurrObjValid() )
    return;
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int numObjs = osize(imod);
  int maxContIdx = csize( obj )-1;
  
  if( numObjs == 1 )
  {
    if ( !MsgBoxYesNo(this, "There is only one object, add another?") )
      return;
    imodNewObject(imod);
    numObjs = osize(imod);
    Iobj *objNew = getObj(imod, numObjs-1);
    imodObjectSetValue(objNew, IobjFlagClosed, 0);
    imodObjectSetValue(objNew, IobjPointSize, 3);
    imodSetIndex( imod, objIdx, contIdx, ptIdx );
  }
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds;
  int ID_CONTMIN       = ds.addSpinBox ( "min cont:", 1, maxContIdx+1, plug.contMin+1, 1 );
  int ID_CONTMAX       = ds.addSpinBox ( "max cont:", 1, maxContIdx+1, plug.contMax+1, 1 );  
	int ID_OBJ           = ds.addSpinBox ( "move to object:", 1, numObjs, numObjs, 1 );
	int ID_SEEDONLY      = ds.addCheckBox( "seed pt only", true );
  int ID_DELETEMATCH   = ds.addCheckBox( "delete matching seeds", true );
  int ID_COPY          = ds.addCheckBox( "copy", false );
	GuiDialogCustomizable dlg(&ds, "Move Options", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  int contMin         = MAX( ds.getResultSpinBox( ID_CONTMIN ) - 1, 0);
  int contMax         = MIN( ds.getResultSpinBox( ID_CONTMAX ) - 1, maxContIdx);
  int objToIdx        = ds.getResultSpinBox   ( ID_OBJ ) - 1;
  bool seedOnly       = ds.getResultCheckBox  ( ID_SEEDONLY );
  bool deleteMatch    = ds.getResultCheckBox  ( ID_DELETEMATCH );
	bool copy       		= ds.getResultCheckBox	( ID_COPY );
  
  
  if ( objToIdx == objIdx &&
       !MsgBoxYesNo(this, "You are copying to the same object.... are you SURE?!") )
    return;
  
  //## FOR EACH CONTOUR IN RANGE: ADD CONTOUR OR SEED TO THE END OF THE DESIGNATED
  //## OBJECT UNLESS IT MATCHES A SEED CURRENTLY IN THAT OBJECT
  
  int numMatchingSeeds = 0;
  int numContsCopied = 0;
  Iobj *objTo = getObj(imod,objToIdx);
  
  for(int c=contMin; c<=contMax; c++)
  {
    Icont  *cont   = getCont(obj, c);
    setDeleteFlag( cont, 0 );     // clear delete flag
    
    if( isEmpty(cont) )
    {
      setDeleteFlag( getCont(obj, c), 1 );
      continue;
    }
    
    bool isSeedPt  = bead_isPtOnView( cont, plug.seedView );
    Ipoint *seedPt = bead_getClosestPtToView( cont, plug.seedView );
    
    if( seedOnly && !isSeedPt )     // if we want to copy seed pt only, but is none:
      continue;                       // don't copy and don't flag for delete
        
    Icont *contCopy = imodContourDup( cont );   // create copy of contour (don't delete)
    if( seedOnly )                              // if we only want the seed point:
    {
      deleteAllPts( contCopy );
      imodPointAppendXYZ( contCopy, seedPt->x, seedPt->y, seedPt->z );
    }
    
    undoContourAddition( plug.view, objToIdx, c );              // REGISTER UNDO
    imodObjectAddContour( objTo, contCopy );                    // copy contour
    free(contCopy);
    numContsCopied++;
    
    if( !copy )
      setDeleteFlag( cont, 1 );
  }
  
  
  //## REMOVE ANY CONTOURS FLAGGED FOR DELETE:
  
  int numContsDeleted = 0;
  for(int c=contMax; c>=contMin; c--)   // for all contours in range:
  {
    if ( isDeleteFlag( getCont(obj,c) ) )   // if delete flag is set:
    {
      undoContourRemoval(plug.view, objIdx, c);   // REGISTER UNDO
      imodObjectRemoveContour( obj, c );          // delete contour
      numContsDeleted++;
    }
  }
  
  if( numContsDeleted || numContsCopied )
    undoFinishUnit( plug.view );                                // FINISH UNDO
  
  imodSetIndex( imod, objToIdx, 0, 0 );
  
  //## PRINT SUMMARY OF ANALYSIS:
  
  wprint( "SUMMARY OF MOVE: \n" );
  wprint( " %d seeds were moved to obj %d \n",    numContsCopied, objToIdx+1 );
  wprint( " %d seeds were removed from obj %d \n", numContsDeleted, objIdx+1  );
  wprint( "  WARNING: there were %d matching seeds\n", numMatchingSeeds );
  
  ivwRedraw( plug.view );
}


//------------------------
//-- Corrects and contours with duplicate points on same view or
//-- contours marking the same fiducial.

void BeadHelper::correctCurrentObject()
{
  if( !isCurrObjValid() )
  {
    MsgBox( "Select a valid object first" );
    return;
  }
  
	CustomDialog ds;
	int ID_EXTRAPTS    = ds.addCheckBox( "delete duplicate pts on same view", true,
                                        "If two (or more) points lie on the same view "
                                        "of the same contour delete the second" );
  int ID_MATCHSEED   = ds.addCheckBox( "delete duplicate seeds", true,
                                        "If two (or more) contours are on the same "
                                        "fiducial on the seed view delete the "
                                        "second one" );
  int ID_POINTORDER  = ds.addCheckBox( "correct bad point order", true,
                                       "If you somehow managed to put contour "
                                       "point in the wrong order this will reorder "
                                       "the points in ascending Z value" );
	GuiDialogCustomizable dlg(&ds, "Correction Options", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  bool deleteExtraPts   = ds.getResultCheckBox  ( ID_EXTRAPTS );
  bool deleteMatchSeeds = ds.getResultCheckBox  ( ID_MATCHSEED );
  bool correctPtOrder   = ds.getResultCheckBox  ( ID_POINTORDER );
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  wprint("VERIFYING CONTOURS   (for object %d)\n", objIdx+1);
  int numProblems = 0;
  
  //## FOR EACH CONTOUR: CHECK POINTS ARE IN ORDER
  
  for( int c=0; c<csize(obj); c++)
  {
    Icont *cont = getCont( obj, c );
    
    if( bead_ptsAreNotInOrder(cont) )
    {
      numProblems++;
      if( correctPtOrder )
      {
        undoContourDataChg(plug.view, objIdx, c);   // REGISTER UNDO
        bead_orderPtsByAscendingZ( cont );
        wprint("\a > Corrected point order in contour %d\n", c+1 );
      }
      else
      {
        wprint("\a > Out of order points found in contour %d\n", c+1 );
      }
    }
  }
  
  //## CHECK FOR MATCHING SEED POINTS:
  
  Icont *seedPts = imodContourNew();
  for( int c=0; c<=csize(obj); c++)
  {
    Icont *cont = getCont( obj, c );
    bool isSeedPt  = bead_isPtOnView( cont, plug.seedView );
    Ipoint *seedPt = bead_getClosestPtToView( cont, plug.seedView );
    
    if( isSeedPt )
    {
      bool matchFound = false;
      
      for(int p=0; p<psize(seedPts); p++)
      {
        float dist = line_distBetweenPts2D( seedPt, getPt(seedPts,p) );
        if( dist < 3 )
          matchFound = true;
      }
      if( matchFound )
      {
        numProblems++;
        if( deleteMatchSeeds )
        {
          undoContourRemoval(plug.view, objIdx, c);   // REGISTER UNDO
          imodObjectRemoveContour( obj, c );          // delete contour
          wprint("\a > Deleted duplicate seed at: cont %d\n", c+1 );
          c--;
        }
        else
        {
          wprint("\a > Duplicate seed: cont %d\n", c+1 );
        }
        continue;
      }

      imodPointAppendXYZ( seedPts, seedPt->x, seedPt->x, seedPt->z );
    }
  }
  imodContourDelete(seedPts);
  
  
  //## FOR EACH CONTOUR: DELETE DUPLICATE POINTS ON SAME SLICE
  
  for( int c=0; c<csize(obj); c++)
  {
    Icont *cont = getCont( obj, c );
    
    if( bead_areDuplicatePtsSameView(cont) )
    {
      numProblems++;
      if( deleteExtraPts )
      {
        undoContourDataChg(plug.view, objIdx, c);   // REGISTER UNDO
        bead_removeDuplicatePtsSameView( cont, true, false );
        wprint("\a > Deleted duplicate points on contour %d\n", c+1 );
      }
      else
      {
        wprint("\a > Duplicate point on contour %d\n", c+1 );
      }
    }
  }
  
  if( numProblems > 1)
    wprint("\a  ... %d problems were found\n", numProblems );
  else
    wprint("  ... No problems were found  :-)\n");
}



//------------------------
//-- Toggles the current contour's stippled flag

void BeadHelper::toggleStippled()
{
  if( !isCurrContValid() )
    return;
  
  Icont *cont = getCurrCont();
  undoContourPropChgCC( plug.view );        // REGISTER UNDO
  setInterpolated( cont, isInterpolated(cont) ? 0 : 1 );
  undoFinishUnit( plug.view );              // FINISH UNDO
	plug.window->drawExtraObject(true);
}

//------------------------
//-- Selects the next/previous stippled/unstippled contour in the current object
//-- depending on the settings of: "plug.enterAction" and "plug.minPtsEnter".
//-- Returns false if no valid object is selected or no action is selected.

bool BeadHelper::enterActionIterateConts( bool reverse )
{
  if( !isCurrObjValid() || plug.enterAction == EN_NONE )
    return false;
  
  bool anyCont      = (plug.enterAction == EN_NEXTCONT)    ? true  : false;
  bool stippledCont = (plug.enterAction == EN_NEXTCHECKED) ? true  : false;
  int  change       = (plug.enterAction == EN_PREVUNCHECKED) ? -1 : 1;
  
  if(reverse)
    change = -1*change;
  
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  //## PRINT THE NUMBER OF CONTOURS MATCHING THE GIVEN CRITERIA:
  
  if( plug.enterPrint )
  {
    int matches = 0;
    for( int i=0; i<csize(obj); i++ )
    {
      Icont *cont = getCont(obj,i);
      if(      ( psize(cont) >= plug.minPtsEnter )
            && ( psize(cont) <= plug.maxPtsEnter )
            && ( anyCont || isInterpolated(cont) == stippledCont ) )
        matches++;
    }
    wprint("%d matching contours left\n", matches);
  }
  
  //## ITERATE FORWARDS OR BACKWARDS FROM CURRENT CONTOUR AND SELECT THE NEXT
  //## CONTOUR MATCHING THE ENTER CRITERIA:
  
  for( int i=0; i<csize(obj); i++ )
  {
    int c = intMod( (i+1)*change+contIdx , csize(obj) );
    
    Icont *cont = getCont(obj,c);
    
    if(      ( psize(cont) >= plug.minPtsEnter )
             && ( psize(cont) <= plug.maxPtsEnter )
             && ( anyCont || isInterpolated(cont) == stippledCont ) )
    {
      ptIdx = MIN( ptIdx, psize(cont)-1 );
      ptIdx = MAX( ptIdx, 0 );
      imodSetIndex( imod, objIdx, c, ptIdx );
      plug.window->drawExtraObject(true);
      ivwRedraw( plug.view );
      return true;
    }
  }
  
  //## IF WE GET HERE: NO MATCHES WERE FOUND SO PRINT RESULT 
  
  if( plug.enterAction == EN_NEXTUNCHECKED || plug.enterAction == EN_PREVUNCHECKED )
    wprint("\aNo more unchecked (non-stippled) contours found\n");
  else if( plug.enterAction == EN_NEXTCHECKED )
    wprint("\aNo more checked (stippled) contours found\n");
  else 
    wprint("\aNo more contours found\n");
  
  return true;
}

//------------------------
//-- Toggles the current contour's stippled flag

void BeadHelper::markRangeAsStippled()
{
  if( !isCurrObjValid() )
  {
    MsgBox( "Select a valid object first" );
    return;
  }
  
  int nConts = csize(getCurrObj());
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int checked = 1;
  
	CustomDialog ds;
  int ID_DUMMY         = ds.addLabel   ( "contours to change:" );
  int ID_CONTMIN       = ds.addSpinBox ( "min:", 1, nConts, plug.contMin, 1,
                                         "Only contours after this contour "
                                         "(inclusive) will be reordered" );
  int ID_CONTMAX       = ds.addSpinBox ( "max:", 1, nConts, nConts, 1,
                                         "Only contours BEFORE this contour "
                                         "(inclusive) will be reordered" );
  int ID_STIPPLED      = ds.addRadioGrp( "mark as:",
                                         "unchecked (unstippled),"
                                         "checked (stippled)", checked );
  int ID_DUMMY2        = ds.addLabel   ( "NOTE: Use [Enter] to iterate through\n"
                                         "unchecked (unstippled) contours and\n"
                                         "[u] to toggle current contour between\n"
                                         "checked and unchecked" );
	GuiDialogCustomizable dlg(&ds, "Mark Contours as Checked", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  int contMin         = ds.getResultSpinBox   ( ID_CONTMIN ) - 1;
  int contMax         = ds.getResultSpinBox   ( ID_CONTMAX ) - 1;
  checked            = ds.getResultRadioGrp  ( ID_STIPPLED );
  
  
  //## CHANGE CONTOURS IN RANGE TO STIPPLED / UNSTIPPLED
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int numChanged = 0;
  
  for( int c=contMin; c<=contMax && c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj,c);
    if( isInterpolated(cont) != checked )
    {
      undoContourPropChgCC( plug.view );        // REGISTER UNDO
      setInterpolated( cont, checked );
      numChanged++;
    }
  }
  if( numChanged )
    undoFinishUnit( plug.view );              // FINISH UNDO
  
  wprint("%d contours changed\n", numChanged);
	plug.window->drawExtraObject(true);
}


//------------------------
//-- Updates the range values and corrects any returns true if a 
//-- valid contour and view range has been provided.
//-- Otherwise displays error message and returns false.

bool BeadHelper::updateAndVerifyRanges()
{
  if( !isCurrObjValid() )
  {
    MsgBox("You have not selected a valid object");
    return false;
  }
  
  plug.viewMin = viewMinSpinner->value() - 1;
  plug.viewMax = viewMaxSpinner->value() - 1;
  plug.contMin = contMinSpinner->value() - 1;
  plug.contMax = contMaxSpinner->value() - 1;
  
  int maxContIdx = csize( getCurrObj() )-1;
  plug.contMax = MIN( plug.contMax, maxContIdx );
  
  if ( plug.contMin > plug.contMax  )
  {
    MsgBox("\aBad contour range");
    contMinSpinner->setValue( plug.contMax+1 );
    return false;
  }
  if ( plug.viewMin > plug.viewMax )
  {
    MsgBox("\aBad view range");
    viewMinSpinner->setValue( plug.viewMax+1 );
    return false;
  }
  
  return true;
}


//------------------------
//-- Advances the currently selected point within the contour by the specified amount.

bool BeadHelper::advanceSelectedPointInCurrCont( int change )
{
  if( !isCurrObjValid() || isEmpty( getCurrCont() ) )
    return false;
  
  Imod *imod = ivwGetModel(plug.view);
  Icont *cont = getCurrCont();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  ptIdx += change;
  keepWithinRange( ptIdx, 0, psize(cont)-1 );
  
  imodSetIndex(imod, objIdx, contIdx, ptIdx);
}


//------------------------
//-- Applies selected 'm' action... returns 1 if the action was dealt with or
//-- 0 if it wasn't.

int BeadHelper::applyMAction()
{
  if      ( plug.mKeyBehav == MK_NORMAL )
  {
    return 0;
  }
  else if( plug.mKeyBehav == MK_GOTOMIDDLE )
  {
    goToSeedView();
  }
  else
  {
    if( !isCurrObjValid() || isEmpty(getCurrCont()) )
    {
      wprint("\aSelect contour to apply smoothing\n");
    }
    else
    {
      int minZ = edit_getZOfTopZap() - 8;
      int maxZ = edit_getZOfTopZap() + 8;
      int ptsMoved, ptsAdded;
      
      Icont *cont = getCurrCont();
      undoContourDataChgCC( plug.view );       // REGISTER UNDO
      
      if( plug.mKeyBehav == MK_SMOOTHLOCAL )
      {
        bead_movePtsTowardsEstimatedPos ( cont, minZ, maxZ, 1.0, 0.2, 1,
                                          false, true,(false),  true, true, true,
                                          ptsMoved, ptsAdded );
      }
      else if( plug.mKeyBehav == MK_SMOOTHLOCALY )
      {
        bead_movePtsTowardsEstimatedPos ( cont, minZ, maxZ, 1.0, 0.2, 1,
                                          false, true,(true),  true, true, true,
                                          ptsMoved, ptsAdded );
      }
      
      if( ptsMoved || ptsAdded )                      // FINISH UNDO
        undoFinishUnit( plug.view );
      
      //## OUTPUT RESULTS:
      
      if( plug.mKeyBehav == MK_SMOOTHLOCALY )
        wprint( "Moved %d points (in Y only)\n", ptsMoved );
      else if( plug.mKeyBehav == MK_SMOOTHLOCAL )
        wprint( "Moved %d points\n", ptsMoved );
      plug.window->drawExtraObject(true);
    }
  }
  
  return 1;
}


//------------------------
//-- Jumps to the seed view and (if a contour is selected) selects
//-- the point in the middle.

void BeadHelper::goToSeedView()
{
  if( isCurrContValid() )
  {
    if( bead_isPtOnView( getCurrCont(), plug.seedView ) )
    {
      Imod *imod = ivwGetModel(plug.view);
      int objIdx, contIdx, ptIdx;
      imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
      int newPtIdx = bead_getPtIdxOnView( getCurrCont(), plug.seedView );
      imodSetIndex(imod, objIdx, contIdx, newPtIdx);
      
      plug.window->drawExtraObject(false);
      ivwRedraw( plug.view );
      return;
    }
  }
  int ix, iy, iz;
  ivwGetLocation( plug.view, &ix, &iy, &iz );
  edit_setZapLocation( ix, iy, plug.seedView, true );
  plug.window->drawExtraObject(true);
}

//------------------------
//-- Prints contour stats for the current object including: 
//-- the number of checked, non-checked and single-point contours.

void BeadHelper::printContourCheckedInfo()
{
  if( !isCurrObjValid() )
  {
    MsgBox( "Select a valid object first" );
    return;
  }
  
  Iobj *obj = getCurrObj();
  
  int totMissingPts = 0;
  int totEmptyC     = 0;
  int totSinglePtC  = 0;
  int totCheckedC   = 0;
  int totFullC      = 0;
  int totPartialC   = 0;
  
  for( int c=0; c<csize(obj); c++ )
  {
    Icont *cont   = getCont(obj,c);
    
    int numPts = psize(cont);
    
    if     ( numPts == 0 )
      totEmptyC += 1;
    else if( numPts == 1 )
      totSinglePtC += 1;
    else if( isInterpolated(cont) )
      totCheckedC += 1;
    else if( numPts == plug.zsize )
      totFullC += 1;
    else
      totPartialC += 1;
    
    totMissingPts += plug.zsize - numPts;
  }
  
  float avgMissingPtsPerCont = fDivide( totMissingPts, csize(obj) );
  int   percentSinglePt      = int( fDivide( totSinglePtC,  csize(obj)  ) * 100.0f );
  int   percentChecked       = int( fDivide( totCheckedC,   csize(obj)  ) * 100.0f );
  int   percentFull          = int( fDivide( totFullC,      csize(obj)  ) * 100.0f );
  int   percentPartial       = int( fDivide( totPartialC,   csize(obj)  ) * 100.0f );
  
  wprint("\nCONTOUR INFO:\n");
  wprint(" total contours  = %d\n", csize(obj) );
  if(totEmptyC)
    wprint("  # empty   \t= %d\n", totEmptyC );
  wprint("  # single pt \t= %d \t(%d%%)\n", totSinglePtC, percentSinglePt );
  wprint("  # partial   \t= %d \t(%d%%)\n", totPartialC,  percentPartial );
  wprint("  # full           \t= %d \t(%d%%)\n", totFullC,     percentFull  );
  wprint("  # checked   \t= %d \t(%d%%)\n", totCheckedC,  percentChecked );
  wprint(" total missing pts = %d\n", totMissingPts );
  wprint(" avg missing pts/cont = %f\n", avgMissingPtsPerCont );
}



//------------------------
//-- Checks over all non-empty contours in the first object and  if any of these are
//-- missing a point on the seed view a warning message box appears suggesting 
//-- that the seed view might be the view below the currently set value.
//-- Returns true if the seed view appears correct.

bool BeadHelper::verifySeedViewIsSeeded()
{
  if( !isCurrObjValid() )
    return false;
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  
  //## COUNT THE NUMBER OF MISSING POINTS ON THE MIDDLE SLICE AND ONE BELOW THE
  //## MIDDLE SLICE FOR ALL NON-EMPTY CONTOURS:
  
  int numNonEmptyConts = 0;
  int numMissingPtsOnSeedView = 0;
  int numMissingPtsOneBelowSeedView = 0;
  
  for( int c=0; c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj,c);
    
    if( psize(cont) >= 1 )
    {
      numNonEmptyConts++;
      if( !bead_isPtOnView(cont,plug.seedView) )
        numMissingPtsOnSeedView++;
      if( !bead_isPtOnView(cont,plug.seedView-1) )
        numMissingPtsOneBelowSeedView++;
    }
  }
  
  //## IF THERE ARE MISSING POINTS ON THE MIDDLE SLICE: PRINT ERROR MESSAGE BOX 
  
  if( numMissingPtsOnSeedView > 1 )
  {    
    if( numMissingPtsOneBelowSeedView < numMissingPtsOnSeedView )
    {
      string msg = "WARNING: \n  There are " + toString(numMissingPtsOnSeedView)
                 + " points missing from the seed view - view "
                 + toString(plug.seedView+1) + "."
                 + "\n  It appears the seed view should instead be view "
                 + toString(plug.seedView) + "."
                 + "\n  Change this value now?"
                 + "\n\n(NOTE: To manually change this click: "
                 + "'More Settings' > 'seed view')";
      
      if( MsgBoxYesNo( this, msg ) )
        plug.seedView = plug.seedView - 1;
    }
    else
    {
      string msg = "WARNING: \n  There are " + toString(numMissingPtsOnSeedView)
                 + " points missing from the seed view - view "
                 + toString(plug.seedView+1) + "."
                 + "\n\nTo manually change the seed view click: "
                 + "'More Settings'";
      
      MsgBox( msg );
    }
  }
  wprint("Middle view = view %d\n", plug.seedView+1);
  
  if( csize(obj)==0 )
    wprint("Use [h] to help you create seed points quickly");
  
  return true;
}

//------------------------
//-- Checks that the 'tilt increment' value is plausible based on the number
//-- of views and the fact tilt series usually never go beyond + or - 70 degrees.

bool BeadHelper::verifyTiltIncrement( bool printResult, bool showErrorMsgBoxIfBad )
{
  float minTilt = -plug.seedView * plug.tiltIncrement;
  float maxTilt = (plug.zsize - plug.seedView) * plug.tiltIncrement;
  
  bool tiltAppearsGood =   ( minTilt >= -80  &&  minTilt <= -10 )
                        && ( maxTilt >=  10  &&  maxTilt <=  80 );
  
  if( printResult )
  {
    string str = "Tilt increment = " + toString(plug.tiltIncrement,2) + DEGREE_SIGN + 
             + "   (" + toString(minTilt,1) + DEGREE_SIGN + " to +"
             + toString(maxTilt,1) + DEGREE_SIGN + ")\n";
    wprint( str.c_str() );
  }
  
  if( showErrorMsgBoxIfBad && !tiltAppearsGood )
  {
    string str = "WARNING: Tilt increment of " + toString(plug.tiltIncrement,2) 
               + DEGREE_SIGN + " (" + toString(minTilt,1) + " to " + toString(maxTilt,1)
               + DEGREE_SIGN + ") appears to be incorrect... \n"
               + "\n\nChange this under 'More Settings' > 'tilt increment'"
               + "\nor load the tilt angles using 'More Actions' > 'load tilt angles'";
    MsgBox( str.c_str() );
  }
  
  return (tiltAppearsGood);
}


//------------------------
//-- Opens a .tlt or .rawtlt "tilt angle file" and populates these entries into
//-- the 'plug.tiltAngles' vector. Returns true if successful.

bool BeadHelper::openTiltAngleFile()
{
  //## GET FILENAME USING OPEN DIALOG:
  
  QString qname;
  char *filter[] = {"Tilt angle files (*.rawtlt;*.tlt)",""};
  qname  = diaOpenFileName(this, "Select Tilt or Raw Tilt File", 2, filter);
  if (qname.isEmpty())
    return false;
  string fileName = qStringToString(qname);
  
  //## READ VALUES INTO A VECTOR OF STRINGS THEN PUT INTO 'plug.tiltAngles'
  
  vector<string> fileStr;  
  fileStr = file_loadTextFromFile( fileName, false );
  
  if( fileStr.empty() )
  {
    wprint( "\aERROR: Could not open file '%s'\n", fileName.c_str() );
    return false;
  }
  
  plug.tiltAngles.clear();
  for (int i=0; i<(int)fileStr.size(); i++ )
  {
    string line = string_replace( fileStr[i], " ", "");
    if( line.length() > 0 )
    {
      float tiltAngle = string_getFloatFromString(line);
      plug.tiltAngles.push_back( tiltAngle );
    }
  }
  plug.tiltAngles = vector_reverse( plug.tiltAngles );  // order of angles needs to
                                                        // to be reversed!
  
  //## PRINT RESULTS:
  
  int numTilts = (int)plug.tiltAngles.size();
  if( numTilts != plug.zsize || numTilts < 2 )
  {
    plug.tiltAngles.clear();
    wprint( "\aERROR: Number of entries did not match number of views\n");
    return false;
  }
  
  string tiltValuesStr;
  for (int i=0; i<(int)plug.tiltAngles.size(); i++ )
    tiltValuesStr += toString( plug.tiltAngles[i] );
  
  float minAngle = plug.tiltAngles[0];
  float maxAngle = plug.tiltAngles.back();
  float tiltIncrementApprox = (maxAngle - minAngle) / numTilts;
  
  wprint("TILT ANGLE FILE: \n\n");
  wprint(" view \t angle\n");
  for (int i=0; i<(int)plug.tiltAngles.size(); i++ )
  {
    wprint( " %d \t %f", i+1, plug.tiltAngles[i] );
    if( i>0 )
    {
      float tiltIncrease = plug.tiltAngles[i] - plug.tiltAngles[i-1];
      if( tiltIncrease < tiltIncrementApprox/2.0f )
        wprint("  <-- PROBLEM?");
    }
    wprint("\n");
  }
  
  wprint("\n > average tilt increment = %f%c\n", tiltIncrementApprox, DEGREE_SIGN);
  wprint(" > %d calculated tilt angles have been loaded\n", numTilts );
}

//------------------------
//-- Method used for testing new routines.

void BeadHelper::test()
{
  /*
  Icont *cont = getCurrCont();
  
  if( !isContValid(cont) )
  {
    wprint("Have not selected valid contour\n");
    return;
  }
  
  int topView = edit_getZOfTopZap();
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  float z = (getPt(cont,ptIdx)->z);
  
  Icont *ptsByDist = imodContourNew();
  bead_getSpacedOutPoints( cont, z, ptsByDist, 5 );
  imodObjectAddContour( obj, ptsByDist );
  free(ptsByDist);
  
  ivwRedraw( plug.view );
  */
}



//## BASIC METHODS TO CHANGE PLUG DATA:



//------------------------
//-- Change changeShowExpectedPos

void BeadHelper::changeShowExpectedPos() {
  plug.showExpectedPos = showEstimatedPosCheckbox->isChecked();
  drawExtraObject(true);
}



//------------------------
//-- Change showSpheres

void BeadHelper::changeShowSpheres() {
  plug.showSpheres = showSpheresCheckbox->isChecked();
  
  if( plug.showSpheres )
    imodObjectSetValue( getCurrObj(), IobjPointSize, plug.sphereSize );
  else
    imodObjectSetValue( getCurrObj(), IobjPointSize, 0 );
  
  drawExtraObject(true);
}

//------------------------
//-- Change sphereSize

void BeadHelper::changeSphereSize( int value ) {
  plug.sphereSize = value; 
  changeShowSpheres();
}

//------------------------
//-- Change lineDisplayType

void BeadHelper::changeLineDisplayType(int value) {
  plug.lineDisplayType = value;
  drawExtraObject(true);
}

//------------------------
//-- Change lineDisplayType

void BeadHelper::changeTiltDisplayType(int value) {
  plug.tiltDisplayType = value;
  drawExtraObject(true);
}

//------------------------
//-- Change wheelBehav

void BeadHelper::changeWheelBehav(int value) {
  plug.wheelBehav = value;
}

//------------------------
//-- Change wheelBehav

void BeadHelper::changeEstPosMethod(int value) {
  plug.estPosMethod = value;
  drawExtraObject(true);
}



//## PROTECTED:


//------------------------
//-- Called to display help window.

void BeadHelper::buttonPressed(int which)
{
  if (!which)
    close();
  else
  {
    QString str = QString(getenv("IMOD_DIR"));
    str += QString("/lib/imodplug/beadhelper.html");
    
    imodShowHelpPage(str);
  }
}

//------------------------
//-- Window closing event handler - removes this pluging from the imod dialog manager

void BeadHelper::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)plug.window);
  clearExtraObj();
  ivwFreeExtraObject(plug.view, plug.extraObjExpPos);
  ivwFreeExtraObject(plug.view, plug.extraObjContDisp);
  ivwFreeExtraObject(plug.view, plug.extraObjTiltAxis);
  ivwFreeExtraObject(plug.view, plug.extraObjExtra);
  ivwTrackMouseForPlugs(plug.view, 0);
  ivwEnableStipple( plug.view, 0 );
  
  if( plug.autoSaveSettings )
    plug.window->saveSettings();
  
  ivwDraw( plug.view, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC );
  
  plug.view = NULL;
  plug.window = NULL;
  e->accept();
}


//------------------------
//-- Key press event handler - closes on escape or passes on event to "ivwControlKey"

void BeadHelper::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

//------------------------
//-- Key release event hander - passes on event to "ivwControlKey"

void BeadHelper::keyReleaseEvent ( QKeyEvent * e )
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
//          EDITING FUNCTIONS:
//
//----------------------------------------------------------------------------




//------------------------
//-- Gets the view value of the top Zap window or returns -1 if no Zap

int edit_getZOfTopZap()
{
  int currView = -1;
  int noZap = ivwGetTopZapZslice(plug.view, &currView);   // gets current slice
  if (noZap == 1)   // if no top ZAP window:
    return (-1);
  return (currView);
}


//------------------------
//-- Sets the top ZAP window to focus on the selected point and view.

int edit_setZapLocation( float x, int y, int z, bool redraw )
{
  ivwSetLocation( plug.view, x, y, z );
  if( redraw )
    ivwDraw( plug.view, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC );
}


//------------------------
//-- Changes the Z view by calling page up or page down

int edit_changeSelectedView( int changeZ, bool redraw )
{
  int ix, iy, iz;
  ivwGetLocation( plug.view, &ix, &iy, &iz );
  edit_setZapLocation( ix, iy, iz+changeZ, redraw );
}



//------------------------
//-- Sets the top ZAP window to focus on the selected point.
//-- Note that the method to do this is quite crude, because it
//-- involves the creationg and then delestion of a point,
//-- since "ivwSetLocation" alone is not enough to shift the view.

bool bead_focusOnPointCrude( float x, float y, float z )
{
  if( !isCurrObjValid() )
    return false;
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  Icont *tempCont = imodContourNew();
  imodPointAppendXYZ( tempCont, x,y,z );
  int tempContIdx = imodObjectAddContour( obj,tempCont );
  imodSetIndex(imod, objIdx, tempContIdx, 0);
  imodObjectRemoveContour( obj, tempContIdx );
  ivwSetLocation(plug.view, x, y, z);
  ivwRedraw( plug.view );
  imodContourDelete(tempCont);
  //free(tempCont);             // already freed by 'imodObjectRemoveContour'
}


//------------------------
//-- Gets the tilt angle of a view from 'plug.tiltAngles' or, if this is empty,
//-- calculates it using 'plug.tiltIncrement' and 'plug.seedView'

float bead_getTiltAngleAtZ( int z )
{
  if( z < (int)plug.tiltAngles.size() )
    return plug.tiltAngles[z];
  
  else
    return ( z - plug.seedView ) * plug.tiltIncrement;
}


//------------------------
//-- Returns true if there two or more consecutive points in the contour are
//-- on the same view.

bool bead_areDuplicatePtsSameView( Icont *cont )
{
  for(int p=1; p<psize(cont); p++)
    if( getPtZInt(cont,p) == getPtZInt(cont,p-1)  )
      return true;
  return false;
}


//------------------------
//-- Removes any duplicate points on the same view within the contour
//-- and returns the number of points deleted.
//-- WARNING: does not provide undo option.

int bead_removeDuplicatePtsSameView( Icont *cont, bool remove, bool print )
{
  int pointsRemoved = 0;
  
  for(int p=1; p<psize(cont); p++)
  {
    if( getPtZInt(cont,p) == getPtZInt(cont,p-1)  )
    {
      if( remove )
      {
        imodPointDelete( cont, p );         // delete point
        p--;
        pointsRemoved++;
        
        if(print)
          wprint("Removed duplicate point\n");
      }
      else if(print)
      {
          wprint("Duplicate point found on view: %d\n", getPtZInt(cont,p) );
      }
    }
  }
}



//------------------------
//-- Returns true if there two or more points in the contour which
//-- are out of order (not in ascending Z value).

bool bead_ptsAreNotInOrder( Icont *cont )
{
  for(int p=1; p<psize(cont); p++)
    if( getPtZInt(cont,p) < getPtZInt(cont,p-1)  )
      return true;
  return false;
}

//------------------------
//-- Arranges points in order of ascending Z value and returns the number
//-- of points reordered.

int bead_orderPtsByAscendingZ( Icont *cont )
{
  if( psize(cont) <= 1 )
    return 0;
  
  //## MAKE VECTOR CONTAINING THE Z VALUE OF EACH POINT AND SORT IT IN ASCENDING ORDER:
  
  vector<IdxToSort> zVals;
  for(int i=0; i<psize(cont); i++ )
    zVals.push_back( IdxToSort( i, getPt(cont,i)->z ) );
  zVals = vector_sort( zVals );
  
  
  //## USE THE SORTED VECTOR OF POINT INDEXES TO REORDER THE POINT IN THE CONTOUR
  
  Icont *copy = imodContourDup(cont);
  int numPtsReordered = 0;
  
  for(int i=0; i<psize(copy) && i<(int)zVals.size(); i++)
  {
    if( zVals[i].idx != i )
    {
      Ipoint *pt = getPt(copy,zVals[i].idx);
      getPt(cont,i)->x = pt->x;
      getPt(cont,i)->y = pt->y;
      getPt(cont,i)->z = pt->z;
      numPtsReordered++;
    }
  }
  
  imodContourDelete( copy );
  
  return (numPtsReordered);
}


//------------------------
//-- Returns true if there is a point in "cont" on the specified view.

bool bead_isPtOnView( Icont *cont, int view )
{
  for( int p=0; p<psize(cont); p++ )
    if( getPtZInt(cont,p) == view )
      return true;
  return false;
}

//------------------------
//-- Returns the index of the first point in "cont" on the specified view or
//-- returns -1 if there is no point on that contour.

int bead_getPtIdxOnView( Icont *cont, int view )
{
  for( int p=0; p<psize(cont); p++ )
    if( getPtZInt(cont,p) == view )
      return p;
  return (NO_POINT);
}

//------------------------
//-- Returns pointer to the first point in "cont" on the specified view
//-- or returns NULL if there is no point on that contour.

Ipoint *bead_getPtOnView( Icont *cont, int view )
{
  for( int p=0; p<psize(cont); p++ )
    if( getPtZInt(cont,p) == view )
      return getPt(cont,p);
  return NULL;
}


//------------------------
//-- Returns the index within the contour where you would insert
//-- a point on the specified view (i.e. the first index 
//-- where z >= "view" ).

int bead_getExpPtIdxForView( Icont *cont, int view )
{
  for( int p=0; p<psize(cont); p++ )
    if( getPtZInt(cont,p) >= view )
      return (p);
  return psize(cont)-1;
}

//------------------------
//-- Returns the index to the point which is closest to current view
//-- or returns -1 if no points

int bead_getClosestPtIdxToView( Icont *cont, int view )
{
  if( isEmpty(cont) )
    return (NO_POINT);
  
  int closestDistInZ = INT_MAX;
  int closestIdx = 0;
  
  for( int p=0; p<psize(cont); p++ )
  { 
    int distInZ = ABS( getPtZInt(cont,p) - view );
    if (distInZ < closestDistInZ)
    {
      closestDistInZ = distInZ;
      closestIdx = p;
    }
  }
  return closestIdx;
}


//------------------------
//-- Returns the point which is closest to current view
//-- or returns NULL if no points

Ipoint *bead_getClosestPtToView( Icont *cont, int view )
{
  int closestPtIdx = bead_getClosestPtIdxToView( cont, view );
  if (closestPtIdx == NO_POINT)
    return NULL;
  return getPt(cont, closestPtIdx);
}


//------------------------
//-- Returns the two points closest to the given view, but not actually on the view.
//-- Returns false if there are not enough points.

bool bead_getClosestTwoPointsToView( Icont *cont, int view, Ipoint *pt1, Ipoint *pt2 )
{
  if( !cont || psize(cont) < 3 )
    return false;
  
  int closestDistInZ1 = INT_MAX;
  int closestDistInZ2 = INT_MAX;
  
  for( int p=0; p<psize(cont); p++ )
  {
    int distInZ = ABS( getPtZInt(cont,p) - view );
    
    if (distInZ == 0)
    {
      continue;
    }
    else if( distInZ <= closestDistInZ1 )
    {
      closestDistInZ2 = closestDistInZ1;
      closestDistInZ1 = distInZ;
      *pt2 = *pt1;
      *pt1 = *getPt(cont,p);
    }
    else if( distInZ <= closestDistInZ2 )
    {
      closestDistInZ2 = distInZ;
      *pt2 = *getPt(cont,p);
    }
  }
  return true;
}

//------------------------
//-- Returns the two closest points above and below the given view,
//-- (but not actually on the view), or returns the two closest points
//-- if there are not points either side.

bool bead_getPointsEitherSideView( Icont *cont, int view, Ipoint *pt1, Ipoint *pt2 )
{
  if( !cont || psize(cont) < 3 )
    return false;
  
  bool pointBelowFound = false;
  bool pointAboveFound = false;
  
  for( int p=0; p<psize(cont); p++ )
  {
    int z = getPtZInt(cont,p);
    
    if ( z < view )
    {
      *pt1 = *getPt(cont,p);
      pointBelowFound = true;
    }
    else if( z > view )
    {
      *pt2 = *getPt(cont,p);
      pointAboveFound = true;
      break;
    }
  }
  
  if( pointBelowFound && pointAboveFound )
    return true;
  
  return bead_getClosestTwoPointsToView(cont,view,pt1,pt2);
}

//------------------------
//-- Returns contour "ptsByDist" containing the points above (if "above") or below
//-- the current view in ascending order of distance from the selected point.
//-- If "minZBetweenPts" is greater than 1
//-- Returns false if there are not enough points.

bool bead_getPointsOneSideOfView( Icont *cont, int view, Icont *ptsByDist,
                                   bool above, bool includeView, int minZBetweenPts )
{
  imodContourDefault( ptsByDist );
  
  if( !isContValid(cont) )
    return false;
  
  //## POPULATE POINTS ABOVE OR BELOW THE SLICE:
  
  bool addedPtOnView = false;
  
  for( int p=0; p<psize(cont); p++ )
  {
    Ipoint *pt = getPt(cont,p);
    float x = pt->x;
    float y = pt->y;
    float z = int(pt->z + 0.5);
    
    if( includeView && z==view)
    {
      imodPointAppendXYZ( ptsByDist, x,y,z );
      addedPtOnView = true;
      continue;
    }
    
    if(above)
    {
      if( z > view )
        imodPointAppendXYZ( ptsByDist, x,y,z );
    }
    else
    {
      if( z < view )
        imodPointAppendXYZ( ptsByDist, x,y,z );
    }
  }
  
  //## ORDERED POINTS BY DISTANCE FROM SLICE 
  if( !above )
    imodel_contour_invert( ptsByDist );    // inverts order of points in the points below
                                          // so they are in order of distance from view
  
  
  if( minZBetweenPts<=1 )
    return true;
  
  
  //## DELETE POINTS WITHIN THE LIST SUCH THAT REMAINING POINTS ARE SPACED AT
  //## LEAST "minZBetweenPts" APART
  
  int i = (addedPtOnView) ? 1 : 0;
  
  if (above)
  {
    int nextZA = view + minZBetweenPts;
    for(; i<psize(ptsByDist); i++)
    {
      if( getPtZInt(ptsByDist,i) < nextZA )
      {
        imodPointDelete(ptsByDist,i);
        i--;
      }
      else
        nextZA = getPtZInt(ptsByDist,i) + minZBetweenPts;
    }
  }
  else
  {
    int nextZB = view - minZBetweenPts;
    for(; i<psize(ptsByDist); i++)
    {
      if( getPtZInt(ptsByDist,i) > nextZB )
      {
        imodPointDelete(ptsByDist,i);
        i--;
      }
      else
        nextZB = getPtZInt(ptsByDist,i) - minZBetweenPts;
    }
  }
  
  return true;
}



//------------------------
//-- Returns a contour "ptsByDist" containing points within "cont"
//-- in ascending distance from the given view, where points are spaced
//-- at least "minZBetweenPts" of each other but do not actually include
//-- any points on "view".
//-- Returns false if "cont" is invalid.

bool bead_getSpacedOutPoints( Icont *cont, int view,
                              Icont *ptsByDist, int minZBetweenPts )
{
  imodContourDefault( ptsByDist );
  
  if( !isContValid(cont) )
    return false;
  
  //## GET SEPERATE LISTS OF POINTS ABOVE AND BELOW THE SLICE:
  
  Icont *ptsAbove = imodContourNew();
  Icont *ptsBelow = imodContourNew();
  
  bead_getPointsOneSideOfView( cont, view, ptsAbove, true,  false, minZBetweenPts );
  bead_getPointsOneSideOfView( cont, view, ptsBelow, false, false, minZBetweenPts );
  
  //## COMBINE LISTS SUCH THAT POINTS ARE IN ORDER OF DISTANCE FROM THE SLICE:
  
  float mid = plug.seedView;       // used as tie breaker
  
  int a=0;
  int b=0;
  while( b<psize(ptsBelow) && a<psize(ptsAbove) ) 
  {
    Ipoint *ptA = getPt(ptsAbove, a);
    Ipoint *ptB = getPt(ptsBelow, b);
    
    float ptADist = ptA->z - view;
    float ptBDist = ptB->z - view;
    
    bool ptAIsAbove = ptADist < ptBDist
                      || (ptADist == ptBDist && ABS(ptA->z-mid) < (ptB->z-mid) );
    
    if( ptAIsAbove )
    {
      imodPointAppend( ptsByDist, ptA );
      a++;
    }
    else
    {
      imodPointAppend( ptsByDist, ptB );
      b++;
    }
  }
  for(;a<psize(ptsAbove);a++)
    imodPointAppend( ptsByDist, getPt(ptsAbove, a) );
  for(;b<psize(ptsBelow);b++)
    imodPointAppend( ptsByDist, getPt(ptsBelow, b) );
  
  imodContourDelete( ptsAbove );
  imodContourDelete( ptsBelow );
  
  return true;
}


//------------------------
//-- Takes two points as input and calculates the estimated position of a point on
//-- view "z" if a straight line was drawn through them or returns the the first
//-- point if both points are on the same view.

Ipoint bead_getPtOnLineWithZ( Ipoint *pt1, Ipoint *pt2, int z )
{
  if( (int)pt1->z == (int)pt2->z )
    return *pt1;
  
  Ipoint estPt;
  estPt.z = z;
  float fractToExpectedPos = fDivide( (z - pt1->z) , ( pt2->z - pt1->z) ); 
  estPt = line_findPtFractBetweenPts( pt1, pt2, fractToExpectedPos );
  return (estPt);
}

//------------------------
//-- Takes two points as input and calculates the estimated position of a point on
//-- view "z" based on tilt angles and a straight line thorugh the points

Ipoint bead_getPtOnLineWithZUsingTiltAngles( Ipoint *pt1, Ipoint *pt2, int z )
{
  if( (int)pt1->z == (int)pt2->z )
    return *pt1;
  
  Ipoint estPt;
  estPt.z = z;
  
  //float xDistBetweenPtsNorm = ABS(fDivide( (pt2->x - pt1->x) , ( pt2->z - pt1->z) )); 
  
  float angle1 = bead_getTiltAngleAtZ( pt1->z+0.5 );
  float angle2 = bead_getTiltAngleAtZ( pt2->z+0.5 );
  float angleZ = bead_getTiltAngleAtZ( z );
  
  float adjSide1  = cosf( DEGS_TO_RADS * angle1 );
  float adjSide2  = cosf( DEGS_TO_RADS * angle2 );
  float adjSideZ  = cosf( DEGS_TO_RADS * angleZ );
  // calculate the relative length of an adjacent side for each point
  // implied by the tilt angle at that Z value
  
  float fractToExpectedPos = fDivide((adjSideZ - adjSide1), ( adjSide2 - adjSide1)); 
  if( (adjSide2 == adjSide1) ||     // if length of both adjacent sides is equal
      (angle1<0 && angle2>0) ||     //  or one angle is positive and
      (angle1>0 && angle2<0) )      //  the other is negeative:
    fractToExpectedPos = fDivide( (z - pt1->z) , ( pt2->z - pt1->z) );
  
  estPt = line_findPtFractBetweenPts( pt1, pt2, fractToExpectedPos );
  
  return (estPt);
}



//------------------------
//-- Outputs the estimated position of the point within "cont" on the specified
//-- view based on the position of the points before and after this view.
//-- Returns true if successful or false if contour is invalid or there are not
//-- enough points to make a reliable prediction

bool bead_getExpectedPosOfPoint( Icont *cont, int view, Ipoint *pt )
{
  if( !cont || psize(cont) < 3 )
    return false;
  
  
  Ipoint estPt;
  float z = view;
  estPt.z = z;
  
  
  switch( plug.estPosMethod )
  {
    case (EM_BESTTWO):
    {
      Ipoint pt1, pt2;
      if ( !bead_getPointsEitherSideView( cont, view, &pt1, &pt2 ) )
        return false;
      estPt = bead_getPtOnLineWithZ( &pt1, &pt2, view );
      break;
    }
    
    case (EM_SMARTTWO):
    {
      Ipoint pt1, pt2;
      if ( !bead_getPointsEitherSideView( cont, view, &pt1, &pt2 ) )
        return false;
      estPt = bead_getPtOnLineWithZUsingTiltAngles( &pt1, &pt2, view );
      break;
    }
    
    case (EM_NEARESTTWO):
    {
      Ipoint pt1, pt2;
      if ( !bead_getClosestTwoPointsToView( cont, view, &pt1, &pt2 ) )
        return false;
      estPt = bead_getPtOnLineWithZ( &pt1, &pt2, view );
      break;
    }
    
    case (EM_PREVTWO):
    {
      bool aboveMiddle = ( view > plug.seedView );
      Icont *ptsByDist = imodContourNew();
      bool success = bead_getPointsOneSideOfView( cont, view, ptsByDist,
                                              !aboveMiddle, false, 1 );
      if( !success || psize(ptsByDist) < 2 ) 
      {
        imodContourDelete(ptsByDist);
        return false;
      }
      estPt = bead_getPtOnLineWithZ( getPt(ptsByDist,1), getPt(ptsByDist,0), view );
      imodContourDelete(ptsByDist);
      break;
    }
    
    case (EM_QUADRATIC):
    {
      Ipoint *p1 = getPt( cont, 0 );
      Ipoint *p2 = getPt( cont, psize(cont) / 2 );
      Ipoint *p3 = getPt( cont, psize(cont)-1 );
      
      Ipoint *seedPt = bead_getClosestPtToView( cont, plug.seedView );
      if ( (seedPt->z == plug.seedView) && (psize(cont) > 20)
          && (seedPt->z > p1->z+10) && (seedPt->z < p3->z-10) )
      {
        p2 = seedPt;
      }
      
          // calculate the quadratic function representing the bend along x
          // relative to z (usually this is greatest):
      
      float aX,bX,cX;
      bead_calcQuadraticCurve( p1->z, p2->z, p3->z, p1->x, p2->x, p3->x, &aX, &bX, &cX );         //     |_____ z
      estPt.x = aX*(z*z) + bX*z + cX;
      
          // calculate the quadratic function representing the bend along y
          // relative to z - this is usually small because usually the
          // tilt axis is only 12 degrees off the Y axis):
      
      float aY,bY,cY;
      bead_calcQuadraticCurve( p1->z, p2->z, p3->z, p1->y, p2->y, p3->y, &aY, &bY, &cY );         //     |_____ z
      estPt.y = aY*(z*z) + bY*z + cY;
      
      break;
    }
    
    case (EM_LOCALQUADRATIC):
    {
      Icont *ptsByDist = imodContourNew();
      bead_getSpacedOutPoints( cont, z, ptsByDist, 10 );
      
      if( psize(ptsByDist) < 3 )
        return (false);
      
      Ipoint *p1 = bead_getClosestPtToView( cont, view );
      Ipoint *p2 = getPt( ptsByDist, 0 );
      Ipoint *p3 = getPt( ptsByDist, 1 );
      
      if( p2->z == p1->z)
        p2 = getPt( ptsByDist, 2 );
      
          // calculate the quadratic function representing the bend along x
          // relative to z (usually this is greatest):
      
      float aX,bX,cX;
      bead_calcQuadraticCurve( p1->z, p2->z, p3->z, p1->x, p2->x, p3->x, &aX, &bX, &cX );         //     |_____ z
      estPt.x = aX*(z*z) + bX*z + cX;
      
          // calculate the quadratic function representing the bend along y
          // relative to z - this is usually small because usually the
          // tilt axis is only 12 degrees off the Y axis):
      
      float aY,bY,cY;
      bead_calcQuadraticCurve( p1->z, p2->z, p3->z, p1->y, p2->y, p3->y, &aY, &bY, &cY );         //     |_____ z
      estPt.y = aY*(z*z) + bY*z + cY;
      
      break;
    }
    
    case (EM_LASTTHREE):
    {
      if ( !bead_getExpectedPosOfPointUsingPtsBefore( cont, view, &estPt, 3 ) )
        return false;
      break;
    }
    
    case (EM_LASTSIX):
    {
      if ( !bead_getExpectedPosOfPointUsingPtsBefore( cont, view, &estPt, 6 ) )
        return false;
      break;
    }
  }
  
  *pt = estPt;
  return true;
}



//------------------------
//-- Calculates the estimated position fo the point wihin on on the specified
//-- view based on the gradient of the last "numPtsToAvg" points before
//-- it in the direction of the middle of the contour.


bool bead_getExpectedPosOfPointUsingPtsBefore( Icont *cont, int view, Ipoint *pt,
                                          int numPtsToAvg )
{
  float z = view;
  
  if( psize(cont) < (2*numPtsToAvg) )
    return (false);
  
  //## DETERMINE IF POINT IS ON SLICE AND WHICH DIRECTION (UP OR DOWN)
  //## IS TOWARDS MIDDLE OF CONTOUR
  
  int midPtIdx  = psize(cont) / 2;
  int ptIdx     = bead_getExpPtIdxForView( cont, view );
  bool searchUp = ptIdx < midPtIdx;
  bool isOnPt   = getPtZInt(cont,ptIdx) == view;
  
  int directionAsc = (searchUp) ? 1 : -1;   // direction from pt1 to p4
  int offset       = (isOnPt)   ? 1 : 0;
  
  int firstIdx = ptIdx + offset*directionAsc;
  
  
  //## SET UP NEW CONTOUR TO CONTAIN "NUMPTS" BEFORE THE CURRENT SLICE
  
  Icont *prevPts = imodContourNew();
  for( int i=0; i<numPtsToAvg; i++ )
  {
    Ipoint *pt = getPt( cont, firstIdx+i*directionAsc );
    imodPointAppendXYZ( prevPts, pt->x, pt->y, pt->z);
  }
  
  //## IF ONLY TWO POINTS FOUND THEN CALCULATE BASED ON LINEAR PROJECTION
  
  if( psize(prevPts) == 2 )
  {
    *pt = bead_getPtOnLineWithZ( getPt(prevPts,0), getPt(prevPts,1), view );
    imodContourDelete( prevPts );
    return true;
  }
  
  //## CACULATE THE GRADIENT BASED ON THESE POINTS:
  
  float gradient, offsetY;
  bool success = bead_calcLineOfBestFit( prevPts, &gradient, &offsetY, 2 );
  if(!success)
  {
    imodContourDelete( prevPts );
    return (false);
  }
  
  
  //## DETERMINE THE DISTANCE BETWEEN EACH PAIR OF POINTS:
  
  vector<float> distXNorm;          // stores the distances between each point and
                                    // the next point divided by their z difference
  
  for( int i=0; i<psize(prevPts)-1; i++ )
  {
    Ipoint *pt     = getPt( prevPts, i );
    Ipoint *ptNext = getPt( prevPts, i+1 );
    distXNorm.push_back( fDivide((pt->x - ptNext->x), ( pt->z - ptNext->z )) );
  }
  
  //## DETERMINE THE AVERAGE RATE AT WHICH THE DISTANCE IS INCREASING/DECREASING:
  
  float totalIncreaseXNorm = 0;
  for( int i=0; i<distXNorm.size()-1; i++ )
  {
    float increaseXFromLastNorm = ( distXNorm[i] - distXNorm[i+1] );
    totalIncreaseXNorm += increaseXFromLastNorm;
  }
  float avgIncreaseXNorm = fDivide( totalIncreaseXNorm, distXNorm.size()-1 );
  
  if( ABS(2*avgIncreaseXNorm) > ABS(distXNorm[0]) )   // if increase is > first x dist:
    avgIncreaseXNorm = 0;                               // don't use increase
                                                        // (we are probably at 
                                                        // a turning point)
  
  //## USE THE AVERAGE GRADIENT AND RATE OF DISTANCE INCREASE TO
  //## CALCULATE THE EXPECTED POINT'S POSITION:
  
  float distZ = (view - getPt(prevPts,0)->z);
  pt->x = -directionAsc*avgIncreaseXNorm*(distZ*distZ) +
    distXNorm[0]*(distZ) + getPt(prevPts,0)->x;
  pt->y = (pt->x*gradient) + offsetY;
  pt->z = view;
  
  imodContourDelete( prevPts );
}



//------------------------
//-- Inserts a point into the approprite place in the contour (in ascending z order)
//-- and returns its index.
//-- If there is already a point at the given z value it is overwritten.

int bead_insertOrOverwritePoint( Icont *cont, Ipoint *pt )
{
  for( int p=0; p<psize(cont); p++ )
  {
    if ( getPtZInt(cont,p) == (int)pt->z ) {
      *getPt(cont,p) = *pt;
      return p;
    }
    else if( getPtZInt(cont,p) > (int)pt->z ) {
      //Ipoint *newPt = *pt;
      imodPointAdd(cont,pt,p);
      return p;
    }
  }
  imodPointAppend(cont,pt);
  return (psize(cont)-1);
}


//------------------------
//-- Inserts a point on the given view into the estimated position
//-- within the given contour. If a point already exists at that view
//-- it is overwritten is "overwrite" is true.
//-- Returns true if a point was inserted or overwritten.

bool bead_insertPtAtEstimatedPos( Icont *cont, int view, bool overwrite )
{
  if( !cont || psize(cont) < 3 )
    return false;
  
  if( !overwrite && bead_isPtOnView(cont,view) )
    return false;
  
  Ipoint estPt;
  if( bead_getExpectedPosOfPoint( cont, view, &estPt ) )
  {
    bead_insertOrOverwritePoint( cont, &estPt );
    return true;
  }
  
  return false;
}



//------------------------
//-- If there is a point on the given "z" of "cont" which is greater than 
//-- "minResid" away from it's estimated position the point will be moved "moveFract"
//-- of the way towards their estimated postion. Returns 1 if the point was moved or,
//-- 0 if the point was not moved or there is no point on this view.
//-- Also has the addition options to "fillGaps" or "moveYOnly"... and to leave
//-- alone the point if it is on the seed view ("leaveSeed") or is on one of the ends
//-- ("leaveEnds")

int bead_movePtTowardsEstimatedPos ( Icont *cont, int z,
                                     float moveFract, float minResid, bool moveYOnly,
                                     bool leaveEnds )
{
  if( psize(cont) < 3 )
    return false;
  
  if( leaveEnds && ( z <= getFirstPt(cont)->z || z >= getLastPt(cont)->z ) )
      return 0;
  
  Ipoint expPt;
  Ipoint *currPt = bead_getPtOnView( cont, z );
  
  if( currPt != NULL
      && bead_getExpectedPosOfPoint( cont, z, &expPt )
      && line_distBetweenPts2D( currPt, &expPt ) >= minResid )
  {
    expPt = line_findPtFractBetweenPts2D( currPt, &expPt, moveFract );
    if( !moveYOnly )
      currPt->x = expPt.x;
    currPt->y = expPt.y;
    return 1;
  }
  return 0;
}


//------------------------
//-- Moves all points in the contour within the z range "minZ" to "maxZ"
//-- that are more than "minResid" away from their estimated position "moveFract"
//-- of the way towards their estimated postion. Returns true if successful,
//-- as well as the number of points added and moved.

bool bead_movePtsTowardsEstimatedPos ( Icont *cont, int minZ, int maxZ,
                                       float moveFract, float minResid, int iterations,
                                       bool fillGaps, bool moveBigFirst, bool moveYOnly,
                                       bool leaveSeed, bool leaveEnds, bool leaveCurrV,
                                       int &ptsMoved, int &ptsAdded )
{  
  ptsMoved = 0;
  ptsAdded = 0;
  
  if( minZ > maxZ || isEmpty(cont) ||  psize(cont) < 3 )
    return false;
  
  //## SETUP VECTOR OF Z VALUES FOR POINTS TO MOVE:

  int zmiddle = (minZ + maxZ) / 2;
  int zLeave1 = (leaveSeed)  ? plug.seedView    : -1;
  int zLeave2 = (leaveCurrV) ? edit_getZOfTopZap() : -1;
  
  vector<IdxToSort> zVals;
    
  for( int z=minZ; z<=maxZ; z++ )
    if( z!= zLeave1 && z!=zLeave2 )
      zVals.push_back( IdxToSort(z, ABS(z-zmiddle) )  );
  
  //## SORT VECTOR OF Z VALUES:
  
  if( moveBigFirst )    // if we want to move the biggest residuals first:
  {                         // calculate the distance from expected for each point:
    for( int i=0; i<(int)zVals.size(); i++ )
      zVals[i].float1 = -1 * bead_calcDistanceFromExpected(cont, (zVals[i].idx), false);
  }
  zVals = vector_sort( zVals );
  
  //## MOVE POINTS ON EACH Z VALUE IN THE SORTED VECTOR:
  
  for( int i=0; i<(int)zVals.size(); i++ )
    ptsMoved += bead_movePtTowardsEstimatedPos( cont, (zVals[i].idx), moveFract, minResid,
                                                moveYOnly, leaveEnds );
  
  if( fillGaps )
    ptsAdded = bead_fillMissingPtsOnCont( cont, minZ, maxZ, false );
  
  return true;
}


//------------------------
//-- Moves points in "cont" within the z range "minZ" to "maxZ"
//-- using the smoothing settings in "plug". Returns true if successful,
//-- as well as the number of points added and moved.

bool bead_smoothPtsUsingPlugSettings( Icont *cont, int &ptsMoved, int &ptsAdded )
{
  int minZ = plug.viewMin;
  int maxZ = plug.viewMax;
  
  if( plug.smoothAdjacentV && edit_getZOfTopZap()>0 )
  {
    minZ = edit_getZOfTopZap() - plug.smoothNumViews;
    maxZ = edit_getZOfTopZap() + plug.smoothNumViews;
  }
  
  
  return bead_movePtsTowardsEstimatedPos ( cont, minZ, maxZ,
                                           plug.smoothMoveFract,
                                           plug.smoothMinResid, plug.smoothIterations,
                                           plug.smoothFillGaps, plug.smoothBigResidFirst,
                                           plug.smoothMoveYOnly, plug.smoothLeaveSeed,
                                           plug.smoothLeaveEnds, plug.smoothLeaveCurrV,
                                           ptsMoved, ptsAdded );
}



//------------------------
//-- Fills in all missing points in current contour within views between
//-- "minZ" and "maxZ" inclusive, by starting at the middle point and
//-- using the estimated middle position. Returns the number of points added.

int bead_fillMissingPtsOnCont( Icont *cont, int minZ, int maxZ, bool fillPastEnds )
{
  if( !cont || psize(cont) < 3 || minZ > maxZ )
    return 0;
  
  if( !fillPastEnds )
  {
    minZ = MAX( minZ, int(getFirstPt(cont)->z + 1) );
    maxZ = MIN( maxZ, int(getLastPt (cont)->z - 1) );
  }
  
  int middleZ = avg(maxZ,minZ);
  int pointsAdded = 0;
  
  for( int z=middleZ; z<=maxZ; z++ )
  {
    if( bead_insertPtAtEstimatedPos(cont,z,false) )
      pointsAdded++;
  }
  for( int z=middleZ-1; z>=minZ; z-- )
  {
    if( bead_insertPtAtEstimatedPos(cont,z,false) )
      pointsAdded++;
  }
  
  return pointsAdded;
}



//------------------------
//-- Deletes all points within the specified range of z values and returns
//-- the number of points deleted.

int bead_deletePtsInZRange( Icont *cont, int minZ, int maxZ, bool inclusive )
{
  //## CHECK RANGE IS GOOD:
  
  if(!inclusive)
  {
    minZ++;
    maxZ--;
  }
  if( isEmpty(cont) || minZ >= maxZ )
    return 0;
  
  //## DELETE POINTS IN RANGE:
  
  int numPtsDeleted = 0;
  for( int p=psize(cont)-1; p>=0; p-- )
  {
    int z = getPtZInt(cont,p);
    if( z >= minZ && z <= maxZ )
    {
      imodPointDelete( cont, p );
      numPtsDeleted++;
    }
  }
  
  return numPtsDeleted;
}


//------------------------
//-- Calculates how far in Y the point at index "idx" in the given contour
//-- is from it's estimated location mid-way between the next and previous points.

float bead_calcYJump( Icont *cont, int idx )
{
  if( !cont || psize(cont) < 3 || idx < 0 || idx >= psize(cont) )
    return 0;
  
  int z = getPtZInt(cont,idx);
  Ipoint expPos;                      // estimated position of our point at "z"
  
  if(idx == 0)                        // if first point: compare to two points after
  {
    expPos = bead_getPtOnLineWithZ( getPt(cont,idx+1), getPt(cont,idx+2), z );
  }
  else if(idx == psize(cont)-1 )      // if last point: compare to two points before
  {
    expPos = bead_getPtOnLineWithZ( getPt(cont,idx-1), getPt(cont,idx-2), z );
  }
  else                                // else: compare to point either side
  {
    expPos = bead_getPtOnLineWithZ( getPt(cont,idx-1), getPt(cont,idx+1), z );
  }
  
  return  ABS(expPos.y - getPt(cont,idx)->y);
}



//------------------------
//-- Calculates the a weighted value representing the average jump in y value between
//-- each set of three consecutive points. This works on the principle that,
//-- if the contour is smooth, then expect the y distance between two points
//-- to be only slightly different from the y distance between the next
//-- two points.

float bead_calcAvgYJump( Icont *cont )
{
  if( !cont || psize(cont) < 5 )
    return 0;
  
  float totalYJump = 0;
  
  for(int p=0; p<psize(cont); p++)
    totalYJump += bead_calcYJump(cont, p);
  
  float avgYJump = totalYJump / (float)psize(cont);
  
  return ( avgYJump );
}


//------------------------
//-- Calculates distance of the point "idx" in the contour from it's estimated
//-- position based on the current estimated point criteria. If "weighted"
//-- is set then the distance is divided by the distance of points either side.
//-- This is done so a deviation of say, 3 pixels, for a point at high tilt
//-- (where the points move rapidly) will be given a much smaller weighting than
//-- at low tilt (where the movement of points is small and easier to see).

float bead_calcDistanceFromExpected( Icont *cont, int idx, bool weighted )
{
  if( !cont || psize(cont) < 5 || idx < 0 || idx >= psize(cont) )
    return 0;
  
  int z = getPtZInt(cont,idx);
  
  Ipoint estimatedPosPt;
  if( !bead_getExpectedPosOfPoint( cont, z, &estimatedPosPt ) )
      return 0;
  
  float distFromExpected = line_distBetweenPts2D( getPt(cont,idx), &estimatedPosPt );
  
  if(weighted)
  {
    Ipoint *pt1 = (idx==0)             ? getPt( cont,idx-1 ) : getPt(cont,idx+2);
    Ipoint *pt2 = (idx==psize(cont)-1) ? getPt( cont,idx+1 ) : getPt(cont,idx-2);
    
    float distPt1AndPt2 = line_distBetweenPts2D( pt1, pt2 );
    float distPerView  = fDivide( distPt1AndPt2, pt2->z - pt1->z );
    
    float crudeWeightDev = distFromExpected / (distPt1AndPt2 + plug.wWeightedDiv);
    return (crudeWeightDev);
  }
  else
  {
    return (distFromExpected);
  }
}

//------------------------
//-- Calculates the crude devition of each point in the contour from it's estimated
//-- position based on the position of the two "reference" points either side of it,
//-- which is then divided by the distance between the two "reference" points.
//-- This is done so a deviation of say, 3 pixels, for a point at high tilt
//-- (where the points move rapidly) will be given a much smaller weighting than
//-- at low tilt (where the movement of points is small and easier to see).
//-- 
//-- The weighted deviations are added together and then divided by 
//-- the number of points and returned. The higher the value returned, the
//-- more bumpy the curve.

float bead_calcCrudeWeightedDevFromMiddle( Icont *cont )
{
  if( !cont || psize(cont) < 5 )
    return 0;
  
  float totalWeightedDev = 0;
  
  for(int p=1; p<psize(cont)-1; p++)
  {
    Ipoint *pt = getPt(cont,p);
    Ipoint *pt1 = (p==0)             ? getPt( cont,p-1 ) : getPt(cont,p+2);
    Ipoint *pt2 = (p==psize(cont)-1) ? getPt( cont,p+1 ) : getPt(cont,p-2);
    
    float fractToExpectedPt = fDivide( (pt->z - pt1->z) , ( pt2->z - pt1->z) ); 
    Ipoint estimatedPosPt = line_findPtFractBetweenPts( pt1, pt2, fractToExpectedPt );
    
    float distFromExpected = line_distBetweenPts2D( pt, &estimatedPosPt );
    float distPt1AndPt2    = line_distBetweenPts2D( pt1, pt2 );
    float crudeWeightDev = distFromExpected / (distPt1AndPt2 + plug.wWeightedDiv);
    
    totalWeightedDev += crudeWeightDev;
  }
  
  float finalWeightedDev = totalWeightedDev / (float)psize(cont);
  
  return (finalWeightedDev);
}


//------------------------
//-- Returns the grey scale value in memory of the pixel nearest to the given point

float bead_getGreyValue( Ipoint *pt )
{
  int x = int(pt->x + 0.5);
  int y = int(pt->y + 0.5);
  int z = int(pt->z + 0.5);
  
  float greyVal = ivwGetFileValue( plug.view, x,y,z );
  return greyVal;
}

//------------------------
//-- Returns the average grey value of the points in the contour.

float bead_avgGreyValueOfPts( Icont *cont )
{
  if( isEmpty(cont) )
    return 0;
  
  float totalGreyVal = 0;
  
  for(int p=0; p<psize(cont); p++)
    totalGreyVal += bead_getGreyValue( getPt(cont,p) );
  
  float avgGreyVal = totalGreyVal / (float)psize(cont);
  
  return (avgGreyVal);
}


//------------------------
//-- Returns the distance of the middle-most point from the
//-- center of the tomogram, or returns FLOAT_MAX if there
//-- are no points

float bead_distFromMiddle( Icont *cont )
{
  if( isEmpty(cont) )
    return FLOAT_MAX;
  
  Ipoint *closestPtToMiddle = bead_getClosestPtToView( cont, plug.seedView );
  
  return ( line_distBetweenPts2D( closestPtToMiddle, &plug.middlePt ) );
}


//------------------------
//-- Sorts and reorderes all contours past "minCont" according to the specified
//-- "sortCriteria" (see enum contsortcriteria).
//-- Setting "reverse" will sort the value in descending instead of descending.
//-- Setting "printVals" will output the sort values for each contour.

void bead_reorderConts( int sortCriteria, int minCont, int maxCont,        
                        bool calcValsOnly, bool reverse, bool printVals )
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int nConts = csize(obj);
  int minC = MAX( minCont,0 );
  int maxC = MIN( maxCont, csize(obj)-1 );
  
  //## CREATE VECTOR OF FLOATS TO SORT CONTOURS BY AND CALCULATE SORT VALUES
  //## USING CHOSEN SORT CRITERIA:
  
  plug.sortVals.resize( nConts );
  
  for( int c=0; c<nConts; c++)
  {
    plug.sortVals[c].idx = c;
    Icont *cont = getCont(obj,c);
    
    switch(sortCriteria)
    {
      case(SORT_YJUMPS):
        plug.sortVals[c].float1 = bead_calcAvgYJump(cont);
        break;
      case(SORT_DEV):
        plug.sortVals[c].float1 = bead_calcCrudeWeightedDevFromMiddle(cont);
        break;
      case(SORT_AVG_GREY):
        plug.sortVals[c].float1 = bead_avgGreyValueOfPts(cont);
        break;
      case(SORT_DIST_FROM_MIDDLE):
        plug.sortVals[c].float1 = bead_distFromMiddle(cont);
        break;
      case(SORT_MISSING_PTS):
        plug.sortVals[c].float1 = plug.zsize - psize(cont);
        break;
      case(SORT_UNCHECKED):
        plug.sortVals[c].float1 = isInterpolated(cont) ? 0 : 1;
        break;
      case(SORT_RANDOM):
        plug.sortVals[c].float1 = rand();
        break;   
    }
  }
  
  if(calcValsOnly)
  {
    if( printVals )
    {
      wprint("\nVALUES:\n");
      for( int c=0; c<nConts; c++)
        wprint( "cont %d = %f\n", c+1, plug.sortVals[c].float1 );
    }
    return;
  }
  
  
  //## SORT THE CHOSEN RANGE OF VALUES WITHIN THE SORT VECTOR:
  
  plug.sortVals = vector_sort( plug.sortVals, minC, maxC );
  
  if( sortCriteria==SORT_AVG_GREY )
    reverse = !reverse;
  
  if( reverse )
    plug.sortVals = vector_reverse( plug.sortVals, minC, maxC );
  
  
  //## REORDER THE CONTOURS WITHIN THE OBJECT USING THE NOW SORTED SORT VECTOR:
  
  Iobj *objCopy = imodObjectDup( obj );
  
  int numContsChanged = 0;
  for( int c=minC; c<=maxC; c++)
  {
    if( plug.sortVals[c].idx != c )
    {
      Icont *cont = getCont(obj,c);
      imodSetIndex(imod, objIdx, c, 0);
      undoContourDataChg( plug.view, objIdx, c );      // REGISTER UNDO
      Icont *newCont = getCont( objCopy, plug.sortVals[c].idx );
      cont_copyPoints( newCont, cont, true );
      if( isInterpolated(cont) != isInterpolated(newCont) )
      {
        undoContourPropChg( plug.view, objIdx, c );      // REGISTER UNDO
        setInterpolated( cont, isInterpolated(newCont) );
      }
      numContsChanged++;
    }
  }
  
  if(numContsChanged)
    undoFinishUnit( plug.view );              // FINISH UNDO
  
  imodObjectDelete( objCopy );
  imodSetIndex(imod, objIdx, nConts-1, 0);
  
  
  //## OUTPUT RESULTS:
  
  if( printVals )
  {
    if      (sortCriteria==SORT_YJUMPS)             wprint("\nY JUMP MEASURE:\n");
    else if (sortCriteria==SORT_DEV)                wprint("\nWEIGHTED DEVIATIONS:\n");
    else if (sortCriteria==SORT_AVG_GREY)           wprint("\nAVERAGE GREY VALUES:\n");
    else if (sortCriteria==SORT_DIST_FROM_MIDDLE)   wprint("\nDIST FROM MIDDLE:\n");
    else if (sortCriteria==SORT_RANDOM)             wprint("\nRANDOM VAL:\n");
    for( int c=0; c<nConts; c++)
      wprint( "cont %d = %f\n", c+1, plug.sortVals[c].float1 );
  }
  
  wprint("\n%d contours have been reordered\n", numContsChanged);
}


//------------------------
//-- Uses the "least fitting squares" equation to generate the line of best fit
//-- which represents a linear path (y=a+bx) through the set of scattered
//-- point in the given contour which minimizes the sum of squared residual
//-- distances from the line to each point along y.
//-- 
//-- y = gradient*x + offset
//-- 
//-- see: http://mathworld.wolfram.com/LeastSquaresFitting.html

bool bead_calcLineOfBestFit( Icont *cont, float *gradient, float *offset, int minPts )
{
  if( isEmpty(cont) || psize(cont) < MAX(2,minPts) )
    return false;
  
  
  if( psize(cont)==2 )
  {
    return line_getLineEquation( getPt(cont,0), getPt(cont,1), gradient, offset );
  }
  
  float n = (float) psize(cont);
  
  float sumX    = 0;
  float sumXSq  = 0;
  float sumY    = 0;
  float sumXY   = 0;
  
  for( int p=0; p<psize(cont); p++ )
  {
    float x = getPt(cont,p)->x;
    float y = getPt(cont,p)->y;
    
    sumX     += x;
    sumXSq   += (x*x);
    sumY     += y;
    sumXY    += (x*y);
  }
  
  float avgX = sumX / n;
  float avgY = sumY / n;
  
  *gradient = ( sumXY - (n*avgX*avgY) ) / ( sumXSq - (n*SQ(avgX)) );
  *offset   = ( (avgY*sumXSq) - (avgX*sumXY) ) / ( sumXSq - (n*SQ(avgX)) );
  
  float a   = ( sumXY - (n*avgX*avgY) ) / ( sumXSq - (n*SQ(avgX)) );
  float b   = ( (avgY*sumXSq) - (avgX*sumXY) ) / ( sumXSq - (n*SQ(avgX)) );
  
  return true;
}



//------------------------
//-- Calculates the quadratic function from three given points.
//-- Returns false if two of the given points have the same x value,
//-- therefore cannot calculate the equation.
//-- 
//-- y = a(x^2) + b(x) + c
//-- 
//-- see: http://mathworld.wolfram.com/LeastSquaresFitting.html

bool bead_calcQuadraticCurve( float x1, float x2, float x3, float y1, float y2, float y3,
                              float *a, float *b, float *c )
{
  //a = [(Y2-Y1)(X1-X3) + (Y3-Y1)(X2-X1)]/[(X1-X3)(X2^2-X1^2) + (X2-X1)(X3^2-X1^2)]
  //b = [(Y2 - Y1) - A(X2^2 - X1^2)] / (X2-X1)
  //c = Y1 - AX1^2 - BX1
  
  float divisorA = (x1-x3)*(SQ(x2)-SQ(x1)) + (x2-x1)*(SQ(x3)-SQ(x1));
  float divisorB = (x2-x1);
  
  *a = fDivide ( ( (y2-y1)*(x1-x3) + (y3-y1)*(x2-x1) ), divisorA );
  *b = fDivide (  ( (y2 - y1) - (*a)*(SQ(x2) - SQ(x1)) ), divisorB );
  *c = y1 - (*a)*SQ(x1) - (*b)*x1;
  
  return ( divisorA!=0 && divisorB != 0);
}


//------------------------
//-- Estimates the "turning point" - the point in the contours where the points
//-- stops shifting in X and Y and (usually) changes directions.

bool bead_estimateTurningPointOfCont( Icont *cont, Ipoint *pt,
                                      float minDistRequired, int *idx )
{
  if( isEmpty(cont) || psize(cont)<5 )
    return false;
  
  float minDistBetweenViews = FLOAT_MAX;
  
  for (int p=0; p<psize(cont)-1; p++)
  {
    Ipoint *pt1 = getPt(cont,p);
    Ipoint *pt2 = getPt(cont,p+1);
    
    float distZ = ABS(pt2->z - pt1->z);
    float distXY = line_distBetweenPts2D( pt2, pt1 );
    
    if( distZ == 0 ) {
      wprint( "ERROR: Two points on view %d", getPtZInt(cont,p) );
      continue;
    }
    
    float distBetweenViews = distXY / distZ;
    
    if( distBetweenViews < minDistBetweenViews )
    {
      minDistBetweenViews = distBetweenViews;
      *idx = p;
      *pt = *getPt( cont, p );
    }
  }
  
  return ( minDistBetweenViews <= minDistRequired );
}




//------------------------
//-- Jumps to the contour point in the current object with the biggest or
//-- "next biggest" distance in  Y from where it is estimated to be based
//-- on the linear trajectory between the points either side of it.

float maxYJumpLastIteration = FLOAT_MAX;
float lastContSelected      = INT_MAX;

bool bead_goToNextBiggestYJump( bool findNextBiggest )
{
  if( !isCurrObjValid() )
    return false;
  
  //## SETUP VARIABLES
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  
  float maxYJumpAllowed = FLOAT_MAX;
  if ( findNextBiggest && contIdx == lastContSelected )
  {
    maxYJumpAllowed = maxYJumpLastIteration;
    wprint("Finding NEXT biggest y jump\n");
  }
  else {
    maxYJumpLastIteration = FLOAT_MAX;
    wprint("Finding biggest y jump\n");
  }
  
  //## FIND THE POINT WITH THE NEXT BIGGEST Y JUMP:
  
  float maxYJump = 0;
  int maxContIdx = 0;
  int maxPtIdx = 0;
  
  for (int c=0; c<csize(obj); c++)
  {
    Icont *cont = getCont(obj,c);
    if(    ( plug.contsToSearch==SC_CHECKED   && !isInterpolated(cont) )
        || ( plug.contsToSearch==SC_UNCHECKED && isInterpolated(cont) )  )
      continue;
    
    for (int p=0; p<psize(cont);p++)
    {
      int z = getPtZInt(cont,p);
      if( !plug.includeEndsResid && ( p==0 || p==psize(cont)-1 || z==plug.seedView) )
        continue;
      if( plug.searchRangeOnly && plug.window->updateAndVerifyRanges() &&
          ( z<plug.viewMin || z>plug.viewMax ) )
        continue;
      
      float yJump = bead_calcYJump(cont,p);
      if ( (yJump > maxYJump) && (yJump < maxYJumpAllowed) )
      {
        maxYJump = yJump;
        maxContIdx = c;
        maxPtIdx = p;
      }
    }
  }
  
  //## PRINT RESULT:
  
  if( maxYJump == 0 )
  {
    wprint("\aNo more y jump found found - try pressing 'Y'.\n");
    return false;
  }
  else
  {
    wprint(" --> y jump: %f pixels\n", maxYJump ); 
  }
  
  //## SELECT POINT:
  
  imodSetIndex(imod, objIdx, maxContIdx, maxPtIdx);
  maxYJumpLastIteration = maxYJump;
  lastContSelected = maxContIdx;
  ivwRedraw( plug.view );
  return true;
}



//------------------------
//-- Jumps to the contour point in the current object with the biggest or
//-- "next biggest" deviation from where it is estimated to be based
//-- on the current "estimated position" criteria

float maxDevLastIteration = FLOAT_MAX;

bool bead_goToNextBiggestDev( bool findNextBiggest )
{
  if( !isCurrObjValid() )
    return false;
  
  //## SETUP VARIABLES
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  
  float maxDevAllowed = FLOAT_MAX;
  if ( findNextBiggest && contIdx == lastContSelected )
  {
    maxDevAllowed = maxDevLastIteration;
    wprint("Finding NEXT biggest deviation from estimated point\n");
  }
  else {
    maxDevLastIteration = FLOAT_MAX;
    wprint("Finding biggest deviation from estimated point\n");
  }
  
  //## FIND THE POINT WITH THE NEXT BIGGEST Y JUMP:
  
  float maxDev = 0;
  int maxContIdx = 0;
  int maxPtIdx = 0;
  
  for (int c=0; c<csize(obj); c++)
  {
    Icont *cont = getCont(obj,c);
    if(    ( plug.contsToSearch==SC_CHECKED   && !isInterpolated(cont) )
        || ( plug.contsToSearch==SC_UNCHECKED && isInterpolated(cont) )  )
      continue;
    
    for (int p=0; p<psize(cont); p++)
    {
      int z = getPtZInt(cont,p);
      if( !plug.includeEndsResid && ( p==0 || p==psize(cont)-1 || z==plug.seedView) )
        continue;
      if( plug.searchRangeOnly && plug.window->updateAndVerifyRanges()
          && ( z<plug.viewMin || z>plug.viewMax ) )
        continue;
      
      float deviaton = bead_calcDistanceFromExpected(cont,p,false);
      if ( (deviaton > maxDev) && (deviaton < maxDevAllowed) )
      {
        maxDev = deviaton;
        maxContIdx = c;
        maxPtIdx = p;
      }
    }
  }
  
  //## PRINT RESULT:
  
  if( maxDev == 0 )
  {
    wprint("\aNo more deviations found found - try pressing 'B'.\n");
    return false;
  }
  else
  {
    wprint(" --> deviation: %f pixels\n",   maxDev );
  }
  
  //## SELECT POINT:
  
  imodSetIndex(imod, objIdx, maxContIdx, maxPtIdx);
  maxDevLastIteration = maxDev;
  lastContSelected = maxContIdx;
  ivwRedraw( plug.view );
  return true;
}



//------------------------
//-- Jumps to the contour point in the current object with the biggest or
//-- "next biggest" weighted deviation from where it is estimated to be based
//-- on the current "estimated position" criteria

float maxWDevLastIteration = FLOAT_MAX;

bool bead_goToNextBiggestWeightedDev( bool findNextBiggest )
{
  if( !isCurrObjValid() )
    return false;
  
  if( plug.wCurrContOnly && !isCurrContValid() )
  {
    wprint("\aSelect contour first\n");
    return false;
  }
  
  //## SETUP VARIABLES
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  float maxDevAllowed = FLOAT_MAX;
  if ( findNextBiggest && contIdx == lastContSelected )
  {
    maxDevAllowed = maxWDevLastIteration;
    wprint("Finding NEXT biggest weighted deviation\n");
  }
  else {
    maxWDevLastIteration = FLOAT_MAX;
    wprint("Finding biggest weighted deviation\n");
  }
  
  //## FIND THE POINT WITH THE NEXT BIGGEST Y JUMP:
  
  float maxDev = 0;
  int maxContIdx = 0;
  int maxPtIdx = 0;
  
  for (int c=0; c<csize(obj); c++)
  {
    if( plug.wCurrContOnly && c!=contIdx )
      continue;
    
    Icont *cont = getCont(obj,c);
    for (int p=0; p<psize(cont); p++)
    {
      int z = getPtZInt(cont,p);
      if( !plug.includeEndsResid && ( p==0 || p==psize(cont)-1 || z==plug.seedView) )
        continue;
      
      float deviaton = bead_calcDistanceFromExpected(cont,p,true);
      if ( (deviaton > maxDev) && (deviaton < maxDevAllowed) )
      {
        maxDev = deviaton;
        maxContIdx = c;
        maxPtIdx = p;
      }
    }
  }
  
  //## PRINT RESULT:
  
  if( maxDev == 0 )
  {
    wprint("\aNo more deviations found found - try pressing 'W'.\n");
    return false;
  }
  else
  {
      wprint(" --> weighted deviation: %f\n", maxDev );
  }
  
  //## SELECT POINT:
  
  imodSetIndex(imod, objIdx, maxContIdx, maxPtIdx);
  maxWDevLastIteration = maxDev;
  lastContSelected = maxContIdx;
  ivwRedraw( plug.view );
  return true;
}



//------------------------
//-- Uses a grid of points to determine the "biggest hole" between
//-- seed points on the seed view. If "findNextBiggest" is true
//-- and the user has not added any points since the last iteration
//-- it will instead find the "next biggest hole". Returns true if a hole
//-- was found.


float maxDistLastIteration    = FLOAT_MAX;
int   numSeedPtsLastIteration = 0;


bool bead_goToNextBiggestHole( bool findNextBiggest )
{
  if( !isCurrObjValid() )
    return false;
  
  
  //## CREATE A NEW CONTOUR CONTAINING ALL SEED POINTS:
  
  Icont *allSeedPts = imodContourNew();
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  for (int c=0; c<csize(obj); c++)
  {
    Icont *cont = getCont(obj,c);
    Ipoint *pt = bead_getPtOnView(cont,plug.seedView);
    if( pt != NULL )
      imodPointAppendXYZ( allSeedPts, pt->x, pt->y, pt->z );
  }
  
  //## DETERMINE WETHER USER HAS ADDED POINTS SINCE LAST ITERATION, IF NOT
  //## GO TO NEXT BIGGEST HOLE (INSTEAD OF BIGGEST HOLE)
  
  int numSeedPts = psize(allSeedPts);
  float maxDistAllowed = FLOAT_MAX;
  if ( findNextBiggest && numSeedPts == numSeedPtsLastIteration ) {
    maxDistAllowed = maxDistLastIteration;
    wprint("Finding NEXT biggest hole\n");
  }
  else {
    maxDistLastIteration = FLOAT_MAX;
    wprint("Finding biggest hole\n");
  }
  
  if( numSeedPts == 0 )
  {
    wprint("No seed points exist\n");
    bead_focusOnPointCrude( plug.middlePt.x, plug.middlePt.y, plug.middlePt.z );
    maxDistLastIteration = FLOAT_MAX;
    numSeedPtsLastIteration = 0;
    return true;
  }
  
  //## INITIALIZE A GRID OF POINT TO MEASURE THE DISTANCE TO THE NEAREST
  //## SEED POINT OR EDGE:
  
  int colsX = int(plug.xsize / plug.biggestHoleGrid);
  int rowsY = int(plug.ysize / plug.biggestHoleGrid);
  
  float sideLenX = fDivide(plug.xsize, colsX);
  float sideLenY = fDivide(plug.ysize, rowsY);
  
  vector< vector<float> > minDistGrid( colsX );
  
  for(int x=0; x<colsX; x++)
  {
    float nearestSideX = MIN(x,colsX-x) * sideLenX;
    for(int y=0; y<rowsY; y++)
    {
      float nearestSideY = MIN(y,rowsY-y) * sideLenY;
      float nearestSide  = MIN( nearestSideX, nearestSideY) - plug.biggestHoleInset;
      minDistGrid[x].push_back( nearestSide );
    }
  }
  
  
  //## FOR EACH GRID POINTS, UPDATE TO CONTAIN DISTANCE TO CLOSEST POINT:
  
  Ipoint pt;
  pt.z = plug.seedView;
  
  for(int x=0; x<colsX; x++)
  {
    pt.x = x * sideLenX;
    for(int y=0; y<rowsY; y++)
    {
      pt.y = y * sideLenY;
      for (int p=0; p<psize(allSeedPts); p++)
      {
        float dist = line_distBetweenPts2D( &pt, getPt(allSeedPts,p) );
        minDistGrid[x][y] = MIN( minDistGrid[x][y], dist );
      }
    }
  }
  
  //## FIND THE POINT WITH THE MINIMUM DISTANCE:
  
  float maxDist = 0;
  Ipoint maxPt;
  
  for(int x=0; x<colsX; x++)
  { 
    for(int y=0; y<rowsY; y++)
    { 
      if( minDistGrid[x][y] > maxDist && minDistGrid[x][y] < maxDistAllowed )
      {
        maxDist = minDistGrid[x][y];
        maxPt.x = x * sideLenX;
        maxPt.y = y * sideLenY;
        maxPt.z = plug.seedView;
      }
    }
  }
  
  //## PRINT RESULT:
  
  if( maxDist == 0 )
  {
    wprint("\aNo more holes found\n"
           "Try pressing 'H' or change 'big hole grid size' in settings\n");
    return false;
  }
  else
  {
    wprint(" --> distance: %d pixels\n", (int)maxDist ); 
  }
  
  //## SET FOCUS ON NEW LOCATION:
  
  bead_focusOnPointCrude( maxPt.x, maxPt.y, maxPt.z );  
  imodContourDelete(allSeedPts);  
  
  maxDistLastIteration = maxDist;
  numSeedPtsLastIteration = numSeedPts;
  
  
   
  return true;
}



//------------------------
//-- Go to the contour with the "next biggest" sort value
//-- within "plugs.sortVals", and will recalculate these values
//-- if "findNextBiggest" is false, or the number of contours
//-- doesn't match the number of sort values.
//-- Returns true if a contour is found and selected.

float maxValLastIteration = FLOAT_MAX;

bool bead_goToContNextBiggestSortVal( bool findNextBiggest )
{
  if( !isCurrObjValid() )
    return false;
  
  //## SETUP VARIABLES
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  
  float maxValAllowed = FLOAT_MAX;
  if ( findNextBiggest && plug.sortVals.size() == csize(obj) )
  {
    if( csize(obj) == 0 ) {
      wprint("\aThere are no contours to sort\n");
      return false;
    }
    maxValAllowed = maxValLastIteration;
    wprint("Finding NEXT biggest value\n");
  }
  else {
    maxValLastIteration = FLOAT_MAX;
    wprint("Finding biggest value\n");
    wprint(" --> RECALCULATING ORDER ...\n");
    bead_reorderConts( plug.sortCriteria, 0, csize(obj)-1, true, false, false );
  }
  
  
  //## FIND THE POINT WITH THE NEXT BIGGEST Y JUMP:
  
  float maxVal     = -9999;
  int   maxContIdx = -1;
  
  for (int v=0; v<plug.sortVals.size(); v++)
  {
    Icont *cont = getCont(obj,v);
    
    if(    ( plug.contsToSearch==SC_CHECKED   && !isInterpolated(cont) )
        || ( plug.contsToSearch==SC_UNCHECKED && isInterpolated(cont) )  )
      continue;
    
    float val = plug.sortVals[v].float1;
    if ( (val > maxVal) && (val < maxValAllowed) )
    {
      maxVal     = plug.sortVals[v].float1;
      maxContIdx = plug.sortVals[v].idx;
    }
  }
  
  //## PRINT RESULT:
  
  if( maxContIdx==-1 )
  {
    wprint("\aNo more 'next worst' values found - try pressing 'O' to reset.\n");
    return false;
  }
  wprint(" --> value: %f\n",   maxVal );
  
  //## SELECT CONTOUR:
  
  Icont *contToSelect = getCont(obj,maxContIdx);
  ptIdx = MIN( ptIdx, psize(contToSelect)-1 );
  ptIdx = MAX( ptIdx, 0 );
  imodSetIndex(imod, objIdx, maxContIdx, ptIdx);
  maxValLastIteration = maxVal;
  lastContSelected = maxContIdx;
  ivwRedraw( plug.view );
  return true;
}



//------------------------
//-- Estimates the tilt axis angle by averaging the gradient for the "line of best fit"
//-- over all contours.

float bead_estimateTiltAngle()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  
  float angleSum = 0;
  int   totalConts = 0;
  
  wprint( "\nCONTOUR ANGLES: \n" );
  for (int c=0; c<csize(obj); c++)
  {
    Icont *cont = getCont(obj,c); 
    
    float gradient, offset;
    bool success = bead_calcLineOfBestFit( cont, &gradient, &offset, 4 );
    
    if( success )
    {
      float angle = atan( gradient ) * RADS_TO_DEGS;
      angleSum += angle;
      totalConts++;
    }
  }
  
  if( totalConts == 0 )
  {
    wprint("/aERROR: No contours could be measured"); 
    return 0;
  }
  
  float avgAngle = angleSum / (float)totalConts;
  wprint("\nAverage angle over %d contours = %f%c", totalConts, avgAngle, DEGREE_SIGN);
  return (avgAngle);
}




//------------------------
//-- Goes through all contours, and determines which fiducials are on the bottom,
//-- and show them in purple on an extra object.
//-- A fiducial is predicted to be on the bottom if it moves left between
//-- the views specified.... while fiducials on top travel the other direction -
//-- hence it is important to chose "zMin" and "zMax" carefully.

bool bead_showBottomContoursInPurple( int zMin, int zMax )
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  
  Iobj *objTemp = imodObjectDup( obj );
  
  //## NORMALIZE CONTOURS BY ROTATING THEN MOVING EACH
  //## RELATIVE TO THE TILT AXIS
  
  for (int c=0; c<csize(objTemp); c++)
    cont_rotateAroundPoint2D( getCont(objTemp,c), &plug.middlePt, -plug.tiltAxisAngle );
  
  //## FOR EACH FIDUCIAL: IF IT TRAVELS LEFT BETWEEN THE TWO VIEW MARK IT
  //## AS A BOTTOM CONTOUR
  
  int numTopFids     = 0;
  int numBottomFids  = 0;
  int numUnknown     = 0;
  
  //## FOR EACH CONTOUR DETERMINE WHICH DIRECTION THE POINT IS TRAVELLING AND IF
  //## TRAVELS LEFT THEN ASSUME IT'S ON BOTTOM AND ADD CONTOUR TO PURPLE OBJECT:
  
  Iobj *xobjX = ivwGetAnExtraObject(plug.view, plug.extraObjExtra);
  ivwClearAnExtraObject(plug.view, plug.extraObjExtra);
  
  for (int c=0; c<csize(obj); c++)
  {
    Icont *cont = getCont(objTemp,c);
    
    Ipoint *minPt  = bead_getClosestPtToView(cont, zMin );
    Ipoint *maxPt  = bead_getClosestPtToView(cont, zMax );
    
    if( psize(cont) < 4 || minPt->z == maxPt->z )
    {
      wprint("cont %d - too few points\n",c+1);
      numUnknown++;
      continue;
    }
    
    bool fidTravelsRight  = maxPt->x  > minPt->x;
    
    if( fidTravelsRight )
    {
      numTopFids++;
    }
    else
    {
      numBottomFids++;
      Icont *contX = imodContourDup( getCont(obj,c) );
      imodObjectAddContour( xobjX, contX );
      free(contX);
    }
  }
  
  
  //## PRINT SUMMARY OF ANALYSIS:
  
  wprint( "ANALYSIS OF FIDUCIALS: \n" );
  wprint( " Unknown:   %d\n", numUnknown );
  wprint( " On top:    %d\n", numTopFids );
  wprint( " On bottom: %d\n", numBottomFids );
  
  imodObjectDelete( objTemp );
}




//------------------------
//-- Goes through all contours and tries to find a turning point
//-- of each contour (where it changes direction).

bool bead_showContourTurningPts()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  
  Iobj *xobjX = ivwGetAnExtraObject(plug.view, plug.extraObjExtra);
  ivwClearAnExtraObject(plug.view, plug.extraObjExtra);
  
  //## TRY TO DETERMINE TURNING POINT FOR EACH CONTOUR:
  
  Icont *turningPts = imodContourNew();       // stores a turning point corresponding
                                              // to each contout in the object
  
  int turningZMin = plug.zsize * 0.1;
  int turningZMax = plug.zsize * 0.9;
  
  for (int c=0; c<csize(obj); c++)
  {
    Ipoint turningPt;
    int idx;
    Icont *cont = getCont(obj,c); 
    bool success = bead_estimateTurningPointOfCont( cont, &turningPt, 3.0, &idx );
    if( !success || turningPt.z < turningZMin || turningPt.z > turningZMax )
      turningPt.z = -1;
    imodPointAppend( turningPts, &turningPt );
  }
  
  //## SHOW TURNING POINTS IN NEW OBJECT:
  
  for (int p=0; p<psize(turningPts); p++)
  {
    Ipoint *pt = getPt(turningPts,p);
    cont_addPtToObj( xobjX, pt->x, pt->y, pt->z, false );
  }  
}


//------------------------
//-- Displays a grid using the 'purple object'

void bead_showGrid()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int colsX = 5;
  static int rowsY = 5;
  static int showStyle = 0;
  static bool rotateByAngle = false;
  static bool dottedLines   = false;
  
	CustomDialog ds;
  int ID_COLS         = ds.addSpinBox ( "columns (x):", 1, 500, colsX, 1,
                                        "Number of columns to display" );
  int ID_ROWS         = ds.addSpinBox ( "rows    (y):", 1, 500, rowsY, 1,
                                        "Number of rows to display" );
	int ID_SHOWSTYLE    = ds.addRadioGrp( "show grid as:",
                                         "lines,"
                                         "spheres only",
                                         showStyle );
  int ID_ROTATE       = ds.addCheckBox( "rotate by tilt axis angle",
                                         rotateByAngle,
                                         "Tilt the grid by the tilt axis angle" );
  int ID_DOTTEDLINES  = ds.addCheckBox( "use dotted lines", dottedLines );
	GuiDialogCustomizable dlg(&ds, "Grid Options");
	dlg.exec();
	if( ds.cancelled )
		return;
  colsX           = ds.getResultSpinBox   ( ID_COLS );
  rowsY           = ds.getResultSpinBox   ( ID_ROWS );
	showStyle       = ds.getResultRadioGrp	( ID_SHOWSTYLE );
  rotateByAngle   = ds.getResultCheckBox  ( ID_ROTATE );
  dottedLines     = ds.getResultCheckBox  ( ID_DOTTEDLINES );
  
  
  //## SETUP GRID:
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  
  Iobj *xobjX = ivwGetAnExtraObject(plug.view, plug.extraObjExtra);
  ivwClearAnExtraObject(plug.view, plug.extraObjExtra);
  
  float colXSide = fDivide( plug.xsize, colsX );
  float rowYSide = fDivide( plug.ysize, rowsY );
  float z = plug.seedView;
  
  if( showStyle == 0)
  {
    for (int x=0; x<=colsX; x++)
      cont_addLineToObj( xobjX, x*colXSide,0,z,
                                x*colXSide,plug.ysize,z, true,dottedLines );
    for (int y=0; y<=rowsY; y++)
      cont_addLineToObj( xobjX, 0,y*rowYSide,z,
                                plug.xsize, y*rowYSide,z, true,dottedLines );
  }
  else
  {
    for (int x=0; x<=colsX; x++)
      for (int y=0; y<=rowsY; y++)
        cont_addPtToObj( xobjX, x*colXSide, y*rowYSide,z, true );
  }
  
  //## ROTATE GIRD (IF SPECIFIED)
  
  if( rotateByAngle )
  {
    for (int c=0; c<csize(xobjX); c++)
      cont_rotateAroundPoint2D(getCont(xobjX,c), &plug.middlePt, plug.tiltAxisAngle);
  }
  
  plug.window->goToSeedView();
}
