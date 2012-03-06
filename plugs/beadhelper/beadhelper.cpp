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

#include <QWheelEvent>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QGridLayout>
#include <QKeyEvent>
#include <QEvent>
#include <QVBoxLayout>

#include "_common_functions.h"
#include "customdialog.h"
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

const char *imodPlugInfo(int *type)
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
  int ctrl    = event->modifiers() & Qt::ControlModifier;   // ctrl modifier
  int shift   = event->modifiers() & Qt::ShiftModifier;     // shift modifier
  
  if(ctrl)          // if the control key is down we don't want to do anything!
    return 0;
  
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
      if (shift || plug.dKeyBehav == DK_NONE)
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
      bead_goToNextBiggestYJump( !shift );
      break;
      
    case Qt::Key_W:                  // go to next biggest weighted deviation
      bead_goToNextBiggestWeightedDev( !shift );
      break;
      
    case Qt::Key_B:                  // go to next biggest deviation from estimated
      bead_goToNextBiggestDev( !shift );
      break;
      
    case Qt::Key_O:                  // go to contour with next biggest sort value
      bead_goToContNextBiggestSortVal( !shift );
      break;
      
    case Qt::Key_I:                  // print info or inverse direction
      if(shift)
        plug.window->printContourCheckedInfo();
      else
        return 0;
      break;
      
    case Qt::Key_U:                  // toggle contour as checked/unchecked
    case Qt::Key_Q:
    if(shift)
      {
        if(plug.uKeyBehav == UK_PRINTINFO)
          plug.window->printContourCheckedInfo();
        else
          plug.window->togglePtsChecked();
      }
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
      
    
//    case Qt::Key_T:                  // temporary testing purposes - comment out %%%%
//      if (shift)
//        plug.window->test();
//      else
//        return 0;
//      break;
    
      
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
    
    plug.window->initValues();
    
    //** OTHER:
    
    plug.window->loadSettings();
    plug.initialized        = true;
  }
  plug.minPtsEnter           = plug.zsize - 1;
  
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
  
  
  //## CREATE THE PLUGIN WINDOW:
  
  plug.window  = new BeadHelper(imodDialogManager.parent(IMOD_DIALOG),"Bead Helper");
  
  imodDialogManager.add((QWidget *)plug.window, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)plug.window, IMOD_DIALOG );
  //plug.window->show();
  
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
        if( plug.centerPtOnScroll )
        {
          Imod *imod = ivwGetModel(plug.view);
          Ipoint *pt = imodPointGet(imod);
          if (pt)
            ivwSetTopZapCenter(plug.view, pt->x, pt->y, pt->z, false);
        }
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
            
            if( plug.centerPtOnScroll )
            {
              Ipoint *pt = imodPointGet(imod);
              if (pt)
                ivwSetTopZapCenter(plug.view, pt->x, pt->y, pt->z, false);
            }
            plug.window->drawExtraObject(false);
            ivwRedraw( plug.view );
            return 1;
          }
          
          if( plug.centerPtOnScroll )
          {
            Ipoint estPt;
            if ( bead_getExpectedPosOfPoint( getCurrCont(), newView, &estPt ) )
            {
              ivwSetTopZapCenter(plug.view, estPt.x, estPt.y, estPt.z, false);
              plug.window->drawExtraObject(true);
              return 1;
            }
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

static const char *buttonLabels[] = {(char*)"Done", (char*)"Video", (char *)"Help"};
static const char *buttonTips[]   = {(char*)"Close this plugin window",
	                             (char*)"See SLASH help videos showing \n"
	                                    "how to use this plugin",
	                             (char*)"Open help window"};

BeadHelper::BeadHelper(QWidget *parent, const char *name) :
      DialogFrame(parent, 3, buttonLabels, buttonTips, true, "Bead Helper", "", name)
{
  const int LAY_MARGIN   = 4;
  const int LAY_SPACING  = 4;
  const int GROUP_MARGIN    = 1;
  const int SPACER_HEIGHT   = 15;
  
  QString toolStr;
  
	//## RECOLOR MIDDLE "Video" BUTTON:
	
	mButtons[1]->setStyleSheet("color: rgb(150, 180, 255);");
	mButtons[1]->setFlat( true );
	mButtons[1]->setCursor( Qt::PointingHandCursor );
	mButtons[2]->setCursor( Qt::PointingHandCursor );
	
	
  //## Actions:
  
  grpActions = new QGroupBox("Actions:", this);
  grpActions->setFocusPolicy(Qt::NoFocus);
  
  vboxLayout1 = new QVBoxLayout(grpActions);
  vboxLayout1->setSpacing(LAY_SPACING);
  vboxLayout1->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  //vboxLayout1->addItem( new QSpacerItem(1,SPACER_HEIGHT) );
  
  deletePtsButton = new QPushButton("Delete points in range [d]", grpActions);
  connect(deletePtsButton, SIGNAL(clicked()), this, SLOT(deletePtsInRange()));
  deletePtsButton->setToolTip(
                "Deletes all points on the specified views from the specified "
                "range of contours");
  vboxLayout1->addWidget(deletePtsButton);
  
  reduceContsToSeedButton = new QPushButton("Reduce contours to seed  [r]", grpActions);
  QObject::connect(reduceContsToSeedButton, SIGNAL(clicked()), this,
                   SLOT(reduceContsToSeed()));
  reduceContsToSeedButton->setToolTip(
                "Deletes all points in the current contour except on the seed view");
  vboxLayout1->addWidget(reduceContsToSeedButton);
  
  movePtsToEstButton = new QPushButton("Move pts to estimated [e]", grpActions);
  QObject::connect(movePtsToEstButton, SIGNAL(clicked()), this,
                   SLOT(movePtsToEstimatedPosOptions()));
  movePtsToEstButton->setToolTip(
                "Smooths contour(s) by moving points towards their estimated position "
                "(provides several different methods to achieve this)");
  vboxLayout1->addWidget(movePtsToEstButton);
  
  fillMissingPtsButton = new QPushButton("Fill missing points   [f]", grpActions);
  QObject::connect(fillMissingPtsButton, SIGNAL(clicked()), this,
                   SLOT(fillMissingPts()));
  fillMissingPtsButton->setToolTip(
                "Fills in missing points");
  vboxLayout1->addWidget(fillMissingPtsButton);
  
  reorderContsButton = new QPushButton("Reorder contours   [o]", grpActions);
  connect(reorderContsButton, SIGNAL(clicked()), this, SLOT(reorderContours()));
  reorderContsButton->setToolTip(
                "Reorders the specified range of contours by one of several crititeria");
  vboxLayout1->addWidget(reorderContsButton);
  
  mLayout->addWidget(grpActions);
  
  
  //## Display:
  
  grpDisplay = new QGroupBox("Visual Aids:", this);
  //grpDisplay->setFocusPolicy(Qt::NoFocus);
  //grpDisplay->setMargin(GROUP_MARGIN);
  
  gridLayout2 = new QGridLayout(grpDisplay);
  gridLayout2->setSpacing(LAY_SPACING);
  gridLayout2->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  //gridLayout2->addItem( new QSpacerItem(1,SPACER_HEIGHT), 0, 0);
  
  showEstimatedPosCheckbox = new QCheckBox("show estimated position", grpDisplay);
  showEstimatedPosCheckbox->setChecked( plug.showExpectedPos );
  QObject::connect(showEstimatedPosCheckbox,SIGNAL(clicked()),this,
                   SLOT(changeShowExpectedPos()));
  showEstimatedPosCheckbox->setToolTip( 
                "Shows the estimated position of the current point based on "
                "position of points either side");
  
  //## Pin-to-top Button:
  QHBoxLayout* topLay = new QHBoxLayout();
  topLay->setSpacing(0);
  topLay->setContentsMargins(0,0,0,0);
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
  toolBut->setToolTip("Keep bead helper window on top");
  topLay->addWidget(showEstimatedPosCheckbox);
  topLay->addWidget(toolBut);
  
  gridLayout2->addLayout(topLay, 1, 0, 1, 2);
  //gridLayout2->addWidget(showEstimatedPosCheckbox, 1, 0, 1, 2);
  
  showSpheresCheckbox = new QCheckBox("sphere size:", grpDisplay);
  showSpheresCheckbox->setChecked( plug.showSpheres );
  QObject::connect(showSpheresCheckbox,SIGNAL(clicked()),this,
                   SLOT(changeShowSpheres()));
  showSpheresCheckbox->setToolTip(
                "Show/hide spheres on the current object");
  gridLayout2->addWidget(showSpheresCheckbox, 2, 0);
  
  sphereSizeSpinner = new QSpinBox(grpDisplay);
  sphereSizeSpinner->setRange(1, 50);
  sphereSizeSpinner->setFocusPolicy(Qt::NoFocus);
  sphereSizeSpinner->setValue( plug.sphereSize );
  QObject::connect(sphereSizeSpinner,SIGNAL(valueChanged(int)),this,
                   SLOT(changeSphereSize(int)));
  sphereSizeSpinner->setToolTip(
                "The size of the spheres you wish to display");
  gridLayout2->addWidget(sphereSizeSpinner, 2, 1);
  
  lblLineDisplay = new QLabel("line display: ", grpDisplay);
  lblLineDisplay->setFocusPolicy(Qt::NoFocus);
  lblLineDisplay->setToolTip( 
              "Visual aid to let you see the trajectory of contours in the ZAP window");
  gridLayout2->addWidget(lblLineDisplay, 3, 0);
  
  lineDisplayCombo = new QComboBox(grpDisplay);
  lineDisplayCombo->setFocusPolicy(Qt::NoFocus);
  lineDisplayCombo->addItem("off");
  lineDisplayCombo->addItem("all objs");
  lineDisplayCombo->addItem("all contours");
  lineDisplayCombo->addItem("curr contour");
  lineDisplayCombo->addItem("missing pts");
  lineDisplayCombo->addItem("result smooth");
  lineDisplayCombo->addItem("pt residuals");
  lineDisplayCombo->addItem("line best fit");
  lineDisplayCombo->setCurrentIndex( plug.lineDisplayType );
  connect(lineDisplayCombo, SIGNAL(activated(int)), this,
          SLOT(changeLineDisplayType(int)));
  toolStr = "Visual aid to let you see the trajectory of contours. "
    "\n"
    "\n > off             - don't display any lines/contours "
    "\n > all contours        - shows ALL contours in the model (accross all objects) "
      "with any checked contours as stippled "
    "\n > all contours        - shows ALL contours int the current object "
    "\n > curr contour   - shows only the current (selected) contour "
    "\n > missing pts    - shows the selected contour as solid, with the expected "
      " position of any missing points as crosshairs "
    "\n > result smooth - shows the selected contour as solid and what will happen "
      " if the current contour is smoothed by pressing [E] as stippled "
    "\n > pt residuals    - shows the displacement of each point in the current "
      " contour with solid arrows "
    "\n > line best fit  - shows a 'line of best fit'; a straight line which "
      "best approximates all the points in the slected contour";
  lineDisplayCombo->setToolTip( toolStr );
	//lineDisplayCombo->setToolTip( "Visual aid to let you see the trajectory of contours");
	gridLayout2->addWidget(lineDisplayCombo, 3, 1);
  
  lblTiltDisplay = new QLabel("tilt display: ", grpDisplay);
  lblTiltDisplay->setFocusPolicy(Qt::NoFocus);
  lblTiltDisplay->setToolTip( 
                "Visual aid to show the axis about which subsequent views are tilted");
  gridLayout2->addWidget(lblTiltDisplay, 4, 0);
  
  tiltDisplayCombo = new QComboBox(grpDisplay);
  tiltDisplayCombo->setFocusPolicy(Qt::NoFocus);
  tiltDisplayCombo->addItem("off");
  tiltDisplayCombo->addItem("tilt axis");
  tiltDisplayCombo->addItem("tilt and seed");
  tiltDisplayCombo->addItem("tilt and pt");
  tiltDisplayCombo->addItem("tilt segments");
  tiltDisplayCombo->addItem("[h] grid");
  tiltDisplayCombo->setCurrentIndex( plug.tiltDisplayType );
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
	tiltDisplayCombo->setToolTip( toolStr);
	gridLayout2->addWidget(tiltDisplayCombo, 4, 1);
  
  mLayout->addWidget(grpDisplay);
  
  
  //## Display:
  
  grpOptions = new QGroupBox("Options:", this);
  
  gridLayout3 = new QGridLayout(grpOptions);
  gridLayout3->setSpacing(LAY_SPACING);
  gridLayout3->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  
  lblEstMethod = new QLabel("estimation meth:", grpOptions);
  lblEstMethod->setToolTip( 
                "The method used to determine the estimated position of points");
  gridLayout3->addWidget(lblEstMethod, 1, 0);
  
  estPosMethodCombo = new QComboBox(grpOptions);
  estPosMethodCombo->setFocusPolicy(Qt::NoFocus);
  estPosMethodCombo->addItem("best 2 pts");
  estPosMethodCombo->addItem("smart 2 pts");
  estPosMethodCombo->addItem("nearest 2 pts");
  estPosMethodCombo->addItem("prev 2 pts");
  estPosMethodCombo->addItem("curve");
  estPosMethodCombo->addItem("local curve");
  estPosMethodCombo->addItem("prev 6 pts");
  estPosMethodCombo->addItem("prev 3 pts");
  estPosMethodCombo->setCurrentIndex( plug.estPosMethod );
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
  estPosMethodCombo->setToolTip( toolStr );
  gridLayout3->addWidget(estPosMethodCombo, 1, 1);
  
  mLayout->addWidget(grpOptions);
  
  keyboardSettingsButton = new QPushButton("Mouse and Keyboard", grpOptions);
  connect(keyboardSettingsButton, SIGNAL(clicked()), this, SLOT(keyboardSettings()));
  keyboardSettingsButton->setToolTip( "Contains several mouse & keyboard "
                                      "related settings");
  gridLayout3->addWidget(keyboardSettingsButton, 2, 0, 1, 2);
  
  moreActionsButton = new QPushButton("More Actions", grpOptions);
  connect(moreActionsButton, SIGNAL(clicked()), this, SLOT(moreActions()));
  moreActionsButton->setToolTip( "Contains several other actions I didn't "
                                 "want to sqeeze into this window");
  gridLayout3->addWidget(moreActionsButton, 3, 0);
  
  moreSettingsButton = new QPushButton("More Settings", grpOptions);
  connect(moreSettingsButton, SIGNAL(clicked()), this, SLOT(moreSettings()));
  moreSettingsButton->setToolTip(
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
/*
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
}*/


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
  float sc = fDiv( 1.0f, zapZoom);
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  
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
      for(int o=0; o<osize(imod);o++)
      {
        Iobj *objO = getObj(imod,o);
        for(int c=0; c<csize(objO);c++)
        {
          Icont *xcont = imodContourDup( getCont(objO,c) );
          setZValue( xcont, z );
          imodContourSetFlag(xcont, ICONT_DRAW_ALLZ | ICONT_MMODEL_ONLY, 1);
          imodObjectAddContour(xobjC, xcont);
          free(xcont);
        }
      }
    }break;
    
    case( LD_OBJ ):
    {
      for(int c=0; c<csize(obj);c++)
      {
        Icont *xcont = imodContourDup( getCont(obj,c) );
        setZValue( xcont, z );
        imodContourSetFlag(xcont, ICONT_DRAW_ALLZ | ICONT_MMODEL_ONLY, 1);
        imodObjectAddContour(xobjC, xcont);
        free(xcont);
      }
    }break;
    
    case( LD_CURRENT ):
    {
      if( isContValid(cont) )
      {
        Icont *xcont = imodContourDup( cont );
        setZValue( xcont, z );
        imodContourSetFlag(xcont, ICONT_DRAW_ALLZ, 1);
        imodObjectAddContour(xobjC, xcont);
        free(xcont);
      }
    }break;
    
    case( LD_CURRMISSING ):
    {
      if( isContValid(cont) )
      {
        Icont *xcont = imodContourNew();
        cont_makeContShowingMissingPoints( xcont, cont, z, sc*2 );
        
        imodContourSetFlag(xcont, ICONT_DRAW_ALLZ, 1);
        imodObjectAddContour(xobjC, xcont);
        free(xcont);
      }
    }break;
    
    case( LD_RESULTSMOOTH ):
    {
      if( isContValid(cont) )
      {
        Icont *xcont  = imodContourNew();
        cont_makeContShowingMissingPoints( xcont, cont, z, sc*2 );
        imodContourSetFlag(xcont, ICONT_DRAW_ALLZ | ICONT_STIPPLED, 1);
        imodObjectAddContour(xobjC, xcont);
        free(xcont);
        
        Icont *xcontS = imodContourDup( cont );
        int ptsMoved, ptsAdded;
        ivwDraw( plug.view, 0 );          // redraw to ensure correct z is selected
        bead_smoothPtsUsingPlugSettings( xcontS, ptsMoved, ptsAdded );
        setZValue( xcontS, z );
        imodContourSetFlag(xcontS, ICONT_DRAW_ALLZ, 1);
        imodObjectAddContour(xobjC, xcontS);
        free(xcontS);
      }
    }break;
    
    case( LD_SLICE_RESID ):
    {
      if( isContValid(cont) )
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
      if( isContValid(cont) )
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
      float sideLenX = fDiv(plug.xsize, colsX);
      float sideLenY = fDiv(plug.ysize, rowsY);
      
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
//-- Used to initialize default values into BeadHelperData.

void BeadHelper::initValues()
{
  //** MAIN OPTIONS:
  
  plug.viewMinN           = 1;
  plug.viewMaxN           = plug.zsize;
  plug.contMinN           = 6;
  plug.contMaxN           = 9999;
  
  plug.viewMin            = plug.viewMinN - 1;
  plug.viewMax            = plug.viewMaxN - 1;
  plug.contMin            = plug.contMinN - 1;
  plug.contMax            = plug.contMaxN - 1;
  
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
  plug.sizeCheckedPts        = 1.1;
  
  plug.selectedAction        = 0;
  plug.sortCriteria          = 0;
  plug.autoSaveSettings      = true;
  
  //** KEYBOARD AND MOUSE OPTIONS:
  
  plug.wheelBehav            = WH_SMART;
  plug.wheelResistance       = 100;
  plug.centerPtOnScroll      = true;
  plug.disableHotKeys        = false;
  plug.includeEndsResid      = false;
  plug.searchRangeOnly       = false;
  plug.contsToSearch         = SC_ALL;
  plug.dKeyBehav             = DK_OPPOSITEMIDDLE;
  plug.mKeyBehav             = MK_GOTOMIDDLE;
  plug.wCurrContOnly         = true;
  plug.wWeightedDiv          = 15;
  plug.uKeyBehav             = UK_TOGGLEPTCHECKED;
  plug.enterAction           = EN_PREVUNCHECKED;
  plug.minPtsEnter           = plug.zsize - 1;
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
    QMessageBox::about( this, "-- Documentation --",
                        "If this is your first time using 'Bead Helper' \n"
                        "we HIGHLY recommended you click 'Help' \n"
                        "(at bottom of the plugin) and read the documentation ! \n"
                        "                                   -- Andrew Noske" );
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
  plug.sizeCheckedPts       = savedValues[15];
  plug.selectedAction       = savedValues[16];
  plug.sortCriteria         = savedValues[17];
  plug.autoSaveSettings     = savedValues[18];
  
  plug.wheelBehav           = savedValues[19];
  plug.wheelResistance      = savedValues[20];
  plug.centerPtOnScroll     = savedValues[21];
  plug.includeEndsResid     = savedValues[22];
  plug.contsToSearch        = savedValues[23];
  plug.dKeyBehav            = savedValues[24];
  plug.mKeyBehav            = savedValues[25];
  plug.wCurrContOnly        = savedValues[26];
  plug.wWeightedDiv         = savedValues[27];
  plug.uKeyBehav            = savedValues[28];
  plug.enterAction          = savedValues[29];
  plug.minPtsEnter          = savedValues[30];
  plug.enterPrint           = savedValues[31];
  
  plug.smoothCurrContOnly   = savedValues[32];
  plug.smoothFillGaps       = savedValues[33];
  plug.smoothBigResidFirst  = savedValues[34];
  plug.smoothMoveYOnly      = savedValues[35];
  plug.smoothLeaveSeed      = savedValues[36];
  plug.smoothLeaveEnds      = savedValues[37];
  plug.smoothLeaveCurrV     = savedValues[38];
  plug.smoothMoveFract      = savedValues[39];
  plug.smoothMinResid       = savedValues[40];
  plug.smoothIterations     = savedValues[41];
  plug.smoothAdjacentV      = savedValues[42];
  plug.smoothNumViews       = savedValues[43];
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
  saveValues[15] = plug.sizeCheckedPts;
  saveValues[16] = plug.selectedAction;
  saveValues[17] = plug.sortCriteria;
  saveValues[18] = plug.autoSaveSettings;
  
  saveValues[19] = plug.wheelBehav;
  saveValues[20] = plug.wheelResistance;
  saveValues[21] = plug.centerPtOnScroll;
  saveValues[22] = plug.includeEndsResid;
  saveValues[23] = plug.contsToSearch;
  saveValues[24] = plug.dKeyBehav;
  saveValues[25] = plug.mKeyBehav;
  saveValues[26] = plug.wCurrContOnly;
  saveValues[27] = plug.wWeightedDiv;
  saveValues[28] = plug.uKeyBehav;
  saveValues[29] = plug.enterAction;
  saveValues[30] = plug.minPtsEnter;
  saveValues[31] = plug.enterPrint;
  
  saveValues[32] = plug.smoothCurrContOnly;
  saveValues[33] = plug.smoothFillGaps;
  saveValues[34] = plug.smoothBigResidFirst;
  saveValues[35] = plug.smoothMoveYOnly;
  saveValues[36] = plug.smoothLeaveSeed;
  saveValues[37] = plug.smoothLeaveEnds;
  saveValues[38] = plug.smoothLeaveCurrV;
  saveValues[39] = plug.smoothMoveFract;
  saveValues[40] = plug.smoothMinResid;
  saveValues[41] = plug.smoothIterations;
  saveValues[42] = plug.smoothAdjacentV;
  saveValues[43] = plug.smoothNumViews;
  
  prefSaveGenericSettings("BeadHelper",NUM_SAVED_VALS,saveValues);
}




//------------------------
//-- Change to flag to keep on top or run timer as for info window
void BeadHelper::keepOnTop(bool state)
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




//------------------------
//-- Deletes all points within the specified range of views from the specified range
//-- of contours.

void BeadHelper::deletePtsInRange()
{
  if( !verifyCurrObjectAndUpdateRanges() )
    return;
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool skipCheckedConts = true;
  static bool skipCheckedPts   = true;
  static bool skipSeedView     = true;
  
  CustomDialog ds(QString("Delete Points"), this);
  ds.addLabel   ( "----- RANGE: -----", true );
  ds.addLabel   ( "contours (inclusive):" );
  ds.addSpinBox ( "  min:", 1, nConts, &plug.contMinN, 1,
                  "Points will only be deleted from contours "
                  "AFTER this contour # (inclusive)" );
  ds.addSpinBox ( "  max:", 1, nConts, &plug.contMaxN, 1,
                  "Points will only be deleted from contours "
                  "AFTER this contour # (inclusive)" );
  ds.addLabel   ( "views to delete (inclusive):" );
  ds.addSpinBox ( "  min:", 1, plug.zsize, &plug.viewMinN, 1,
                  "Points will only be deleted from views "
                  "AFTER this view (inclusive)" );
  ds.addSpinBox ( "  max:", 1, plug.zsize, &plug.viewMaxN, 1,
                  "Points will only be deleted from views "
                  "BEFORE this view (inclusive)" );
  ds.addLabel   ( "-----", true );
  ds.addCheckBox( "skip checked contours", &skipCheckedConts,
                  "Will not delete any points from checked /n"
                  "(stippled) contours (marked with [u])" );
  ds.addCheckBox( "skip checked points", &skipCheckedPts,
                  "Will not delete checked points (marked with [U]) " );
  ds.addCheckBox( "skip middle (seed) view",  &skipSeedView,
                  "Will not delete any seed points (points on the seed view)" );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  if( !verifyAndUpdateEnteredRangeValues() )
    return;
  
  
  //## DELETE ALL POINTS IN RANGE:
  
  int numPtsDeleted = 0;
  
  for( int c=plug.contMin; c<=plug.contMax && c<csize(obj); c++)
  {
    imodSetIndex(imod, objIdx, c, 0);
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    
    Icont *cont = getCont( obj, c );
    
    numPtsDeleted += bead_deletePtsInZRange( obj, cont, plug.viewMin, plug.viewMax, 
                            skipCheckedConts, skipCheckedPts, skipSeedView, true );
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
  if( !verifyCurrObjectAndUpdateRanges() || !isCurrContValid() )
    return;
  
  int numPtsDeleted = 0;
  
  undoContourDataChgCC( plug.view );      // REGISTER UNDO
  Icont *cont = getCurrCont();
  
  for( int p=0; p<psize(cont); p++ )
  {
    if( isBetweenAsc( plug.viewMinN-1, getPtZInt(cont,p), plug.viewMaxN-1 ) )
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
  if( !verifyCurrObjectAndUpdateRanges() || !isCurrPtValid() || plug.dKeyBehav == DK_NONE )
    return false;
  
  //## GET INFORMATION:
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  Icont *cont = getCurrCont();
  int currPtZ = floor(getCurrPt()->z + 0.5);
  undoContourDataChgCC( plug.view );            // REGISTER UNDO
  
  //## DETERMINE WHAT Z RANGE TO DELETE ACCORDING TO "dKeyBehav" VALUE:
  
  int minZ = plug.viewMinN-1;
  int maxZ = plug.viewMaxN-1;
  
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
  
  int numPtsDeleted = bead_deletePtsInZRange( obj, cont, minZ, maxZ, 
                                              false, false, false, true );
  
  
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
  if( !verifyCurrObjectAndUpdateRanges() )
    return;
  
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool skipCheckedConts = true;
  static bool skipCheckedPts   = true;
  
  CustomDialog ds("Reduce Contours to Seed Point", this);
  ds.addLabel   ( "----- RANGE: -----", true );
  ds.addLabel   ( "contours to reduce (inclusive):" );
  ds.addSpinBox ( "  min:", 1, nConts, &plug.contMinN, 1,
                  "Only contours AFTER this contour # (inclusive)"
                  "will be reduce to a seed point" );
  ds.addSpinBox ( "  max:", 1, nConts, &plug.contMaxN, 1,
                  "Only contours BEFORE this contour # (inclusive)"
                  "will be reduce to a seed point" );
  ds.addLabel   ( "-----", true );
  ds.addLabel   ( "views to fill (inclusive):" );
  
  ds.addCheckBox( "skip checked contours", &skipCheckedConts,
                  "Will not delete any points from checked "
                  "(stippled) contours " );
  ds.addCheckBox( "skip checked points", &skipCheckedPts,
                  "Will not delete checked points "
                  "(marked with [U]) " );
  ds.exec();
  if( ds.wasCancelled() )
    return;

  if( !verifyAndUpdateEnteredRangeValues() )
    return;
  
  //## REDUCE RANGE OF CONTOURS TO SEED:
  
  int numContsReduced = 0;
  int numPtsDeleted = 0;
  
  for( int c=plug.contMin; c<=plug.contMax && c<nConts; c++)
  {
    Icont *cont = getCont( obj, c );
    if( psize(cont) <= 1 || !bead_isPtOnView(cont,plug.seedView)  )
      continue;
    
    undoContourDataChg( plug.view, objIdx, c );      // REGISTER UNDO
    int deleted = bead_deletePtsInZRange( obj, cont, 0, plug.zsize, 
                           skipCheckedConts, skipCheckedPts, true, true );
    if(deleted>0)
      numContsReduced++;
    numPtsDeleted += deleted;
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
    
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    int numPtsDeleted = bead_deletePtsInZRange( getCurrObj(), cont, 0, plug.zsize, 
                                                false, false, true, true );
    undoFinishUnit( plug.view );            // FINISH UNDO
    
    //## OUTPUT RESULT:
    wprint( "Reduced contour %d to seed - %d points deleted\n",
            contIdx+1, numPtsDeleted );
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
  if( !verifyCurrObjectAndUpdateRanges() )
    return;
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int smoothMoveFract  = plug.smoothMoveFract * 10;
  int smoothMinResid   = plug.smoothMinResid  * 10;
  
  CustomDialog ds("Smoothing Options", this);
  ds.addRadioGrp( "apply smoothing to:",
                  "specified range of contours|"
                  "current contour only [E]|"
                  "update values only (no action)",
                  &plug.smoothCurrContOnly );
  ds.addCheckBox( "fill gaps", &plug.smoothFillGaps,
                  "Any missing points between existing points "
                  "will be added" );
  ds.addCheckBox( "move big residuals first", &plug.smoothBigResidFirst,
                  "If ticked: reorders points in order of "
                  "descending distance from expected - which "
                  "works well at correcting isolated bad points. "
                  "If unticked: reorders point from the middle "
                  "point upwards and then downwards.");
  ds.addCheckBox( "change y value only", &plug.smoothMoveYOnly,
                  "All points will only be moved up/down along "
                  "the Y (not left/right along the X)" );
  ds.addCheckBox( "leave seed", &plug.smoothLeaveSeed,
                  "No points on the seed view will "
                  "be moved" );
  ds.addCheckBox( "leave end points", &plug.smoothLeaveEnds,
                  "The first and last point in each contour will "
                  "not be moved" );
  ds.addSpinBox ( "move fraction (X/10):", 1, 10, &smoothMoveFract, 1,
                  "If 10: points will be moved entire way to "
                  "estimated positions" );
  ds.addSpinBox ( "min residual to move (X/10):", 0, 1000, &smoothMinResid, 1,
                  "Only points further than this from their "
                  "estimated point will be moved (measured in "
                  "tenths of a pixel)" );
  ds.addSpinBox ( "iterations:", 1, 10, &plug.smoothIterations, 1,
                  "The more iterations, the further points "
                  "will be moved" );
  ds.addLabel   ("--------", true);
  ds.addCheckBox( "only smooth adjacent views", &plug.smoothAdjacentV,
                  "Only the specified number of view above and "
                  "below the current view will be smoothed" );
  ds.addSpinBox ( "num views either side:", 1, 20, &plug.smoothNumViews, 1,
                  "How many views above and below the current "
                  "view will be moved" );
  ds.addCheckBox( "leave point on current view", &plug.smoothLeaveCurrV, 
                  "will not shift any points in the current view "
                  "in the top most ZAP window" );
  
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  plug.smoothMoveFract          = float( smoothMoveFract ) * 0.1f;
  plug.smoothMinResid           = float( smoothMinResid  ) * 0.1f;
  
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
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  
  //## DISPLAY OPTIONS:
  
  static bool fillPastEnds = false;
  
	CustomDialog ds("Move Points", this);
  ds.addLabel   ( "----- RANGE: -----", this );
  ds.addLabel   ( "contours to smooth (inclusive):" );
  ds.addSpinBox ( "  min:", 1, nConts, &plug.contMinN, 1,
                  "Points will only be moved in contours "
                  "AFTER this contour # (inclusive)" );
  ds.addSpinBox ( "  max:", 1, nConts, &plug.contMaxN, 1,
                  "Points will only be moved in contours "
                  "AFTER this contour # (inclusive)" );
  ds.addLabel   ( "views to fill (inclusive):" );
  ds.addSpinBox ( "  min:", 1, plug.zsize, &plug.viewMinN, 1,
                  "Points will only be moved in views "
                  "AFTER this number (inclusive)" );
  ds.addSpinBox ( "  max:", 1, plug.zsize, &plug.viewMaxN, 1,
                  "Points will only be moved in views "
                  "BEFORE this number (inclusive)" );
  ds.addLabel   ( "-----", true );
  ds.addLabel   ( "Are you sure you want to move ALL points in this range?!" );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  if( !verifyAndUpdateEnteredRangeValues() )
    return;
  
  int currView = edit_getZOfTopZap();
    
  int numPtsMoved = 0;
  int numPtsAdded = 0;
  
  
  //## ITERATE THROUGH SPECIFIED RANGE OF CONTOURS AND MOVE ALL POINTS WITHIN
  //## SPECIFIED VIEWS TO THEIR EXPECTED POSITION: 
  
  for( int c=plug.contMin; c<=plug.contMax && c<csize(obj); c++)
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
  if( !verifyCurrObjectAndUpdateRanges() )
    return;
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool fillPastEnds = false;
  
	CustomDialog ds("Fill Missing Points", this);
  ds.addLabel   ( "----- RANGE: -----", true );
  ds.addLabel   ( "contours to fill (inclusive):" );
  ds.addSpinBox ( "  min:", 1, nConts, &plug.contMinN, 1,
                  "Points will only be added to contours "
                  "AFTER this contour # (inclusive)" );
  ds.addSpinBox ( "  max:", 1, nConts, &plug.contMaxN, 1,
                  "Points will only be added to contours "
                  "AFTER this contour # (inclusive)" );
  ds.addLabel   ( "views to fill (inclusive):" );
  ds.addSpinBox ( "  min:", 1, plug.zsize, &plug.viewMinN, 1,
                  "Points will only be added to views "
                  "AFTER this number (inclusive)" );
  ds.addSpinBox ( "  max:", 1, plug.zsize+1, &plug.viewMaxN, 1,
                  "Points will only be added to views "
                  "BEFORE this number (inclusive)" );
  ds.addLabel   ( "-----", true );
  ds.addCheckBox( "fill past ends",
                  &fillPastEnds,
                  "Will add points past the start and end point of \n"
                  "each contour based on their estimated postions" );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  if( !verifyAndUpdateEnteredRangeValues() )
    return;
  
  
  //## FILL MISSING POINTS:
  
  int numContsChanged = 0;
  int numPtsAddedTotal = 0;
  
  for( int c=plug.contMin; c<=plug.contMax && c<csize(obj); c++)
  {
    Icont *cont = getCont( obj, c );
    
    undoContourDataChg( plug.view, objIdx, c );      // REGISTER UNDO
    int numPtsAdded = bead_fillMissingPtsOnCont( cont, plug.viewMin, plug.viewMax, 
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
  verifyCurrObjectAndUpdateRanges();
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds("Perform Action", this);
  ds.addRadioGrp( "action (for current object):",
                  "calculate tilt axis angle|"
                  "show fiducials on bottom as purple|"
                  "show contour turning points|"
                  "show grid|"
                  "clear purple object|"
                  "move contour range to another object|"
                  "split or merge contours between objects **|"
                  "verify contours"
                     "\n(reorders pts and removes duplicates)|"
                  "mark contours as checked/unchecked **|"
                  "mark points as checked/unchecked|"
                  "print contour info|"
                  "load tilt angles|"
                  "reset values",
                  &plug.selectedAction,
                  "",
                  "Estimates the angle of the tilt axis by averaging the \n"
                    "angles for the 'line of best' for each contour|"
                  "Uses the direction of movement of fiducials to \n"
                    "guess which fidicials are on the bottom and \n"
                    "shows these in purple using the 'purple object'|"
                  "Uses the 'purple object' to show the point in each \n"
                    "contour where the line changes direction.... \n"
                    "which turns out to a fairly useless feature|"
                  "Uses the 'purple object' to display a grid \n"
                    "with your desired number of rows and columns|"
                  "Clears the infamous 'purple object' after you've \n"
                    "applied one of three actions above|"
                  "Use this to move/copy a range of contours/seeds in \n"
                    "the current object to another object. "
                    "This is useful because each object is tracked \n"
                    "seperately and you'll often yeild better results \n"
                    "for a set of points if you first track a subset \n"
                    "and then later move the other points back|"
                  "Use this to split a large number of contours \n"
                    "across multiple objects... or to merge all contours "
                    "into the first object|"
                  "Can be used to quickly fix the common problems \n"
                    "whereby you've accidently added two points \n"
                    "in the same view or they are not in order|"
                  "Use this to change the checked/unchecked value of your contours|"
                  "Prints some basic numbers including the number of \n"
                    "complete contours; incomplete contours; \n"
                    "checked contours and also number of missing points|"
                  "Loads the calculated tilt angle of all views from \n"
                    "the appropriate a .tlt file|"
                  "Resets all setting (for this plugin) to default");
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  
  //## EXECUTE ACTION:
  
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
      CustomDialog dsB("Show Bottom Contours", this);
      
      int static minViewR = plug.seedView;
      int static maxViewR = plug.seedView + 5;
      
      dsB.addLabel   ( "... select range of views where top \n"
                       "and bottom fiducials appear to move in \n"
                       "opposite directions (suggest a range of ~5):" );
      dsB.addSpinBox ( "min view:", 1, plug.zsize, &minViewR, 1 );
      dsB.addSpinBox ( "max view:", 1, plug.zsize, &maxViewR, 1 );
      dsB.exec();
      if( dsB.wasCancelled() )
        return;
      
      if( minViewR >= maxViewR )
      {
        MsgBox( "Bad range given !" );
        return;
      }
      
      bead_showBottomContoursInPurple( minViewR, maxViewR );
    } break;
    
    case(2):      // show contour turning points
      bead_showContourTurningPts();
      break;
    
    case(3):      // show grid
      bead_showGrid();
      break;
      
    case(4):      // clear purple object
      ivwClearAnExtraObject(plug.view, plug.extraObjExtra);
      break;
    
    case(5):      // move points between objects
      moveMultipleContours();
      break;
      
    case(6):      // move points between objects
      splitOrMergeContours();
      break;
      
    case(7):      // remove duplicate pts from object
      correctCurrentObject();
      break;
    
    case(8):      // mark all contours as stippled/unstippled
      markRangeAsStippled();
      break;
    
    case(9):      // mark all points as checked/unchecked
      markRangePtsAsChecked();
      break;
      
    case(10):      // print contour info
      printContourCheckedInfo();
      break;
    
    case(11):      // load tilt angles
    
      openTiltAngleFile();
      break;
      
    case(12):     // reset values
      if( !MsgBoxYesNo(this, "Restore all default settings for this plugin?" ) )
        return;
      plug.window->initValues();
      wprint("Default values restored\n");
      break;
  }
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Allows user to change other plugin values/settings.

void BeadHelper::moreSettings()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds("More Settings", this);
  
  float newTiltIncrement = plug.tiltIncrement;
  float newTiltAxisAngle = plug.tiltAxisAngle;
  int tiltOffsetX = plug.tiltOffsetX;
  int sizeCheckedPts = plug.sizeCheckedPts;
  int seedView = plug.seedView+1;
  int biggestHoleInset = plug.biggestHoleInset;
  int biggestHoleGrid = plug.biggestHoleGrid;
  
  ds.addLabel   ("----- TILT STACK: -----", true);
  
  ds.addSpinBox ( "seed view:",
                  1, plug.zsize+1, &seedView, 1,
                  "The view used to seed points - this is usually "
                  "the middle-most view, but not in all cases \n"
                  "NOTE: This settings is not saved on exit \n\n"
                  "NOTE: You'll often have to reduce this value by 1 on b axis stacks" );
  ds.addLineEditF( "tilt increment:           ", INT_MIN, INT_MAX, &newTiltIncrement, 5,
                  "The angle (in degrees) the specimen was tilted "
                  "between subsequent views \n"
                  "NOTE: This value becomes irrelevant after loading the calculated "
                  "tilt angles via 'More Actions' > 'load tilt angles'" );
  ds.addLineEditF( "tilt axis angle:           ", INT_MIN, INT_MAX, &newTiltAxisAngle, 5,
                  "The angle (in degrees) clockwise from "
                  "vertical about which the views rotate \n"
                  "NOTE: This can be calculated in "
                  "'More Actions' > 'calculate tilt axis angle'");
  ds.addSpinBox ( "tilt x offset:", -200, 200, &tiltOffsetX, 1,
                  "How far the tilt axis is shifted along X "
                  "from passing through the center" );
  ds.addSpinBox (  "biggest hole grid size:", 1, 1000, &biggestHoleGrid, 5,
                   "The distance between grid points used to search \n"
                   "for the point furthest from any seed points and \n"
                   "the edge of the biggest hold grid\n\n"
                   "Set 'tilt display' to '[h] grid' to see the effect");
  ds.addSpinBox ( "biggest hole grid inset:",
                  -10000, (plug.xsize/2)+24, &biggestHoleInset, 10,
                  "The distance (in pixels) to inset the "
                    "biggest hole grid [h] such that: \n"
                  "  > a large value will concentrate points in the middle and \n"
                  "  > a negative value encourages points towards the edge "
                  "as you press 'h'.\n\n"
                  "Set 'tilt display' to '[h] grid' to see the effect");
  
  ds.addLabel   ("\n----- DISPLAY: -----", true);
  
  ds.addComboBox( "estimated pt display:",
                  "cross|"
                  "diamond|"
                  "arrow", &plug.expPtDisplayType,
                  "Symbol used to display the estimated point" );
  ds.addSpinBox ( "estimated pt size:",
                  1, 200, &plug.expPtSize, 1,
                  "The size of the estimated point symbol in screen pixels" );
  ds.addSpinBox ( "line display spheres:",
                  0, 50, &plug.sizeLineSpheres, 1,
                  "The size of points used to display contours as lines" );
  ds.addSpinBox ( "line display width:",
                  1, 50, &plug.lineDisplayWidth, 1,
                  "The thickness of lines used to display "
                  "contours as lines" );
  ds.addSpinBox ( "size purple spheres:",
                  1, 200, &plug.sizePurpleSpheres, 1,
                  "The size of points in the extra purple object "
                  "(used to show bottom fiducials etc)" );
  ds.addSpinBox ( "size checked pts:",
                  1, 20, &sizeCheckedPts, 1,
                  "The size of points which have been checked by pressing [U]" );
  
  ds.addLabel   ("\n----- OTHER: -----", true);
  
  ds.addCheckBox( "save settings on close", 
                  &plug.autoSaveSettings,
                  "Automatically saves all your Bead Helper "
                  "settings to 'beadhelpersettings.txt' "
                  "when you close 3dmod, so they will load "
                  "next time you open 3dmod");
  
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  plug.tiltOffsetX       = tiltOffsetX;
  plug.seedView          = seedView - 1;
  plug.sizeCheckedPts   += 0.1f;
  plug.biggestHoleInset  = biggestHoleInset;
  plug.biggestHoleGrid   = biggestHoleGrid;
  plug.sizeCheckedPts    = sizeCheckedPts;
  
  
  //float newTiltAxisAngle = string_getFloatFromString( tiltAxisAngleStr );
  if( newTiltAxisAngle < -200 || newTiltAxisAngle >200 )
    wprint("\aERROR: Invalid tilt axis angle entered"); 
  else
    plug.tiltAxisAngle = newTiltAxisAngle;
  
  
  //float newTiltIncrement = string_getFloatFromString( tiltIncrementStr );
  if( newTiltIncrement <= 0 || newTiltIncrement >20 )
    wprint("\aERROR: Invalid tilt increment entered"); 
  else
  {
    if(  plug.tiltIncrement != newTiltIncrement);
    {
      plug.tiltIncrement = newTiltIncrement;
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
  
  int wheelResistance = plug.wheelResistance;
  
	CustomDialog ds("Mouse and Keyboard Settings", this);
  
  ds.addLabel   ("----- MOUSE: -----", true);
  
  ds.addComboBox( "wheel behavior:",
                  "none|"
                  "scroll points|"
                  "scroll views|"
                  "smart scroll", &plug.wheelBehav,
                  "Allows you to use the mouse wheel to "
                  "scroll through points and/or views" ); 
  ds.addSpinBox ( "wheel resistance:",
                  10, 1000, &wheelResistance, 10,
                  "The higher the value, the slower mouse scrolling works"
                  "(useful if you have a touchy mouse wheel)" );
  ds.addCheckBox( "center point on scroll", 
                  &plug.centerPtOnScroll,
                  "Keeps ZAP centered on the point or estimated pos \n"
                  "as you scroll with the mouse wheel");
  
  ds.addLabel   ("----- KEYBOARD: -----", true);
  
  ds.addCheckBox( "disable all hot keys", 
                  &plug.disableHotKeys,
                  "Disables all BeadHelper hot keys in case "
                  "they conflict with another key you "
                  "need to press \n\nNOTE: this setting "
                  "defaults to off and is not saved on exit");
  ds.addCheckBox( "include seed && end pts on [y],[b]&&[w]", 
                  &plug.includeEndsResid,
                  "Includes end points when searching for the "
                  "point with the next biggest y jump "
                  "or deviation from expected when "
                  "[y], [b] or [w] is pressed"
                  "\n\nRECOMMENDED VALUE: off");
  ds.addCheckBox( "on [y]&&[b] search view range only", 
                  &plug.searchRangeOnly,
                  "Only searches points within the specified "
                  "range of views (in the 'Range' box above) "
                  "when searching for the "
                  "point with the next biggest y jump [y] "
                  "or deviation from expected [b] "
                  "\n\nNOTE: this value is not saved on exit "
                  "and defaults to off");
  ds.addComboBox( "[y],[b]&[o] searches: ",
                  "all contours|"
                  "unchecked only|"
                  "checked only", &plug.contsToSearch,
                  "Which contours are searched when [y] "
                  "(biggest y jump), [b] (biggest deviation "
                  "from expected) or [o] (worst contour) "
                  "keys are pressed");
  
  ds.addCheckBox( "[w] checks current contour only",
                  &plug.wCurrContOnly,
                  "If true: finds biggest weighted value "
                  "in current contour. If false: searches "
                  "all contours in current object.");
  ds.addSpinBox ( "[w] divisor number:",
                  1, 500, &plug.wWeightedDiv, 5,
                  "The lower this number, the more likely "
                  "to select small shifts \namoungs closely "
                  "placed points.... \nuses the formula: "
                  "weighted_dev = distance_to_expected_pt / "
                  "(distance_nearest_pts + wWeightedDiv)"
                  "\n\nRECOMMEDED VALUE: 15" );
  
  ds.addComboBox( "on [d] delete pts:",
                  "do nothing|"
                  "opposite seed|"
                  "to nearest end|"
                  "specified range", &plug.dKeyBehav,
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
  
  ds.addComboBox( "on [m]:",
                  "normal|"
                  "go to middle pt|"
                  "smooth local|"
                  "smooth y local",
                  &plug.mKeyBehav,
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
  
  ds.addComboBox( "on [U]:",
                  "print object info|"
                  "check current pt|"
                  "check contour pts",
                  &plug.uKeyBehav,
                  "Action performed when [U] is pressed."
                  "\n"
                  "\n > print object info - prints info about "
                  "how many contours are checked etc "
                  "(same as [I])"
                  "\n > check current pt  - toggles currently "
                  "selected point between checked and unchecked"
                  "\n > check contour pts - toggles all points "
                  "in current contour between checked and "
                  "unchecked"
                  "\n NOTE: a checked point is shown by a "
                  "small sphere" );
  
  ds.addLabel   ("");
  
  ds.addComboBox( "on [Enter] go to:",
                  "do nothing|"
                  "next unchecked|"
                  "prev uncheced|"
                  "next checked|"
                  "next contour", &plug.enterAction,
                  "Action performed when [Enter] is pressed "
                  "\n\nNOTE: If you don't use enter you may, "
                  "wish to leave this on 'do nothing' because "
                  "it can interfer when you press enter in "
                  "other windows/plugins");
  ds.addSpinBox  ( "min points for [Enter]:", 
                   0, plug.zsize+10, &plug.minPtsEnter, 1,
                   "The minimum number of points a contour "
                   "must have to be jumped to when [Enter] "
                   "is pressed" );
  ds.addSpinBox  ( "max points for [Enter]:", 
                   1, plug.zsize+10, &plug.maxPtsEnter, 1,
                   "The maximum number of points a contour "
                   "must have to be jumped to when [Enter] "
                   "is pressed"
                   "\n\nNOTE: Unlike 'min points' this value is "
                   "not saved on exit and instead defaults to "
                   "the number of views" );
  ds.addCheckBox ( "print results on [Enter]", &plug.enterPrint,
                   "Prints the number of contours matching the "
                   "above criteria each time enter is pressed" );
  
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  plug.wheelResistance = wheelResistance;
  
  
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
  if( !verifyCurrObjectAndUpdateRanges() )
    return;
  
  int nConts = csize(getCurrObj());
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool reverseOrder = false;
  static bool printVals    = true;
  static bool calcValsOnly = false;
  
	CustomDialog ds("Sorting Options", this);
  ds.addLabel   ( "contours to sort (inclusive):" );
  ds.addSpinBox ( "  min:", 1, nConts, &plug.contMinN, 1,
                  "Only contours after this contour "
                  "(inclusive) will be reordered" );
  ds.addSpinBox ( "  max:", 1, nConts, &plug.contMaxN, 1,
                  "Only contours BEFORE this contour "
                  "(inclusive) will be reordered" );
	 ds.addRadioGrp( "sort by:         (sort criteria)",
                   "y jumps (asc)|"
                   "deviation (asc)|"
                   "avg grey value (desc)|"
                   "dist from middle (asc)|"
                   "num missing pts (asc)|"
                   "checked first|"
                   "random",
                   &plug.sortCriteria,
                   "",
                   "Sorts based on how far points jump in Y|"
                   "Uses a weighted score of how far points are "
                   "from their estimated positions|"
                   "Average the grey value closest to each point|"
                   "The distance of the seed point from the "
                   "dead center of the tomogram in X an Y|"
                   "Contours with the least points will be moved "
                   "to the end|"
                   "Moves checked (stippled) contours to the start "
                   "and unchecked to the end|"
                   "Uses a random number for each contour" );
   ds.addCheckBox( "calc values only (don't reorder)",
                   &calcValsOnly,
                   "No contours will be reordered, but "
                   "values will be calculated and you "
                   "can iterate from largest to smallest "
                   "by pressing 'o'" );
	 ds.addCheckBox( "reverse order", &reverseOrder );
   ds.addCheckBox( "print values",  &printVals );
   
   ds.exec();
	if( ds.wasCancelled() )
		return;
  
  if( !verifyAndUpdateEnteredRangeValues() )
    return;
  
  bead_reorderConts( plug.sortCriteria, plug.contMin, plug.contMax, calcValsOnly,
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
  int newContIdx = QInputDialog::getInteger(this, "Move contour", "Move to:",
                                            lastCont, 1, lastCont, 1, &ok) - 1;
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
  if ( !verifyCurrObjectAndUpdateRanges() )
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
  
  int         objToIdx     = numObjs;
  static bool seedOnly     = false;
  static bool deleteMatch  = false;
  static bool copy         = false;
  
	CustomDialog ds("Move Options", this);
  ds.addSpinBox ( "min cont:", 1, maxContIdx+1, &plug.contMinN, 1 );
  ds.addSpinBox ( "max cont:", 1, maxContIdx+1, &plug.contMaxN, 1 );  
	ds.addSpinBox ( "move to object:", 1, numObjs, &objToIdx, 1 );
	ds.addCheckBox( "seed pt only", &seedOnly );
  ds.addCheckBox( "delete matching seeds", &deleteMatch );
  ds.addCheckBox( "copy", &copy );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  if( !verifyAndUpdateEnteredRangeValues() )
    return;
  objToIdx -= 1;
  
  
  if ( objToIdx == objIdx &&
       !MsgBoxYesNo(this, "You are copying to the same object.... are you SURE?!") )
    return;
  
  //## FOR EACH CONTOUR IN RANGE: ADD CONTOUR OR SEED TO THE END OF THE DESIGNATED
  //## OBJECT UNLESS IT MATCHES A SEED CURRENTLY IN THAT OBJECT
  
  int numMatchingSeeds = 0;
  int numContsCopied = 0;
  Iobj *objTo = getObj(imod,objToIdx);
  
  for(int c=plug.contMin; c<=plug.contMax; c++)
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
  for(int c=plug.contMax; c>=plug.contMin; c--)   // for all contours in range:
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
//-- Splits all the contours in the model into multiple
//-- objects, each with N objects.... or merges them into
//-- the first object.

void BeadHelper::splitOrMergeContours()
{
         bool mergeOnly          = false;
  static int  numContsPerObj     = 10;
  static int  numContsToLeave    = 0;
  static bool randomizeOrder     = false;
  
	CustomDialog ds("Split Contours Options", this);
  ds.addCheckBox( "MERGE ALL CONTOUR INTO FIRST OBJECT ONLY", &mergeOnly,
                  "Will ignore the value immediately below and move all contours "
                  "into object 1" );
  ds.addSpinBox ( "number of contours per object:", 1, 1000, &numContsPerObj, 5,
                  "The number of contours to place in each object" );
  ds.addLabel   ( "-----", true );
  ds.addCheckBox( "randomize contour order:", &randomizeOrder,
                  "Will randomize contours (excluding the range below) before splitting "
                  "them between objects" );
  ds.addSpinBox ( "contours to leave at the start:", 0, 9999, &numContsToLeave, 1,
                  "This many contours at the start of object 1 will not be moved "
                  "or reordered" );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  Imod *imod = ivwGetModel(plug.view);
  imodSetIndex(imod, 0, 0, 0);          // select first object, first contour
  
  //## COPY ALL CONTOURS TO THE FIRST OBJECT
  
  Iobj *firstObj = getObj(imod,0);
  
  for( int o=1; o<osize(imod); o++ )
  {
    Iobj *obj = getObj(imod,o);
    
    while( csize(obj) )
    {
      Icont *cont = getCont(obj,0);
      //imodObjectAddContour(firstObj, cont );
      edit_addContourToObj( firstObj, cont, false );
      setDeleteFlag(cont,1);
      //undoContourRemoval( plug.view, o, 0 );         // REGISTER UNDO
      //imodObjectRemoveContour( obj, 0 );
    }
  }
  
  int nConts = csize(firstObj);
  int nObjectsNeeded = ceil( fDiv(nConts, numContsPerObj) );
  int nObjectsToAdd  = nObjectsNeeded - osize(imod);
  
  cout << "nConts         =" << nConts << endl;
  cout << "nObjectsNeeded =" << nObjectsNeeded << endl;
  cout << "osize(imod)    =" << osize(imod) << endl;
  cout << "nObjectsToAdd  =" << nObjectsToAdd << endl;
  flush(cout);
  
  //## IF SPECIFIED, REORDER CONTOURS RANDOMLY:
  
  if( randomizeOrder )
    bead_reorderConts( SORT_RANDOM, numContsToLeave, 9999, false, false, false );
  
  if(mergeOnly)
  {
    wprint("\nAll %d contours have been moved to object 1\n", nConts );
    //undoFinishUnit( plug.view );              // FINISH UNDO
    ivwRedraw( plug.view );
  }
  
  //## ADD EXTRA OBJECTS:
  
  
  
  for( int o=0; o<nObjectsToAdd; o++ )
  {
    undoObjectAddition(plug.view, osize(imod));       // REGISTER UNDO
    imodNewObject( imod );
    Iobj *newObj = getObj(imod, osize(imod)-1);
    imodObjectSetValue( newObj, IobjFlagClosed, 0 );
    imodObjectSetValue( newObj, IobjPointSize,  3 );
  }
  
  //cout << "osize(imod)    =" << osize(imod) << endl;
  //flush(cout);
  
  //## FOR EACH OBJECT AFTER FIRST OBJECT, CLEAR ALL CONTOURS:
  
  for( int c=0; c<csize(firstObj); c++ )
  {
    Icont *cont = getCont(firstObj,c);
    setDeleteFlag( cont, 0 );
    int destObjIdx = int( fDiv(c,numContsPerObj) ) % osize(imod);
    
    if(destObjIdx != 0)
    {
      Imod *objDest = getObj(imod,destObjIdx);
      //undoContourMove( plug.view, 0, c, destObjIdx, osize(objDest) );         // REGISTER UNDO
      edit_addContourToObj( objDest, cont, false);
      
      //imodObjectAddContour(objDest, cont );
      //undoContourRemoval( plug.view, o, 0 );         // REGISTER UNDO
      //imodObjectRemoveContour( firstObj, c );
      setDeleteFlag( cont, 1 );
    }
  }
  
  //edit_removeAllDeleteFlaggedContoursFromObj( firstObj, false );
  
  for( int o=0; o<osize(imod); o++ )
    edit_removeAllDeleteFlaggedContoursFromObj( getObj(imod,o), false );
  
  /*while( csize(firstObj) > nObjectsToAdd )
  {
    
  }*/
  
  /*
  for( int o=1; o<osize(imod); o++ )
  {
    Iobj *obj = getObj(imod,o);
    while( csize(obj) )
    {
      undoContourRemoval( plug.view, o, 0 );         // REGISTER UNDO
      imodObjectRemoveContour( obj, 0 );
    }
  }
  */
  
  //## MOVE SELECT CONTOURS FROM THE FIRST OBJECT TO APPROPRIATE OBJECTS:
  /*
  imodSetIndex(imod, 0, 0, 0);          // select first object, first contour
  for( int c=csize(firstObj)-1; c>=numContsToLeave; c-- )
  {
    Icont *cont = getCont(firstObj,c);
    int destObjIdx = c % osize(imod);
    //int destObjIdx = (c / numContsPerObj);
    //cout << destObjIdx << endl;
    
    if(destObjIdx != 0)
    {
      //Imod *objDest = getObj(imod,destObjIdx);
      //Icont *contCopy = imodContourDup( cont );   // create copy of contour (don't delete)
      //undoContourAddition( plug.view, destObjIdx, osize(objDest) );              // REGISTER UNDO
      //imodObjectAddContour( objDest, contCopy );                    // copy contour
      //free(contCopy);
      Imod *objDest = getObj(imod,destObjIdx);
      imodObjectAddContour(objDest, cont );
      //undoContourRemoval( plug.view, 0, c );         // REGISTER UNDO
      imodObjectRemoveContour( firstObj, c );
      //imodDeleteContour( imod, c );
      
      //imodSetIndex(imod, destObjIdx, 0, 0);          // select first object, first contour
      //Imod *objDest = getObj(imod,destObjIdx);
      //edit_addContourToObj( objDest, cont, false );
      //undoContourRemoval( plug.view, 0, c );         // REGISTER UNDO
      //imodObjectRemoveContour( firstObj, c );
    }
  }
  */
  
  wprint("\n%d contours have been split across %d objects\n", nConts, nObjectsNeeded );
  
  undoFinishUnit( plug.view );              // FINISH UNDO
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
  
  static bool deleteExtraPts   = true;
  static bool deleteMatchSeeds = true;
  static bool correctPtOrder   = true;
  
	CustomDialog ds("Correction Options", this);
	ds.addCheckBox( "delete duplicate pts on same view", &deleteExtraPts,
                  "If two (or more) points lie on the same view \n"
                  "of the same contour delete the second" );
  ds.addCheckBox( "delete duplicate seeds", &deleteMatchSeeds,
                  "If two (or more) contours are on the same fiducial \n"
                  "on the seed view delete the second one" );
  ds.addCheckBox( "correct bad point order", &correctPtOrder,
                  "If you somehow managed to put contour point in \n"
                  "the wrong order this will reorder the points in \n"
                  "ascending Z value" );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
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
//-- Toggles the current contour's points as checked
//-- (by changing the size of spheres)

void BeadHelper::togglePtsChecked()
{
  if( !isCurrPtValid() )
    return;
  
  wprint("POINT\n");
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  undoContourDataChgCC( plug.view );        // REGISTER UNDO
  
  if( plug.uKeyBehav == UK_TOGGLEPTCHECKED )
  {
    if( bead_isPtChecked(obj,cont,ptIdx) )                    // if pt checked:
      removePtSize( cont, ptIdx );
    else                                                      // else:
      imodPointSetSize( cont, ptIdx, plug.sizeCheckedPts );     // check point
  }
  else if( plug.uKeyBehav == UK_TOGGLEALLPTSCHECKED )
  {
    bool allPtsChecked = true;
    for( int p=0; p<psize(cont); p++ )
      if( !bead_isPtChecked(obj,cont,p) )
      {
        allPtsChecked = false;
        break;
      }
    
    if( allPtsChecked )
    {
      for( int p=0; p<psize(cont); p++ )
        removePtsSize( cont );
    }
    else
    {
      for( int p=0; p<psize(cont); p++ )
        imodPointSetSize( cont, p, plug.sizeCheckedPts );
    }
  }
  
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
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  //## PRINT THE NUMBER OF CONTOURS MATCHING THE GIVEN CRITERIA:
  
  if( plug.enterPrint )
  {
    Iobj *obj   = imodObjectGet(imod);
    int matches = 0;
    for( int i=0; i<csize(obj); i++ )
    {
      Icont *cont = getCont(obj,i);
      if(      ( psize(cont) >= plug.minPtsEnter )
            && ( psize(cont) <= plug.maxPtsEnter )
            && ( anyCont || isInterpolated(cont) == stippledCont ) )
        matches++;
    }
    wprint("%d matching contours left (this object)\n", matches);
  }
  
  //## ITERATE FORWARDS OR BACKWARDS FROM CURRENT CONTOUR AND SELECT THE NEXT
  //## CONTOUR MATCHING THE ENTER CRITERIA:
  
  for( int j=0; j<osize(imod); j++ ) 
  {
    int o = intMod( j*change+objIdx, osize(imod) );
    Iobj *obj = getObj(imod,o);
    
    for( int i=1; i<csize(obj); i++ )
    {
      int c = intMod( i*change+contIdx, csize(obj) );
      
      Icont *cont = getCont(obj,c);
      
      if(      ( psize(cont) >= plug.minPtsEnter )
               && ( psize(cont) <= plug.maxPtsEnter )
               && ( anyCont || isInterpolated(cont) == stippledCont ) )
      {
        ptIdx = MIN( ptIdx, psize(cont)-1 );
        ptIdx = MAX( ptIdx, 0 );
        imodSetIndex( imod, o, c, ptIdx );
        plug.window->drawExtraObject(true);
        ivwRedraw( plug.view );
        return true;
      }
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
  if( !verifyCurrObjectAndUpdateRanges() )
    return;
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int checked = 1;
  
	CustomDialog ds("Mark Contours as Checked", this);
  ds.addLabel   ( "-- RANGE: --", true );
  ds.addLabel   ( "contours to change:" );
  ds.addSpinBox ( "  min:", 1, nConts, &plug.contMinN, 1,
                  "Only contours after this contour (inclusive) will be changed" );
  ds.addSpinBox ( "  max:", 1, nConts, &plug.contMaxN, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addRadioGrp( "mark as:",
                  "unchecked (unstippled)|"
                  "checked (stippled)", &checked );
  ds.addLabel   ( "NOTE: Use [Enter] to iterate through \n"
                  "unchecked (unstippled) contours and \n"
                  "[u] to toggle current contour between \n"
                  "checked and unchecked" );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  if( !verifyAndUpdateEnteredRangeValues() )
    return;
  
  
  //## CHANGE CONTOURS IN RANGE TO STIPPLED / UNSTIPPLED
  
  int numChanged = 0;
  
  for( int c=plug.contMin; c<=plug.contMax && c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj,c);
    if( isInterpolated(cont) != checked )
    {
      undoContourPropChg( plug.view, objIdx, c );        // REGISTER UNDO
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
//-- Toggles the current contour's stippled flag

void BeadHelper::markRangePtsAsChecked()
{
  if( !verifyCurrObjectAndUpdateRanges() )
    return;
  
  int nConts = csize(getCurrObj());
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int checked  = 1;
  
	CustomDialog ds("Mark Points as Checked", this);
  ds.addLabel   ( "RANGE:", true );
  ds.addLabel   ( "contours to change:" );
  ds.addSpinBox ( "  min:", 1, nConts, &plug.contMinN, 1,
                  "Only contours after this contour (inclusive) will be reordered" );
  ds.addSpinBox ( "  max:", 1, nConts, &plug.contMaxN, 1,
                  "Only contours BEFORE this contour (inclusive) will be reordered" );
  ds.addLabel   ( "views to change:" );
  ds.addSpinBox ( "  min:", 1, plug.zsize, &plug.viewMinN, 1,
                  "Only pts on view after this (inclusive) will be changed" );
  ds.addSpinBox ( "  max:", 1, plug.zsize, &plug.viewMaxN, 1,
                  "Only pts on view BEFORE this (inclusive) will be changed" );
  ds.addLabel   ( "------", true );
  ds.addRadioGrp( "mark as:",
                  "unchecked (no sphere)|"
                  "checked   (little sphere)", &checked );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  if( !verifyAndUpdateEnteredRangeValues() )
    return;
  
  //## CHANGE POINTS IN RANGE TO CHECKED / UNCHECKED
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int numChanged = 0;
  
  for( int c=plug.contMin; c<=plug.contMax && c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj,c);
    for(int p=0; p<psize(cont); p++)
    {
      int z = getPtZInt(cont,p);
      if( z > plug.viewMax || z < plug.viewMin )
        continue;
      
      int ptChecked = ( bead_isPtChecked(obj,cont,p) ) ? 1 : 0;
      if( ptChecked != checked )   // if point needs to change:
      {
        undoContourDataChg( plug.view, objIdx, c );   // REGISTER UNDO
        if(checked)
          imodPointSetSize( cont, p, plug.sizeCheckedPts );
        else
          removePtSize( cont, p );
        numChanged++;
      }
    }
  }
  if( numChanged )
    undoFinishUnit( plug.view );              // FINISH UNDO
  
  wprint("%d points changed\n", numChanged);
	plug.window->drawExtraObject(true);
}


//------------------------
//-- Returns false if the current object is invalid or empty.
//-- Otherwise returns true and updates the range values to
//-- match the current object, and ensure all range values are valid.

bool BeadHelper::verifyCurrObjectAndUpdateRanges()
{
  //## CHECK CURRENT OBJECT IS VALID AND CONTAINS CONTOURS:
  
  if( !isCurrObjValid() )
  {
    MsgBox("You have not selected a valid object");
    return false;
  }
  
  int nCont = csize( getCurrObj() );
  
  if( nCont == 0 )
  {
    MsgBox("Selected object has no contours");
    return false;
  }
  
  //## UPDATE RANGE VALUES:
  
  if( plug.contMinN > nCont )
    plug.contMinN = nCont;
  
  plug.contMinN = MAX( plug.contMinN, 1 );
  plug.contMaxN = nCont;              // set max contour to last contour
  
  plug.viewMinN = MAX( plug.viewMinN, 1 );
  plug.viewMaxN = MIN( plug.viewMaxN, plug.zsize );
  
  if( plug.contMinN > plug.contMaxN )
    plug.contMaxN = plug.contMinN;
  
  if( plug.viewMinN > plug.viewMaxN )
    plug.viewMaxN = plug.viewMinN;
  
  return verifyAndUpdateEnteredRangeValues();
}


//------------------------
//-- Updates the range values and corrects any returns true if a 
//-- valid contour and view range has been provided.
//-- Otherwise displays error message and returns false.

bool BeadHelper::verifyAndUpdateEnteredRangeValues()
{
  if( !isCurrObjValid() )
    return false;
  
  int nCont = csize( getCurrObj() );
  bool rangeIsGood = true;
  
  if(    !isBetweenAsc( 1, plug.contMinN, nCont )
      || !isBetweenAsc( 1, plug.contMinN, nCont ) 
      || plug.contMinN > plug.contMaxN )
  {
    MsgBox("\aBad range of contours was entered");
    plug.contMinN = 1;
    plug.contMaxN = nCont;
    rangeIsGood = false;
  }
  
  if(    !isBetweenAsc( 1, plug.viewMinN, plug.zsize )
      || !isBetweenAsc( 1, plug.viewMaxN, plug.zsize ) 
      || plug.viewMinN > plug.viewMaxN )
  {
    MsgBox("\aBad range of views was entered");
    plug.viewMinN = 1;
    plug.viewMaxN = plug.zsize;
    rangeIsGood = false;
  }
  
  plug.viewMin = plug.viewMinN - 1;
  plug.viewMax = plug.viewMaxN - 1;
  plug.contMin = plug.contMinN - 1;
  plug.contMax = plug.contMaxN - 1;
  
  return (rangeIsGood);
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
//-- Prints contour stats for the each object in the model including: 
//-- the number of checked, non-checked and single-point contours.

void BeadHelper::printContourCheckedInfo()
{
  Imod *imod  = ivwGetModel(plug.view);
  bool multipleObjects = osize(imod) > 1;
  
  int totMissingPts = 0;
  int totEmptyC     = 0;
  int totSinglePtC  = 0;
  int totCheckedC   = 0;
  int totFullC      = 0;
  int totPartialC   = 0;
  int totConts      = 0;
  
  for( int o=0; o<osize(imod); o++ )
  {
    Iobj *obj = getObj(imod,o);
    int objCheckedC   = 0;
    int objFullC      = 0;
    
    for( int c=0; c<csize(obj); c++ )
    {
      Icont *cont   = getCont(obj,c);
      
      int numPts = psize(cont);
      
      if     ( numPts == 0 )            totEmptyC += 1;
      else if( numPts == 1 )            totSinglePtC += 1;
      else if( isInterpolated(cont) )   objCheckedC += 1;
      else if( numPts == plug.zsize )   objFullC += 1;
      else                              totPartialC += 1;
      
      totMissingPts += plug.zsize - numPts;
    }
    
    totCheckedC += objCheckedC;
    totFullC    += objFullC;
    totConts    += csize(obj);
    
    if( multipleObjects )
    {
      if(o==0)
        wprint("\nOBJ \tCONTS \tFULL \tCHECKED\n");
      wprint("%d \t%d \t%d \t%d\n", o+1, csize(obj), totFullC, totCheckedC );
    }
  }
  
  float avgMissingPtsPerCont = fDiv( totMissingPts, totConts );
  int   percentSinglePt      = calcPercentInt( totSinglePtC,  totConts  );
  int   percentChecked       = calcPercentInt( totCheckedC,   totConts  );
  int   percentFull          = calcPercentInt( totFullC,      totConts  );
  int   percentPartial       = calcPercentInt( totPartialC,   totConts  );
  
  wprint("\nCONTOUR INFO:\n");
  wprint(" total contours  = %d\n", totConts );
  if(totEmptyC)
    wprint("  # empty   \t= %d\n", totEmptyC );
  wprint("  # single pt \t= %d \t(%d%%)\n", totSinglePtC,  percentSinglePt );
  wprint("  # partial   \t= %d \t(%d%%)\n", totPartialC,   percentPartial );
  wprint("  # full           \t= %d \t(%d%%)\n", totFullC, percentFull  );
  wprint("  # checked   \t= %d \t(%d%%)\n", totCheckedC,   percentChecked );
  wprint(" total missing pts = %d\n", totMissingPts );
  wprint(" avg missing pts/cont = %g\n", avgMissingPtsPerCont );
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
  float maxTilt = (plug.zsize - plug.seedView - 1) * plug.tiltIncrement;
  
  bool tiltAppearsGood =   ( minTilt >= -80  &&  minTilt <= -10 )
                        && ( maxTilt >=  10  &&  maxTilt <=  80 );
  
  if( printResult )
  {
    string str = "\nTilt increment = " + toString(plug.tiltIncrement,2) + DEGREE_SIGN + 
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
  const char *filter[] = {"Tilt angle files (*.rawtlt;*.tlt)",""};
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
    wprint( " %d \t %g", i+1, plug.tiltAngles[i] );
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


//############################################################
//## PROTECTED SLOTS:


//------------------------
//-- Displays a (html) help page with information about the plugin

void BeadHelper::helpPluginHelp()
{
  imodShowHelpPage("../plughelp/beadhelper.html#TOP");
}


//------------------------
//-- Callback for the buttons at the bottom

void BeadHelper::buttonPressed(int which)
{
  if      (which==0)
    close();
  else if (which==1)
		openUrl( "http://www.slashsegmentation.com/tools/imod/bead-helper-plugin" );
	else if (which==2)
    helpPluginHelp();
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

int edit_removeAllDeleteFlaggedContoursFromObj( Iobj *obj, bool enableUndo )
{
	int numRemoved = 0;
	for( int c=csize(obj)-1; c>=0; c-- )
	{
    Icont *cont = getCont(obj, c);
		if( isDeleteFlag( cont ) && isInterpolated( cont ) )
		{
      if(enableUndo)
        undoContourRemovalCO( plug.view, c );              // REGISTER UNDO
			imodObjectRemoveContour( obj, c );
			numRemoved++;
		}
	}
	return numRemoved;
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
  //imodObjectRemoveContour( obj, tempContIdx );
  ivwSetLocation(plug.view, x, y, z);
  imodSetIndex(imod, objIdx, -1, 0);
  imodObjectRemoveContour( obj, tempContIdx );
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
//-- Returns true if the given point appears checked.

bool bead_isPtChecked( Iobj *obj, Icont *cont, int ptIdx )
{
  float ptSize = imodPointGetSize(obj,cont,ptIdx);
  return (ptSize == plug.sizeCheckedPts);
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
  float fractToExpectedPos = fDiv( (z - pt1->z) , ( pt2->z - pt1->z) ); 
  estPt = line_findPtFractBetweenPts( pt1, pt2, fractToExpectedPos );
  estPt.z = z;
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
  
  //float xDistBetweenPtsNorm = ABS(fDiv( (pt2->x - pt1->x) , ( pt2->z - pt1->z) )); 
  
  float angle1 = bead_getTiltAngleAtZ( pt1->z+0.5 );
  float angle2 = bead_getTiltAngleAtZ( pt2->z+0.5 );
  float angleZ = bead_getTiltAngleAtZ( z );
  
  float adjSide1  = cosf( DEGS_TO_RADS * angle1 );
  float adjSide2  = cosf( DEGS_TO_RADS * angle2 );
  float adjSideZ  = cosf( DEGS_TO_RADS * angleZ );
  // calculate the relative length of an adjacent side for each point
  // implied by the tilt angle at that Z value
  
  float fractToExpectedPos = fDiv((adjSideZ - adjSide1), ( adjSide2 - adjSide1)); 
  if( (adjSide2 == adjSide1) ||     // if length of both adjacent sides is equal
      (angle1<0 && angle2>0) ||     //  or one angle is positive and
      (angle1>0 && angle2<0) )      //  the other is negeative:
    fractToExpectedPos = fDiv( (z - pt1->z) , ( pt2->z - pt1->z) );
  
  estPt = line_findPtFractBetweenPts( pt1, pt2, fractToExpectedPos );
  estPt.z = z;
  
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
  
  estPt.z = (float)view;
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
    distXNorm.push_back( fDiv((pt->x - ptNext->x), ( pt->z - ptNext->z )) );
  }
  
  //## DETERMINE THE AVERAGE RATE AT WHICH THE DISTANCE IS INCREASING/DECREASING:
  
  float totalIncreaseXNorm = 0;
  for( int i=0; i<distXNorm.size()-1; i++ )
  {
    float increaseXFromLastNorm = ( distXNorm[i] - distXNorm[i+1] );
    totalIncreaseXNorm += increaseXFromLastNorm;
  }
  float avgIncreaseXNorm = fDiv( totalIncreaseXNorm, distXNorm.size()-1 );
  
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
  if( psize(cont)>0 && (int)pt->z < getPtZInt(cont,0) )
  {
    imodPointAdd(cont,pt,0);
    return 0;
  }
  
  for( int p=0; p<psize(cont); p++ )
  {
    if ( getPtZInt(cont,p) == (int)pt->z ) {
      *getPt(cont,p) = *pt;
      return p;
    }
    else if( getPtZInt(cont,p) > (int)pt->z ) {
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
  int minZ = plug.viewMinN-1;
  int maxZ = plug.viewMaxN-1;
  
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
  for( int zz=middleZ-1; zz>=minZ; zz-- )
  {
    if( bead_insertPtAtEstimatedPos(cont,zz,false) )
      pointsAdded++;
  }
  
  return pointsAdded;
}



//------------------------
//-- Deletes all points within the specified range of z values and returns
//-- the number of points deleted.

/*int bead_deletePtsInZRange( Icont *cont, int minZ, int maxZ, bool inclusive )
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
}*/


//------------------------
//-- Deletes all points within the specified range of z values and returns
//-- the number of points deleted.

int bead_deletePtsInZRange( Iobj *obj, Icont *cont, int minZ, int maxZ,
                            bool skipCheckedConts, bool skipCheckedPts, bool skipSeedView,
                            bool inclusive )
{
  
  if( skipCheckedConts && isInterpolated(cont) )
    return 0;
  
  //## CHECK RANGE IS GOOD:
  
  if( inclusive==false )
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
    if( ( z >= minZ && z <= maxZ )
        && !( skipSeedView && z == plug.seedView )
        && !( skipCheckedPts && bead_isPtChecked(obj,cont,p) ) )
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
    float distPerView  = fDiv( distPt1AndPt2, pt2->z - pt1->z );
    
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
    
    float fractToExpectedPt = fDiv( (pt->z - pt1->z) , ( pt2->z - pt1->z) ); 
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
      cont_copyPts( newCont, cont, true );
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
  if( isEmpty(cont) || psize(cont) < 2 || psize(cont) < minPts )
    return false;
  
  
  if( psize(cont)==2 )
  {
    return line_getLineEquation( getPt(cont,0), getPt(cont,1), gradient, offset );
  }
  
  float n = (float)psize(cont);
    
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
  double divisor = (sumXSq - (n*SQ(avgX)));
  if(divisor == 0)
    divisor = 0.0001;
  
  //float a   = ( sumXY - (n*avgX*avgY) ) / ( sumXSq - (n*SQ(avgX)) );
  //float b   = ( (avgY*sumXSq) - (avgX*sumXY) ) / ( sumXSq - (n*SQ(avgX)) );
  
  //*gradient = a;
  
  *gradient = ( sumXY - (n*avgX*avgY) )        / divisor;
  *offset   = ( (avgY*sumXSq) - (avgX*sumXY) ) / divisor;
  
  
  
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
  
  *a = fDiv ( ( (y2-y1)*(x1-x3) + (y3-y1)*(x2-x1) ), divisorA );
  *b = fDiv (  ( (y2 - y1) - (*a)*(SQ(x2) - SQ(x1)) ), divisorB );
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
  
  float maxYJump = 0.0f;
  int maxObjIdx  = 0;
  int maxContIdx = 0;
  int maxPtIdx   = 0;
  
  for (int o=0; o<osize(imod);o++)
  {
    Iobj *obj  = getObj(imod,o);
    
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
        if( plug.searchRangeOnly && plug.window->verifyCurrObjectAndUpdateRanges() &&
            ( z<plug.viewMinN-1 || z>plug.viewMaxN-1 ) )
          continue;
        
        float yJump = bead_calcYJump(cont,p);
        if ( (yJump > maxYJump) && (yJump < maxYJumpAllowed) )
        {
          maxYJump = yJump;
          maxObjIdx = o;
          maxContIdx = c;
          maxPtIdx = p;
        }
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
  
  imodSetIndex(imod, maxObjIdx, maxContIdx, maxPtIdx);
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
      if( plug.searchRangeOnly && plug.window->verifyCurrObjectAndUpdateRanges()
          && ( z<plug.viewMinN-1 || z>plug.viewMaxN-1 ) )
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
  
  for (int o=0; o<osize(imod); o++)
  {
    Iobj *obj = getObj(imod,o);
    
    for (int c=0; c<csize(obj); c++)
    {
      Icont *cont = getCont(obj,c);
      Ipoint *pt = bead_getPtOnView(cont,plug.seedView);
      if( pt != NULL )
        imodPointAppendXYZ( allSeedPts, pt->x, pt->y, pt->z );
    }
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
  
  float sideLenX = fDiv(plug.xsize, colsX);
  float sideLenY = fDiv(plug.ysize, rowsY);
  
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
  
  //edit_setZapLocation( maxPt.x, maxPt.y, maxPt.z, false );
  bead_focusOnPointCrude( maxPt.x, maxPt.y, maxPt.z );  
  //ivwSetLocation( plug.view, maxPt.x, maxPt.y, maxPt.z );
  //ivwRedraw( plug.view );
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
  
	CustomDialog ds("Grid Options");
  ds.addSpinBox ( "columns (x):", 1, 500, &colsX, 1, "Number of columns to display" );
  ds.addSpinBox ( "rows    (y):", 1, 500, &rowsY, 1, "Number of rows to display" );
	ds.addRadioGrp( "show grid as:",
                  "lines|"
                  "spheres only",
                  &showStyle );
  ds.addCheckBox( "rotate by tilt axis angle",
                  &rotateByAngle,
                  "Tilt the grid by the tilt axis angle" );
  ds.addCheckBox( "use dotted lines", &dottedLines );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  
  //## SETUP GRID:
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj = imodObjectGet(imod);
  
  Iobj *xobjX = ivwGetAnExtraObject(plug.view, plug.extraObjExtra);
  ivwClearAnExtraObject(plug.view, plug.extraObjExtra);
  
  float colXSide = fDiv( plug.xsize, colsX );
  float rowYSide = fDiv( plug.ysize, rowsY );
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
