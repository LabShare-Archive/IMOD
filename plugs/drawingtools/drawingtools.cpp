/*
 *  drawingtools.c -- Special plugin for contour drawing tools
 *
 */

/*****************************************************************************
 *   Copyright (C) 2007 by Andrew Noske from the Institute for Molecular     *
 *   Bioscience at the University of Queensland (Australia)                  *
 *****************************************************************************/

/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 1.5  2008/02/21 07:37:46  tempuser
    *** empty log message ***

    Revision 1.4  2008/02/21 07:33:42  tempuser
    Changed DBL_MAX

    Revision 1.3  2008/01/29 02:32:47  tempuser
    *** empty log message ***

    Revision 1.2  2008/01/29 00:04:01  tempuser
    *** empty log message ***

    Revision 1.1  2008/01/24 01:25:30  tempuser
    *** empty log message ***

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
#include "imodplugin.h"
#include "dia_qtutils.h"
#include "drawingtools.h"

//############################################################

static DrawingToolsData plug = { 0, 0 };

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
    
  return("Drawing Tools");
}

//------------------------
//-- MAPPED FUNCTION: Grab hotkey input. return 1 if we handle the key.
 
int imodPlugKeys(ImodView *vw, QKeyEvent *event)
{
  int keyhandled = 1;
  
  if (!plug.view)          // if plugin window isn't open: don't grab keys
    return 0;
  
  int keysym = event->key();            // key value (Key_A, Key_Space... etc)
  int ctrl    = event->state() & Qt::ControlButton;   // ctrl modifier
  int shift   = event->state() & Qt::ShiftButton;     // shift modifier
  
  switch(keysym)
  {
    case Qt::Key_Q:
      plug.window->changeDeformCircleRadius( (shift) ? -5 : -1 );
      plug.window->setFocus();
      plug.window->drawExtraObject(true);
      break;
    case Qt::Key_W: 
      plug.window->changeDeformCircleRadius( (shift) ? 5 : 1 );
      plug.window->setFocus();
      plug.window->drawExtraObject(true);
      break;
    case Qt::Key_R:
      plug.window->reduceCurrentContour();
      break;
    case Qt::Key_E:
      plug.window->smoothCurrentContour();
      break;
      
    case Qt::Key_A:
      plug.window->selectNextOverlappingContour();
      break;
    case Qt::Key_I:
      edit_inversePointsInContour(shift);
      break;
    //case Qt::Key_T:                  // temporary testing purposes - comment out
    //  plug.window->test();
    //  break;
      
    case Qt::Key_X:
      if(ctrl)
        plug.window->cut();
      break;
    case Qt::Key_C:
      if(ctrl)
        plug.window->copy();
      break;
    case Qt::Key_V:
      if(ctrl)
        plug.window->paste();
      break;
      
    case Qt::Key_1:
      plug.window->changeTypeSelected( DM_NORMAL ); 
      break;
    case Qt::Key_2:
      plug.window->changeTypeSelected( DM_DEFORM ); 
      break;
    case Qt::Key_3:
      plug.window->changeTypeSelected( DM_JOIN ); 
      break;
    case Qt::Key_4:
      plug.window->changeTypeSelected( DM_TRANSFORM ); 
      break;
    case Qt::Key_5:
      plug.window->changeTypeSelected( DM_ERASER ); 
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
    plug.drawMode                 = DM_DEFORM;
    plug.draw_reducePts           = 1;
    plug.draw_reducePtsMaxArea    = 3;
    plug.draw_smoothMinDist       = 5;
    plug.draw_smoothTensileFract  = 0.5;
    plug.draw_deformRadius        = 30.0;
    
    Ipoint origin;
    setPt( &origin, 0,0,0);
    plug.copiedCont = imodContourNew();
    cont_generateCircle( plug.copiedCont, 30.0f, 20, origin, false );
        // puts a circle in copiedCont until the user copies his own contour
    
    plug.initialized = true;
  }
  plug.view = inImodView;
  ivwTrackMouseForPlugs(plug.view, 1);
  ivwGetImageSize(inImodView, &plug.xsize, &plug.ysize, &plug.zsize);
  plug.extraObjNum = ivwGetFreeExtraObjectNumber(plug.view);
  
  //## CREATE THE PLUGING WINDOW:
  
  plug.window  = new DrawingTools(imodDialogManager.parent(IMOD_DIALOG),"Drawing Tools");
  
  imodDialogManager.add((QWidget *)plug.window, IMOD_DIALOG);
  plug.window->show();
}


//------------------------
//-- MAPPED FUNCTION: Process wheel events

int imodPlugEvent(ImodView *vw, QEvent *event, float imx, float imy)
{
  if( plug.window == NULL )
    return (0);
  
  if (event->type() == QEvent::Wheel)
  {
    if( plug.drawMode == DM_DEFORM
        || plug.drawMode == DM_JOIN
        || plug.drawMode == DM_ERASER )
    {
      QWheelEvent *wheelEvent = static_cast<QWheelEvent*>(event);
      plug.window->changeDeformCircleRadius( (float)wheelEvent->delta() / 100.0 );
      plug.window->drawExtraObject(true);
    }
  }
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
  
//## UPDATE MOUSE VALUES:

  plug.mousePrev = plug.mouse;
  
  int noZap = ivwGetTopZapMouse(plug.view, &plug.mouse); // returns 1 if no Zap window
  if(noZap == 1)
    return (2);
  
  plug.changeX = plug.mouse.x - plug.mousePrev.x;
  plug.changeY = plug.mouse.y - plug.mousePrev.y;
  
//## REGENERATE DEFORM CIRCLE:
  
  plug.window->drawExtraObject(false);
  
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
  
//## IF ANY BUTTON WAS JUST PRESSED: GET THE CENTROID OF THE CURRENT CONTOUR
  if ( plug.but3Pressed || plug.but2Pressed || plug.but1Pressed )
  {
    Icont *cont = imodContourGet( ivwGetModel(plug.view) );
    if( isContValid(cont) ) {
      Ipoint ll, ur;
      imodContourGetBBox(cont, &ll, &ur);
      plug.centerPt = line_getPtHalfwayBetween( &ll, &ur );
    }
    plug.mouseDownPt = plug.mouse;
  }
  
  
  //## IF BUTTON 1 AND SHIFT ARE DOWN: SCROLL CONTOURS
  
  if( plug.but1Down && plug.shiftDown )
  {
    float zapZoom = 1.0f;
    ivwGetTopZapZoom(plug.view, &zapZoom);
    edit_changeSelectedSlice( plug.changeY * zapZoom, true );
    return (1);
  }
  
  //## EXIT EARLY IF NO ACTION IS NEEDED:
  
  bool actionNeeded =
    ( plug.but2Pressed
      || plug.but2Down
      || plug.but2Released
      || ((plug.drawMode==DM_TRANSFORM || plug.drawMode==DM_ERASER) && plug.but3Down) );
  
  if ( !(actionNeeded) )            // if no action is needed: do nothing
    return (2);
  
  //if ( !isCurrObjValidAndShown() ) {    // if object is not valid: exit
  //  wprint("ERROR: invalidObject");
  //  return (2);
  //}
  
//## PERFORM ACTION:
  
  switch( plug.drawMode )
  {
    case (DM_DEFORM):
    {
      if( plug.but2Pressed ) {
        edit_executeDeformStart();
      }
      else if ( plug.but2Down ) {
        edit_executeDeform();
      }
      else if( plug.but2Released ) {
        edit_executeDeformEnd();
      }
      break;
    }
    
    case (DM_JOIN):
    {
      if( plug.but2Pressed ) {
        edit_executeDeformStart();
      }
      else if ( plug.but2Down ) {
        edit_executeDeform();
      }
      else if( plug.but2Released ) {
        edit_executeJoinEnd();
      }
      break;
    }
    
    case (DM_TRANSFORM):
    {
      Icont *cont = imodContourGet( ivwGetModel(plug.view) );
      if( isContValid(cont) )
      {
        if( !plug.shiftDown )
        {
          if ( plug.but2Down )            // move currently selected contour
          {
            if ( !edit_copiedContIfDiffSlice(true) )
            {
              undoContourDataChgCC( plug.view );
              cont_translate( cont, plug.changeX, plug.changeY );
            }
          }
          else if (plug.but3Down )        // rotate currently selected contour
          {
            undoContourDataChgCC( plug.view );
            float scaleX = 1.0f + (plug.changeX / 100.0f);
            float scaleY = 1.0f + (plug.changeY / 100.0f);
            cont_scaleAboutPtXY( cont, &plug.centerPt, scaleX, scaleY );
          }
          else if (plug.but2Released || plug.but3Released )
          {
            undoFinishUnit( plug.view );          // FINISH UNDO
          }
        }
        else
        {
          if ( plug.but2Down )            // scale currently selected contour
          {
            undoContourDataChgCC( plug.view );
            cont_rotateAroundPoint2D( cont, &plug.centerPt, plug.changeY );
          }
          else if ( plug.but3Down )       // strech currently selected contour
          {
            float distMovedAway = line_distBetweenPts2D(&plug.centerPt,&plug.mouse) -
              line_distBetweenPts2D(&plug.centerPt,&plug.mousePrev);
            float  stretchFactor = 1.0 + (fDivide((distMovedAway),
                            (line_distBetweenPts2D(&plug.centerPt,&plug.mouse)+0.1f)));
            float  angle = line_getAngle2D( &plug.centerPt, &plug.mouse );
            undoContourDataChgCC( plug.view );
            cont_stretchAlongAngle( cont, &plug.centerPt, angle, stretchFactor );
          }
          else if ( plug.but2Released || plug.but3Released )
          {
            undoFinishUnit( plug.view );          // FINISH UNDO
          }
        }
      }
      break;
    }
    
    case (DM_ERASER):
    {
      if( !plug.shiftDown )
      {
        if ( plug.but2Down )              // delete contours touching deform circle
        {
          edit_eraseContsInCircle(plug.mouse, plug.draw_deformRadius);
        }
        else if (plug.but3Down )          // delete points within deform circle
        {  
          edit_erasePointsInCircle(plug.mouse, plug.draw_deformRadius);
        }
      }
      else
      {
        if ( plug.but2Down )              // delete contours touching deform circle  
        {
          edit_breakPointsInCircle(plug.mouse, plug.draw_deformRadius);
        }
      }
      break;
    }
    
    default:    // DM_NORMAL
    {
      return (2);
    }
  }
  
  
  ivwDraw( plug.view, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC );
  
  //ivwRedraw( plug.view );     //|- if we get to here we have dealt with the action
  return (1);                 //|  so we redraw and return 1
}


//############################################################

//----------------------------------------------------------------------------
//
//          DrawingTools METHODS:
//
//----------------------------------------------------------------------------


//## WINDOW CLASS CONSTRUCTOR:

static char *buttonLabels[] = {"Done", "Help"};
static char *buttonTips[] = {"Close Drawing Tools", "Open help window"};

DrawingTools::DrawingTools(QWidget *parent, const char *name) :
DialogFrame(parent, 2, buttonLabels, buttonTips, true, "Drawing Tools", "", name)
{
  const int LAYOUT_MARGIN   = 4;
  const int LAYOUT_SPACING  = 4;
  const int GROUP_MARGIN    = 1;
  const int SPACER_HEIGHT   = 15;
  
  
  //## Type:
  
  typeButtonGroup = new QVButtonGroup("Drawing Mode:", this);
  typeButtonGroup->setInsideSpacing(0);
  typeButtonGroup->setInsideMargin(5);
  typeButtonGroup->setMargin(GROUP_MARGIN);
  connect(typeButtonGroup, SIGNAL(clicked(int)), this, SLOT(changeType(int)));
  
  typeRadio_Normal = new QRadioButton("Normal        [1]", typeButtonGroup);
  typeRadio_Normal->setFocusPolicy(QWidget::NoFocus);
  QToolTip::add(typeRadio_Normal, "Contours are drawn normally");
  
  typeRadio_Deform = new QRadioButton("Deform        [2]", typeButtonGroup);
  typeRadio_Deform->setFocusPolicy(QWidget::NoFocus);
  QToolTip::add(typeRadio_Deform, "Draw and modify closed contours "
                "quickly using the deform circle to push or pinch lines");
  
  typeRadio_Join = new QRadioButton("Join              [3]", typeButtonGroup);
  typeRadio_Join->setFocusPolicy(QWidget::NoFocus);
  QToolTip::add(typeRadio_Join, "Join or split contours quickly by making overlaps");
  
  typeRadio_Transform = new QRadioButton("Transform    [4]", typeButtonGroup);
  typeRadio_Transform->setFocusPolicy(QWidget::NoFocus);
  QToolTip::add(typeRadio_Transform, "Allows you to move, rotate and "
                "scale the selected contour");
  
  typeRadio_Eraser = new QRadioButton("Eraser          [5]", typeButtonGroup);
  typeRadio_Eraser->setFocusPolicy(QWidget::NoFocus);
  QToolTip::add(typeRadio_Eraser, "Erase contours instantly by clicking them");
  
  changeTypeSelected( plug.drawMode );
  
  mLayout->addWidget(typeButtonGroup);
  
  
  //## Interpolation Options:
  
  grpOptions = new QGroupBox("Contour Smoothing Options:", this);
  grpOptions->setFocusPolicy(QWidget::NoFocus);
  grpOptions->setMargin(GROUP_MARGIN);
  
  gridLayout1 = new QGridLayout(grpOptions);
  gridLayout1->setSpacing(LAYOUT_SPACING);
  gridLayout1->setMargin(LAYOUT_MARGIN);
  gridLayout1->addItem( new QSpacerItem(1,SPACER_HEIGHT), 0, 0);
  
  reducePtsCheckbox = new QCheckBox("reduce drawn contours", grpOptions);
  reducePtsCheckbox->setFocusPolicy(QWidget::NoFocus);
  reducePtsCheckbox->setChecked( plug.draw_reducePts );
  QObject::connect(reducePtsCheckbox,SIGNAL(clicked()),this,
                   SLOT(changeReducePts()));
  QToolTip::add(reducePtsCheckbox, 
                "Automatically applies smoothing to contours drawn with the "
                "deform and join tools after the mouse button is released");
  gridLayout1->addMultiCellWidget(reducePtsCheckbox, 1, 1, 0, 1);
  
  lblMaxArea = new QLabel("reduction max area:", grpOptions);
  lblMaxArea->setFocusPolicy(QWidget::NoFocus);
  gridLayout1->addWidget(lblMaxArea, 2, 0);
  
  maxAreaSpinner = new QSpinBox(grpOptions);
  maxAreaSpinner->setFocusPolicy(QWidget::NoFocus);
  maxAreaSpinner->setMinValue(1);
  maxAreaSpinner->setMaxValue(20);
  maxAreaSpinner->setValue( (int)plug.draw_reducePtsMaxArea );
  QObject::connect(maxAreaSpinner,SIGNAL(valueChanged(int)),this,
                   SLOT(changeMaxArea(int)));
  QToolTip::add(maxAreaSpinner, "If three consequtive points within a contour "
                "form a triangular area greater than this the middle point is removed");
  gridLayout1->addWidget(maxAreaSpinner, 2, 1);
  
  lblSmoothPtsDist = new QLabel("smooth point dist:", grpOptions);
  lblSmoothPtsDist->setFocusPolicy(QWidget::NoFocus);
  gridLayout1->addWidget(lblSmoothPtsDist, 3, 0);
  
  smoothPtsDist = new QSpinBox(grpOptions);
  smoothPtsDist->setFocusPolicy(QWidget::NoFocus);
  smoothPtsDist->setMinValue(1);
  smoothPtsDist->setMaxValue(50);
  smoothPtsDist->setValue( (int)plug.draw_smoothMinDist );
  QObject::connect(smoothPtsDist,SIGNAL(valueChanged(int)),this,
                   SLOT(changeSmoothPtsDist(int)));
  QToolTip::add(smoothPtsDist,
                "The minimum distance between points or when a contour is "
                "smoothed - if two consequtive are greater than this "
                "distance a point will be added between them");
  gridLayout1->addWidget(smoothPtsDist, 3, 1);
  
  lblSmoothTensileFract = new QLabel("smooth tensile value:", grpOptions);
  lblSmoothTensileFract->setFocusPolicy(QWidget::NoFocus);
  gridLayout1->addWidget(lblSmoothTensileFract, 4, 0);
  
  smoothTensileFract = new QSpinBox(grpOptions);
  smoothTensileFract->setFocusPolicy(QWidget::NoFocus);
  smoothTensileFract->setMinValue(0);
  smoothTensileFract->setMaxValue(20);
  smoothTensileFract->setValue( (int)(plug.draw_smoothTensileFract * 10.0f) );
  QObject::connect(smoothTensileFract,SIGNAL(valueChanged(int)),this,
                   SLOT(changeSmoothTensileFract(int)));
  QToolTip::add(smoothTensileFract,
                "This value dictates how curvy the contour will be when "
                "points are added during smoothing (5 is recommended). "
                "Smoothing is done using a cardinal spline algorithm "
                "using a tensile fraction of this value divide 10.");
  gridLayout1->addWidget(smoothTensileFract, 4, 1);
  
  mLayout->addWidget(grpOptions);
  
  
  //## Object
  
  grpObject = new QGroupBox("Smoothing Actions:", this);
  grpObject->setFocusPolicy(QWidget::NoFocus);
  grpObject->setMargin(GROUP_MARGIN);
  
  vboxLayout1 = new QVBoxLayout(grpObject);
  vboxLayout1->setSpacing(LAYOUT_SPACING);
  vboxLayout1->setMargin(LAYOUT_MARGIN);
  vboxLayout1->addItem( new QSpacerItem(1,SPACER_HEIGHT) );
  
  reduceContButton = new QPushButton("Reduce Contour", grpObject);
  reduceContButton->setFocusPolicy(QWidget::NoFocus);
  connect(reduceContButton, SIGNAL(clicked()), this, SLOT(reduceCurrentContour()));
  QToolTip::add(reduceContButton,
                "Reduces the number of points in the current contour");
  vboxLayout1->addWidget(reduceContButton);
  
  smoothContButton = new QPushButton("Smooth Contour", grpObject);
  smoothContButton->setFocusPolicy(QWidget::NoFocus);
  connect(smoothContButton, SIGNAL(clicked()), this, SLOT(smoothCurrentContour()));
  QToolTip::add(smoothContButton,
                "Smooths and increases the number ""of points in the current contour");
  vboxLayout1->addWidget(smoothContButton);
  
  reduceObjectButton = new QPushButton("Reduce OBJECT", grpObject);
  reduceObjectButton->setFocusPolicy(QWidget::NoFocus);
  connect(reduceObjectButton, SIGNAL(clicked()), this, SLOT(reduceObject()));
  QToolTip::add(reduceObjectButton,
                "Reduces ALL contours in the current object");
  vboxLayout1->addWidget(reduceObjectButton);
  
  smoothObjectButton = new QPushButton("Smooth OBJECT", grpObject);
  smoothObjectButton->setFocusPolicy(QWidget::NoFocus);
  connect(smoothObjectButton, SIGNAL(clicked()), this, SLOT(smoothObject()));
  QToolTip::add(smoothObjectButton,
                "Smooths ALL contours in the current object (use with caution)");
  vboxLayout1->addWidget(smoothObjectButton);
  
  mLayout->addWidget(grpObject);
  
  
  mLayout->addStretch();
  this->adjustSize();
  
  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
}


//## SLOTS:


//------------------------
//-- Accesses the extra object and draw a red deform circle and/or other
//-- reference contour at the last recorded position of the mouse. What is
//-- drawn depends on what drawing mode is selected.

bool DrawingTools::drawExtraObject( bool redraw )
{
  Iobj *xobj = ivwGetAnExtraObject(plug.view, plug.extraObjNum);
  Icont *con;
  
  if ( !plug.window || !xobj )
    return 0;
  
  //## INITIALIZE EXTRA OBJECT:
  
  ivwClearAnExtraObject(plug.view, plug.extraObjNum);
  imodObjectSetColor(xobj, 1.0, 0.0, 0.0);
  imodObjectSetValue(xobj, IobjFlagClosed, 1);
  con = imodContourNew();
  if (!con)
    return false;
  
  //## GET Z VALUE:
  
  int ix, iy,iz;
  ivwGetLocation(plug.view, &ix, &iy, &iz);
  plug.mouse.z = iz;
  
  float x = plug.mouse.x;
  float y = plug.mouse.y;
  float z = plug.mouse.z;
  
  float radius = plug.draw_deformRadius;
  float hRadius = radius*0.5f;
  float qRadius = radius*0.25f;
  
  
  //## IF CHANING Z HEIGHT: DRAW RECTANGLE REPRESENTING SLICES
  
  if( plug.but1Down && plug.shiftDown )   // draw rectangle and bar representing z slices
  {
    float zapZoom = 1.0f;
    ivwGetTopZapZoom(plug.view, &zapZoom);
    int currSlice;
    ivwGetTopZapZslice(plug.view, &currSlice);
    float sc = fDivide( 1.0f, zapZoom);
    float xmin = x - 20*sc;
    float xmax = xmin + 10*sc;
    float ymin = y - (currSlice-2)*sc;
    float ymax = ymin + plug.zsize*sc;
    
    imodPointAppendXYZ( con, xmin, ymin, currSlice );
    imodPointAppendXYZ( con, xmax, ymin, currSlice );
    imodPointAppendXYZ( con, xmax, ymax, currSlice );
    imodPointAppendXYZ( con, xmin, ymax, currSlice );
    
    imodPointAppendXYZ( con, xmin,     y, currSlice );
    imodPointAppendXYZ( con, xmax,     y, currSlice );
    imodPointAppendXYZ( con, xmin,     y, currSlice );
		
    imodContourSetFlag(con, ICONT_CURSOR_LIKE | ICONT_MMODEL_ONLY, 1);
    imodObjectAddContour(xobj, con);
    
		if( redraw )
			ivwRedraw( plug.view );
		return true;
  }
  
  //## CONSTRUCT REFERENCE CONTOUR:
  
  switch( plug.drawMode )
  {
  case (DM_NORMAL ):            // draw a tiny verticle line
    {
      imodPointAppendXYZ( con, x, y, z );
      imodPointAppendXYZ( con, x, y+1, z );
      break;
    }
  case(DM_DEFORM):            // draw deform circle
    {
      cont_generateCircle( con, radius, 100, plug.mouse, true );
      if( plug.shiftDown )
      {
        imodPointAppendXYZ( con, x+radius, y, z-1);
        imodPointAppendXYZ( con, x+hRadius, y+qRadius, z-1 );
        imodPointAppendXYZ( con, x+hRadius, y+qRadius, z );
        imodPointAppendXYZ( con, x+qRadius, y, z );
        imodPointAppendXYZ( con, x+hRadius, y-qRadius, z );
        imodPointAppendXYZ( con, x+hRadius, y-qRadius, z-1 );
        
        imodPointAppendXYZ( con, x-hRadius, y-qRadius, z-1 );
        imodPointAppendXYZ( con, x-hRadius, y-qRadius, z );
        imodPointAppendXYZ( con, x-qRadius, y, z );
        imodPointAppendXYZ( con, x-hRadius, y+qRadius, z );
        imodPointAppendXYZ( con, x-hRadius, y+qRadius, z-1 );
        imodPointAppendXYZ( con, x-radius, y, z-1);
      }
      break;
    }
  case(DM_JOIN):              // draw deform circle with plus sign in middle
    {
      cont_generateCircle( con, radius, 100, plug.mouse, true );
      
      imodPointAppendXYZ( con, x+radius,  y,      z-1  );
      imodPointAppendXYZ( con, x+hRadius,  y,      z-1 );
      imodPointAppendXYZ( con, x+hRadius,  y,      z );
      imodPointAppendXYZ( con, x-hRadius,  y,      z );
      imodPointAppendXYZ( con, x,      y,      z );
      imodPointAppendXYZ( con, x,      y+hRadius,  z );
      imodPointAppendXYZ( con, x,      y-hRadius,  z );;
      imodPointAppendXYZ( con, x,      y,      z );  
      imodPointAppendXYZ( con, x,      y,      z-1 );
      imodPointAppendXYZ( con,  x+radius,  y,      z-1);  
      break;
    }
  case(DM_TRANSFORM):         // draw rectangle around current contour or next to mouse 
    {

      Icont *cont = imodContourGet( ivwGetModel(plug.view) );
      if( isContValid(cont) )
      {
        Ipoint ll, ur;
        imodContourGetBBox(cont, &ll, &ur);
        imodPointAppendXYZ( con, ll.x, ll.y, z-1 );
        imodPointAppendXYZ( con, ll.x, ll.y, z );
        imodPointAppendXYZ( con, ur.x, ll.y, z );
        imodPointAppendXYZ( con, ur.x, ur.y, z );
        imodPointAppendXYZ( con, ll.x, ur.y, z );
        imodPointAppendXYZ( con, ll.x, ll.y, z );
        imodPointAppendXYZ( con, ll.x, ll.y, z-1 );
        
        if(plug.but2Down)      // draw line from point clicked to mouse
        {
          imodPointAppendXYZ( con, plug.mouseDownPt.x, plug.mouseDownPt.y, z-1 );
          imodPointAppendXYZ( con, plug.mouseDownPt.x, plug.mouseDownPt.y, z );  
          imodPointAppendXYZ( con, x, y, z );
          imodPointAppendXYZ( con, x, y, z-1 );    
        }
        else if(plug.but3Down)    // draw line from center of contour to mouse
        {
          imodPointAppendXYZ( con, plug.centerPt.x, plug.centerPt.y, z-1 );
          imodPointAppendXYZ( con, plug.centerPt.x, plug.centerPt.y, z );  
          imodPointAppendXYZ( con, x, y, z );      
          imodPointAppendXYZ( con, x, y, z-1 );      
        }
      }
      else
      {
        float rectLen = 10.0f;
        float zapZoom = 1.0f;
        int noZap = ivwGetTopZapZoom(plug.view, &zapZoom);
        if( noZap != 1 )   // if there is a top window: determine pixel length
          rectLen = fDivide( 10.0f, zapZoom);
        imodPointAppendXYZ( con, x+1.0f*rectLen, y+1.0f*rectLen, z );
        imodPointAppendXYZ( con, x+2.0f*rectLen, y+1.0f*rectLen, z );
        imodPointAppendXYZ( con, x+2.0f*rectLen, y+1.5f*rectLen, z );
        imodPointAppendXYZ( con, x+1.0f*rectLen, y+1.5f*rectLen, z );
        imodPointAppendXYZ( con, x+1.0f*rectLen, y+1.0f*rectLen, z );
        imodPointAppendXYZ( con, x+1.0f*rectLen, y+1.0f*rectLen, z-1 );
      }
      
      break;
    }
  case(DM_ERASER):            // draw deform circle with a diagonal line through it
    {
      if( plug.but2Down || plug.but3Down )  {
        cont_generateCircle( con, radius*0.99f, 100, plug.mouse, true );
        cont_generateCircle( con, radius*0.98f, 100, plug.mouse, true );
      }
      cont_generateCircle( con, radius, 100, plug.mouse, true );
      
      imodPointAppendXYZ( con, x+hRadius, y, z-1);    
      imodPointAppendXYZ( con, x+hRadius, y+hRadius, z-1 );
      imodPointAppendXYZ( con, x+hRadius, y+hRadius, z );
      imodPointAppendXYZ( con, x-hRadius, y-hRadius, z );
      imodPointAppendXYZ( con, x-hRadius, y-hRadius, z-1 );
      imodPointAppendXYZ( con, x+hRadius, y, z-1);    
      if( plug.shiftDown )              // draw extra arrows on diagonal line
      {
        imodPointAppendXYZ( con, x+hRadius, y, z-1);    
        imodPointAppendXYZ( con, x+hRadius, y+qRadius, z-1 );
        imodPointAppendXYZ( con, x+hRadius, y+qRadius, z );
        imodPointAppendXYZ( con, x+hRadius, y+hRadius, z );
        imodPointAppendXYZ( con, x-hRadius, y-hRadius, z );
        imodPointAppendXYZ( con, x-hRadius, y-qRadius, z );
        imodPointAppendXYZ( con, x-hRadius, y-qRadius, z-1 );
        imodPointAppendXYZ( con, x+hRadius, y, z-1);
      }
      break;
    }
  }
  
  imodContourSetFlag(con, ICONT_CURSOR_LIKE | ICONT_MMODEL_ONLY, 1);
  imodObjectAddContour(xobj, con);
  
  if( redraw )
    ivwRedraw( plug.view );
  
  return true;
}


//------------------------
//-- Clears all the contents of the extra object.

void DrawingTools::clearExtraObj()
{
  Iobj *obj = ivwGetExtraObject(plug.view);
  int ncont = imodObjectGetMaxContour(obj);
  if (!ncont)
    return;
  
  // Get the contour pointer.  "Remove" contours from the end, then delete
  // and free the contour data
  Icont *cont = getCont(obj, 0);
  for (int co = ncont - 1; co >= 0; co--)
    imodObjectRemoveContour(obj, co);
  imodContoursDelete(cont, ncont);
}


//------------------------
//-- Reduces the number of points in the current contour.

void DrawingTools::reduceCurrentContour()
{
  if( !isCurrContValid() )
    return;
  
  undoContourDataChgCC( plug.view );      // REGISTER UNDO
  bool change = edit_reduceCurrContour();
  if(change)
  {
    undoFinishUnit( plug.view );            // FINISH UNDO
    ivwRedraw( plug.view );
  }
  wprint("Current contour has been reduced\n");
}

//------------------------
//-- Smooths and increases the number of points in the current contour.

void DrawingTools::smoothCurrentContour()
{
  if( !isCurrContValid() )
    return;
    
  undoContourDataChgCC( plug.view );      // REGISTER UNDO
  bool change = edit_smoothCurrContour();
  if(change)
  {
    undoFinishUnit( plug.view );            // FINISH UNDO
    ivwRedraw( plug.view );
  }
  wprint("Current contour has been smoothed\n");
}


//------------------------
//-- Reduces ALL contours in the current object.

void DrawingTools::reduceObject()
{
  if( !isCurrObjValidAndShown() )
  {
    MsgBox("You have not selected a valid displayed object");
    return;
  }
  if( !MsgBoxYesNo( plug.window,
                    "Are you sure you want to reduce this object?\n"
                    "You CANNOT undo this operation and you will"
                    "lose points/data." ) )
  {
    return;
  }
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj  = getCurrObj();
  Icont *cont;
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int totalContoursChanged = 0;
  int totalPointsBefore = 0;
  int totalPointsAfter = 0;
  
  for(int c=0; c<imodObjectGetMaxContour(obj); c++)
  {
    cont = getCont( obj, c );
    
    if( !isContValid(cont) )
      continue;
    
    int nPointsBefore = psize( cont );        
    //imodSetIndex(imod, objIdx, c, 0);    // |-- DOESN'T WORK FOR SOME REASON
    //undoContourDataChgCC( imod );        // |
    cont_reducePtsMinArea( cont, plug.draw_reducePtsMaxArea, isContClosed(obj,cont) );
    int nPointsAfter = psize( cont );
    
    totalPointsBefore += nPointsBefore;
    totalPointsAfter  += nPointsAfter;
    
    if( nPointsBefore != nPointsAfter )
      totalContoursChanged++;
  }
  //if(totalContoursChanged)
  //  undoFinishUnit( plug.view );
  
  imodSetIndex(imod, objIdx, contIdx, 0);
  
  int percentReduction = 100 - (fDivide( totalPointsAfter, totalPointsBefore ) * 100);
  
  wprint("All contours have been reduced\n");
  wprint(" # contours changed = %d\n", totalContoursChanged );
  wprint(" total # points = %d -> %d (%d%% reduction)\n",
         totalPointsBefore,totalPointsAfter,percentReduction);
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Smooths ALL contours in the current object.

void DrawingTools::smoothObject()
{
  if( !isCurrObjValidAndShown() )
  {
    MsgBox("You have not selected a valid displayed object");
    return;
  }
  if( !MsgBoxYesNo( plug.window,
                    "Are you sure you want to smooth this object?\n"
                    "You CANNOT undo this operation so you should test the \n"
                    "result of your smoothing input on a few contours first." ) )
  {
    return;
  }
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj  = getCurrObj();
  Icont *cont;
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int totalContoursChanged = 0;
  int totalPointsBefore = 0;
  int totalPointsAfter = 0;
  
  for(int c=0; c<imodObjectGetMaxContour(obj); c++)
  {
    cont = getCont( obj, c );
    
    if( !isContValid(cont) )
      continue;
    
    int nPointsBefore = psize( cont );        
    //imodSetIndex(imod, objIdx, c, 0);    // |-- DOESN'T WORK FOR SOME REASON
    //undoContourDataChgCC( imod );        // |
    cont_addPtsSmooth( cont, plug.draw_smoothMinDist, plug.draw_smoothTensileFract,
                       isContClosed(obj,cont) );
    int nPointsAfter = psize( cont );
    
    totalPointsBefore += nPointsBefore;
    totalPointsAfter  += nPointsAfter;
    
    if( nPointsBefore != nPointsAfter )
      totalContoursChanged++;
  }
  //if(totalContoursChanged)
  //  undoFinishUnit( plug.view );
  
  imodSetIndex(imod, objIdx, contIdx, 0);
  
  int percentIncrease = (fDivide( totalPointsAfter, totalPointsBefore ) * 100);
  
  wprint("All contours have been reduced\n");
  wprint(" # contours changed = %d\n", totalContoursChanged );
  wprint(" total # points = %d -> %d (%d%% increase)\n",
         totalPointsBefore,totalPointsAfter,percentIncrease);
  
  ivwRedraw( plug.view );
}


//------------------------
//-- Selects the next contour after the currently selected one which overlaps
//-- another contour.

void DrawingTools::selectNextOverlappingContour()
{
  edit_selectNextOverlappingCont();
  ivwRedraw( plug.view );
}


//------------------------
//-- Method used for testing new routines.

void DrawingTools::test()
{  
  Icont *cont = getCurrCont();
  
  if( !isContValid(cont) )
  {
    wprint("Have not selected valid contour\n");
    return;
  }
  
  ivwRedraw( plug.view );
}


//------------------------
//-- Method used for testing new routines.

void DrawingTools::cut()
{  
  Icont *cont = getCurrCont();
  if( !isContValid(cont) )  {
    return;
  }
  
  //## COPY CURRENT CONTOUR TO plug.copiedCont AND TRANSLATE TO ORIGIN:
  imodContourDelete( plug.copiedCont );
  plug.copiedCont = imodContourDup(cont);
  Ipoint centroidPt;
  imodContourCenterOfMass( plug.copiedCont, &centroidPt );
  centroidPt.x = -centroidPt.x;
  centroidPt.y = -centroidPt.y;
  centroidPt.z = -centroidPt.z;
  cont_translate( plug.copiedCont, &centroidPt );
  
  //## DELETE CURRENT CONTOUR:
  Imod *imod  = ivwGetModel(plug.view);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  undoContourRemovalCC( plug.view );                        // REGISTER UNDO
  imodObjectRemoveContour( getCurrObj(), contIdx );         // delete current contour
  imodSetIndex(imod, objIdx, -1, -1);
  
  wprint("Contour has been cut\n");
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Method used for testing new routines.

void DrawingTools::copy()
{  
  Icont *cont = getCurrCont();
  if( !isContValid(cont) )  {
    wprint("Select a valid contour to copy\n");
    return;
  }
  
  imodContourDelete( plug.copiedCont );
  plug.copiedCont = imodContourDup(cont);
  Ipoint centroidPt;
  imodContourCenterOfMass( plug.copiedCont, &centroidPt );
  centroidPt.x = -centroidPt.x;
  centroidPt.y = -centroidPt.y;
  centroidPt.z = -centroidPt.z;
  cont_translate( plug.copiedCont, &centroidPt );
  
  wprint("Contour has been copied\n");
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Method used for testing new routines.

void DrawingTools::paste()
{  
  if( !isContValid( plug.copiedCont ) )  {
    wprint("No contour has been copied yet\n");
    return;
  }
  if( !isCurrObjValidAndShown() )  {
    wprint("Must select valid object\n");
    return;
  }
  
  Imod *imod  = ivwGetModel(plug.view);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  Icont *contNew = imodContourDup(plug.copiedCont);
  cont_translate( contNew, &plug.mouse );
  int newContPos = edit_addContourToObj( getCurrObj(), contNew, true );
  imodSetIndex(imod, objIdx, newContPos, 0);
  imodContourDelete(contNew);
  
  ivwRedraw( plug.view );
}



//## BASIC METHODS TO CHANGE PLUG DATA:

//------------------------
//-- Change the drawMode

void DrawingTools::changeType( int value ) {
  plug.drawMode = value;
}

//------------------------
//-- Change drawMode and set appropriate radio button

void DrawingTools::changeTypeSelected( int newType) {
  plug.drawMode = newType;
  typeButtonGroup->setButton( plug.drawMode);
  plug.window->drawExtraObject(true);
}

//------------------------
//-- Change draw_reducePtsMaxArea

void DrawingTools::changeMaxArea( int value ) { 
  plug.draw_reducePtsMaxArea = value;
}

//------------------------
//-- Change draw_smoothMinDist

void DrawingTools::changeSmoothPtsDist( int value ) {
  plug.draw_smoothMinDist = value;
}

//------------------------
//-- Change draw_smoothTensileFract

void DrawingTools::changeSmoothTensileFract( int value ) {
  plug.draw_smoothTensileFract = (float)value/10.0f;  
}

//------------------------
//-- Change changeReducePts

void DrawingTools::changeReducePts() {
  plug.draw_reducePts = reducePtsCheckbox->isChecked() ? 1 : 0;
}

//------------------------
//-- Change changeDeformCircleRadius

void DrawingTools::changeDeformCircleRadius( float value ) {
  changeNumWithinRange( plug.draw_deformRadius, 2.0f, 200.0f, value );
}


//## PROTECTED:


//------------------------
//-- Called to display help window.

void DrawingTools::buttonPressed(int which)
{
  if (!which)
    close();
  else
  {
    QString str = QString(getenv("IMOD_DIR"));
    str += QString("/lib/imodplug/drawingtools.html");
    
    imodShowHelpPage(str);
  }
}

//------------------------
//-- Window closing event handler - removes this pluging from the imod dialog manager

void DrawingTools::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)plug.window);
  clearExtraObj();
  ivwFreeExtraObject(plug.view, plug.extraObjNum);
  ivwTrackMouseForPlugs(plug.view, 0);
  
  imodContourDelete( plug.copiedCont );
  plug.view = NULL;
  plug.window = NULL;
  e->accept();
}


//------------------------
//-- Key press event handler - closes on escape or passes on event to "ivwControlKey"

void DrawingTools::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

//------------------------
//-- Key release event hander - passes on event to "ivwControlKey"

void DrawingTools::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}






//############################################################



//----------------------------------------------------------------------------
//
//          GUI FUNCTIONS:
//
//----------------------------------------------------------------------------


//---------
//-- Converts a QString to a standard string

string qStringToString( QString qstr )
{
  string str = "";
  for( int i=0; i<qstr.length(); i++ )
    str +=  qstr.at(i).latin1();
  return str;
}

//---------
//-- Display a simple message box

void MsgBox( string str )
{
  QMessageBox::information(0, "...", str.c_str() );
}

//---------
//-- Display a tyes/no dialog box and return "true" if use clicks yes.

bool MsgBoxYesNo( QWidget *parent, string str )
{
  int result = QMessageBox::information( parent, "...", str.c_str(),
                                         QMessageBox::Yes, QMessageBox::No );
  return ( result == QMessageBox::Yes );
}

//---------
//-- Display an input dialog and return the string entered by the user.

string InputBoxString( QWidget *parent, string title, string label, string defaultStr )
{
  return qStringToString( QInputDialog::getText(title.c_str(), label.c_str(),
                                                QLineEdit::Normal, defaultStr.c_str()));
}


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
}


//------------------------
//-- Changes the Z slice by calling page up or page down

int edit_changeSelectedSlice( int changeZ, bool redraw )
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
  int numConts = imodObjectGetMaxContour(obj);
  if(enableUndo)
    undoContourAdditionCO( plug.view, numConts );    // REGISTER UNDO
  int newContPos = imodObjectAddContour( obj, newCont );
  return newContPos;
}


//------------------------
//-- Removes all contours in the object which have their delete flag set to 1

int edit_removeAllFlaggedContoursFromObj( Iobj *obj )
{
  Icont *cont;
  int numRemoved = 0;
  for( int c=imodObjectGetMaxContour(obj)-1; c>=0; c-- )
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


//------------------------
//-- Searches all contours and tries to find and select a point and contour
//-- with the given z value and within the specified distance of the x, y 
//-- coordinates and returns true if a point is actually found.
//-- NOTE: "distTolerance" represents the maximum distance (in tomogram pixels)
//--       the x, y coordinates must be from our point
//-- NOTE: This function is used (primarily) the user right-clicks the ZAP
//--       window to select a different point.

bool edit_selectContourPtNearCoords(float x, float y, int z, float distTolerance)
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  
  int nConts = imodObjectGetMaxContour(obj);
  
  for(int c=0; c<nConts; c++)    // for each contour:
  {
    Icont *cont = getCont(obj, c);
    if( imodContourZValue(cont)==z && !isEmpty(cont) )  // if on desired slice:
    {
      int nPoints = psize(cont);
      for(int p=0; p<nPoints; p++)         // for each point in that contour:
      {
        float xDist = ABS(imodContourGetPoint(cont,p)->x - x);
        float yDist = ABS(getPt(cont,p)->y - y);
        
              // if point is within threshold distance: select point and return true
        
        if ( xDist<=distTolerance && yDist<=distTolerance )     
        {    
          int objIdx, contIdx, ptIdx;
          imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
          imodSetIndex(imod, objIdx, c, p );
          return true;
        }
      }
    }
  }
  
  return false;          // (if no points found close enough were found) return false
}


//------------------------
//-- If the mouse is not on the same slice as the selected contour,
//-- the selected contour is copied to the current slice and returns true,
//-- otherwise returns false.

bool edit_copiedContIfDiffSlice( bool selectNewCont )
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( isContValid(cont) && imodContourZValue(cont) != (int)plug.mouse.z )
  {
    Icont *contNew = imodContourDup(cont);
    Ipoint shift;
    shift.x = 0;
    shift.y = 0;
    shift.z = plug.mouse.z - getZ( contNew );
    cont_translate( contNew, &shift );
    int objIdx, contIdx, ptIdx;
    imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
    int newContPos = edit_addContourToObj( getCurrObj(), contNew, true );
    imodSetIndex(imod, objIdx, newContPos, 0);
    imodContourDelete(contNew);
    
    wprint("Contour has been copied\n");
    return true;
  }
  
  return false;
}



//------------------------
//-- Commences a deform operation.
//-- If the currently selected contour is too far from the mouse click,
//-- the algorithm will determine if the user is trying to edit/deform
//-- a different contour - in which case it will set this as the
//-- current contour - if not then it will create a NEW contour
//-- around the deform circle.

void edit_executeDeformStart()
{
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  
//## DETERMINE IF USER IS TRYING TO EDIT THE CURRENT CONTOUR,
//## A DIFFERENT CONTOUR, OR START A NEW CONTOUR:
  
  bool suitableContourSelected = false;
  
  if( isContValid(cont) && imodContourZValue(cont) == plug.mouse.z )
  {
    float distFromCircleToCont = cont_minDistPtAndContourPts2D(&plug.mouse,cont,true) 
                                 - plug.draw_deformRadius;
    float maxDistFromCurrCont = MAX( plug.draw_deformRadius*3.0, 10.0 );
    
        // if user clicked within reasonable distance from current contour: use it
    if ( distFromCircleToCont <= maxDistFromCurrCont ) 
      suitableContourSelected = true;
  }
  if( suitableContourSelected == false )  // if no contour selected, or was too far away:
  {
    float minDist = MIN( plug.draw_deformRadius*2.0, plug.draw_deformRadius + 10.0 );
    if(edit_selectContourPtNearCoords(plug.mouse.x,plug.mouse.y,(int)plug.mouse.z,minDist))
    {    // try to find & select another contour on the same slice close to the mouse
      suitableContourSelected = true;
      cont = imodContourGet(imod);
    }
  }
  
//## (IF SUITABLE CONTOUR IS SELECTED) DEFORM CONTOUR BY PUSHING POINTS TO
//## EDGE OF DEFORM CIRCLE:
  
  if( suitableContourSelected && isContValid(cont) )
  {
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    edit_executeDeform();
  }

//## (IF NO SUITABLE CONTOUR WAS FOUND) ADD THE DEFORM CIRCLE AS A NEW CONTOUR
  
  else
  {
    Icont *newCont = imodContourNew();
    cont_generateCircle(newCont, plug.draw_deformRadius, 16, plug.mouse, false);
    int newContPos = edit_addContourToObj(obj, newCont, true);
    int objIdx, contIdx, ptIdx;
    imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
    imodSetIndex(imod, objIdx, newContPos, 0);
  }
  
}


//------------------------
//-- Executes a deform operation by calling the appropriate funtion depending
//-- on what mouse buttons are down.

void edit_executeDeform()
{
  if(plug.shiftDown)      // pinch
  {
    edit_executeDeformPinch( plug.mouse, plug.draw_deformRadius );
    return;
  }
  else                  // push
  {
    float mouseMoveDist = imodPointDistance( &plug.mousePrev, &plug.mouse );       
            // the distance the mouse moved from its previously recorded position 
    
    int numIntermediates = mouseMoveDist / float(plug.draw_deformRadius + 1.0f);
            // the number of extra circles we will have to add between the last
            // and current mouse position to deform smoothly.
    
    if( numIntermediates > 10 ) {
      wprint("You are moving the mouse to fast!\n");
      return;
    }
    for( int i=1; i<=numIntermediates; i++ )
    {
      float fractAlong = float(i) / float(numIntermediates+1);
      Ipoint intermediateMousePos =
        line_findPtFractBetweenPts2D( &plug.mousePrev, &plug.mouse, fractAlong );
      edit_executeDeformPush( intermediateMousePos, plug.draw_deformRadius );
    }
    edit_executeDeformPush( plug.mouse, plug.draw_deformRadius );
    return;
  }
}


//------------------------
//-- Executes an "push deform operation" for a "deform circle" (with
//-- radius "pda.draw_deformRadius") at the position of the mouse. 
//-- This tool is used to draw and modify contours more quickly
//-- than is possible with the a normal draw operation.
//-- 
//-- In this function, all points within the currently selected contour 
//-- which are ALSO inside the deform circle are pushed away to the edge
//-- of the deform circle. Points are also removed/added if too
//-- close together/far apart.

void  edit_executeDeformPush( Ipoint center, float radius )
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( isContValid(cont) && imodContourZValue(cont) == center.z )
  {
    const int POINT_SHIFTED = -1;
    cont_addPtsCrude( cont, radius/3.0f, isContClosed(obj,cont) );         
                  // add extra points to ensure circle does not lie "between points"
    float radiusSq = radius*radius;
    
    //## FOR EACH POINT: IF IT'S IN THE DEFORM CIRCLE SHIFT IT TO 
    //## EDGE OF CIRCLE AND MARK IT AS "POINT_SHIFTED"
    
    for (int i=0; i<psize(cont); i++)
    {
      float distFromCenterSq = line_distBetweenPts2DSq( &center, getPt(cont,i) );
      if ( distFromCenterSq < radiusSq )    // if current pt is within deform circle:
      {
        float distFromCenter = sqrt(distFromCenterSq);
        if( distFromCenter==0 )
        {
          imodPointDelete(cont, i);
          continue;            // was "return" before
        }
                // moves point in a direct line from the centre of the
                // circle to the edge of the circle
                // and mark it as "shifted"
        
        float fractToShiftPt = radius / distFromCenter;      
        *getPt(cont,i) = line_findPtFractBetweenPts2D(&center, getPt(cont,i),
                                                      fractToShiftPt);
        getPt(cont,i)->z = POINT_SHIFTED;
      }
    }
    
    
    //## FOR EACH POINT: DETERMINE IF IT'S BEEN SHIFTED AND WETHER IT'S
    //## NECCESSARY TO ADD OR REMOVE POINTS EITHER SIDE OF IT
    
    
    int extra = isContClosed(obj, cont) ? 0 : -1;
    for (int i=0; i<psize(cont)+extra; i++)
    {
      bool thisPtShifted = ( getPt(cont,i)->z   == POINT_SHIFTED);
      bool nextPtShifted = ( getPt(cont,i+1)->z == POINT_SHIFTED);
      
      if( thisPtShifted || nextPtShifted )  // if the current pt OR next pt was shifted:
      {
        float line_distBetweenPts = imodPointDistance( getPt(cont,i), getPt(cont,i+1) );
        
        if( line_distBetweenPts > radius*5 )
          wprint("CRAP!!!!\n");
        
        // if BOTH points have been shifted: add a point in the middle
        
        if     ( thisPtShifted && nextPtShifted )   
        {
          if ( line_distBetweenPts > plug.draw_smoothMinDist )
          {
            Ipoint newPt = line_getPtHalfwayBetween( getPt(cont,i), getPt(cont,i+1) );
            float distFromCenter = imodPointDistance( &center, &newPt );
            if (distFromCenter==0)
              continue;
            float fractToShiftPoint = radius / distFromCenter;
            newPt = line_findPtFractBetweenPts2D( &center, &newPt, fractToShiftPoint );
            newPt.z = POINT_SHIFTED;
            
            imodPointAdd( cont, &newPt, i+1 );
            i--;
          }
        }
        else if( thisPtShifted && !nextPtShifted )
        {
          if ( line_distBetweenPts < plug.draw_smoothMinDist ) {
            int idxToRemove = (i+1) % psize(cont);
            imodPointDelete(cont, idxToRemove);    // remove next point
            i--;
          }
        }
        else if( nextPtShifted && !thisPtShifted )
        {
          if ( line_distBetweenPts < plug.draw_smoothMinDist ) {
            imodPointDelete(cont, i);              // remove current point
            i--;
          }
        }
      }
    }
    
    changeZValue( cont, (int)center.z );    
  }
}


//------------------------
//-- Executes an "pinch deform operation" for a "deform circle" (with
//-- radius "pda.draw_deformRadius") at the position of the mouse. 
//-- This function is almost identical in structure to edit_executeDeform(), 
//-- execept that points within the deform circle are pulled TOWARDS
//-- the center of the circle, rather than being pushed to the edge.

void edit_executeDeformPinch( Ipoint center, float radius )
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  
  if( isContValid(cont) && imodContourZValue(cont) == center.z )
  {
        // add extra points to ensure circle does not lie "between points"
        // (i.e. overlapping a line without touching any points)
    
    cont_addPtsCrude( cont, radius/3.0f, isContClosed(obj,cont) );
    float radiusSq = radius*radius;
    
    //## FOR EACH POINT: IF IT'S IN THE DEFORM CIRCLE SHIFT IT TOWARDS THE MIDDLE
    //## OF CIRCLE AND MARK IT AS "POINT_SHIFTED"
    
    for (int i=0; i<psize(cont); i++)
    {
      float distFromCenterSq = line_distBetweenPts2DSq( &center, getPt(cont,i) );
      if ( distFromCenterSq < radiusSq )    // if current pt is within deform circle:
      {
        if( distFromCenterSq==0 )
          continue;
        float distFromCenter = sqrt(distFromCenterSq);
        
                                    // moves point towards the center of the circle
        float fractToShiftPt = distFromCenter / radius;
        *getPt(cont,i) = line_findPtFractBetweenPts2D( &center, getPt(cont,i),
                                                       fractToShiftPt );
      }
    }
    imodContourUnique( cont );
  }
  
}



//------------------------
//-- Compleles a deform action by making the contour simple
//-- (so it doesn't cross itself) and reducing points if specified

void edit_executeDeformEnd()
{
  edit_makeCurrContSimple();
  edit_deleteCurrContIfTooSmall();
  
  if (plug.draw_reducePts)
    edit_reduceCurrContour();
  
  undoFinishUnit( plug.view );        // FINISH UNDO
}

//------------------------
//-- Compleles a join action by merging the contour
//-- with any other contours it touches.... and breaking it into
//-- multiple contours if a contour was split apart.

void edit_executeJoinEnd()
{
  edit_reduceCurrContour();
  edit_joinCurrContWithAnyTouching();
  edit_breakCurrContIntoSimpleContsAndDeleteSmallest();
  edit_makeCurrContSimple();
  edit_deleteCurrContIfTooSmall();
  
  if (plug.draw_reducePts)
    edit_reduceCurrContour();
  
  undoFinishUnit( plug.view );        // FINISH UNDO
}

//------------------------
//-- Inverses the order of points or reorders the point in the current contour.

void edit_inversePointsInContour( bool reorder )
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( isContValid(cont) )
  {
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    if(reorder)
    {
      int objIdx, contIdx, ptIdx;
      imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
      cont_reorderPtsToStartAtIdx(cont, ptIdx);
      imodSetIndex(imod, objIdx, contIdx, 0);
    }
    else
    {
      int objIdx, contIdx, ptIdx;
      imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
      imodel_contour_invert( cont );
      imodSetIndex(imod, objIdx, contIdx, psize(cont)-ptIdx-1 );
    }
    undoFinishUnit( plug.view );            // FINISH UNDO
    ivwRedraw( plug.view );
  }
}

//------------------------
//-- Tries to reduce the number of points in the current contour
//-- and returns true if the contour is changed

bool edit_reduceCurrContour()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( isContValid(cont) )
  {
    int nPointsBefore = psize( cont );
    cont_reducePtsMinArea( cont, plug.draw_reducePtsMaxArea, isContClosed(obj,cont) );
    int nPointsAfter = psize( cont );
    return ( nPointsBefore != nPointsAfter );
  }
  return false;
}

//------------------------
//-- Tries to smooth and increase the number of points in the
//-- current contour and returns true if the contour is changed

bool edit_smoothCurrContour()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( isContValid(cont) )
  {
    if( !isContClosed(obj,cont) )
      wprint("NOT CLOSED!");
    int nPointsBefore = psize( cont );
    cont_addPtsSmooth( cont, plug.draw_smoothMinDist, plug.draw_smoothTensileFract,
                       isContClosed(obj,cont) );
    int nPointsAfter = psize( cont );
    return ( nPointsBefore != nPointsAfter );
  }
  return false;
}


//------------------------
//-- Erases all contours in the given radius and returns the
//-- number of contours removed.

bool edit_doesBBoxTouchCircle( Ipoint *ll, Ipoint *ur, Ipoint *center, float radius )
{
  if( center->z < ll->z || center->z > ur->z )
    return false;
  
  return (point_distToBBox2D(center, ll, ur) < radius);
}



//------------------------
//-- Erases all contours in the given radius and returns the
//-- number of contours removed, and creates undo if contours
//-- were removed.

int edit_eraseContsInCircle( Ipoint center, float radius )
{
  Imod *imod = ivwGetModel(plug.view);
  
  float radiusSq = (radius*radius);
  
  int selObjIdx, selContIdx, selPtIdx;
  imodGetIndex( imod, &selObjIdx, &selContIdx, &selPtIdx );
  int numRemoved = 0;
  
  for (int o=0; o<imodGetMaxObject(imod); o++ )
  {
    imodSetIndex( imod, o, 1, 1);
    Iobj *obj  = imodObjectGet(imod);
    
    if (!isObjectValidAndShown(obj))
      continue;
    
    for(int c=0; c<imodObjectGetMaxContour(obj); c++)    // for each contour:
    {
      Icont *cont = getCont(obj, c);
      
      if( getZ(cont) == center.z )        // if contour on desired slice:
      {
        for(int p=0; p<psize(cont); p++)    // for each point in that contour:
        {
          float distSq = line_distBetweenPts2DSq( &center, getPt(cont,p) );
          if ( distSq <= radiusSq )           // if point is inside the circle:
          {
            undoContourRemoval( plug.view, o, c );    // REGISTER UNDO
            imodObjectRemoveContour( obj, c );        // delete the contour
            numRemoved++;
            if( o == selObjIdx && c < selContIdx )
              selContIdx--;
            break;
          }
        }
      }
    }
  }
  
  if(numRemoved)
    undoFinishUnit( plug.view );          // FINISH UNDO
  
  //imodSetIndex( imod, selObjIdx, selContIdx, selPtIdx );
  imodSetIndex( imod, selObjIdx, -1, -1 );
  
  return numRemoved;
}


//------------------------
//-- Erases all points in the given radius, returns the number of
//-- points removed and creates undo if points were removed.

int edit_erasePointsInCircle( Ipoint center, float radius )
{
  Imod *imod = ivwGetModel(plug.view);
  
  float radiusSq = (radius*radius);
  int selObjIdx, selContIdx, selPtIdx;
  imodGetIndex( imod, &selObjIdx, &selContIdx, &selPtIdx );
  
  int numPtsRemoved = 0;
  
  for (int o=0; o<imodGetMaxObject(imod); o++ )
  {
    imodSetIndex( imod, o, 1, 1);
    Iobj *obj  = imodObjectGet(imod);
    
    for(int c=0; c<imodObjectGetMaxContour(obj); c++)    // for each contour:
    {
      Icont *cont = getCont(obj, c);
      
      if ( isObjClosed(obj) )
      {
        if( getZ(cont) == center.z )    // if contour is on desired slice:
        {
          float minDistToPt = cont_minDistPtAndContourPts2D( &center, cont, false );
          if( minDistToPt <= radius )
          {
            imodSetIndex( imod, o, c, 0);
            undoContourDataChgCC( plug.view );
            numPtsRemoved += cont_removePointsInCircle( cont, &center, radius, false );
          }
        }
      }
      else
      {
        Ipoint ll, ur;
        imodContourGetBBox( cont, &ll, &ur );
        
        if ( edit_doesBBoxTouchCircle(&ll, &ur, &center, radius) )
        {
          imodSetIndex( imod, o, c, 0);
          undoContourDataChgCC( plug.view );
          numPtsRemoved += cont_removePointsInCircle( cont, &center, radius, true );
        }
      }
    }
  }
    
  if(numPtsRemoved)
    undoFinishUnit( plug.view );          // FINISH UNDO
  
  imodSetIndex( imod, selObjIdx, -1, -1 );    // ensures Zap doesn't jump to new slice
  
  return numPtsRemoved;
}




//------------------------
//-- Finds the first contour withing the circle, removes any of it's
//-- points within the circle and breaks the contour apart either side,
//-- and if any changes was made it creates an undo and returs true.

bool edit_breakPointsInCircle( Ipoint center, float radius )
{
  Imod *imod = ivwGetModel(plug.view);
  
  float radiusSq = (radius*radius);
  int numPtsRemoved = 0;
  
  int selObjIdx, selContIdx, selPtIdx;
  imodGetIndex( imod, &selObjIdx, &selContIdx, &selPtIdx );
  
  for (int o=0; o<imodGetMaxObject(imod); o++ )
  {
    imodSetIndex( imod, o, 1, 1);
    Iobj *obj  = imodObjectGet(imod);
    
    for(int c=0; c<imodObjectGetMaxContour(obj); c++)    // for each contour:
    {
      Icont *cont = getCont(obj, c);
      
      if( getZ(cont) == center.z )                      // if contour on desired z slice:
      {
        float minDistToPt = cont_minDistPtAndContourPts2D( &center, cont, false );
        if( minDistToPt <= radius )
        {
          imodSetIndex( imod, o, c, 1);
          
          if( isContClosed(obj,cont) )    // if contour is closed:
          {
            float closestDist;
            Ipoint closestPt;
            int closestPtIdx;
            cont_findClosestPtInContToGivenPt( &center, cont, &closestDist,
                                               &closestPt, &closestPtIdx );
            cont_reorderPtsToStartAtIdx( cont, closestPtIdx );
                  // reorder points so ends are broken either side of circle
          }
          
          vector<IcontPtr> contSegments;
          numPtsRemoved += cont_breakContByCircle(cont,contSegments,&plug.mouse,
                                                  plug.draw_deformRadius);
          
          for (int i=0; i<contSegments.size(); i++)   // make any new contours open
            setOpenFlag( contSegments[i].cont, 1 );
          
          if( contSegments.size() == 1 )
          {
            undoContourDataChgCC(plug.view);                  // REGISTER UNDO
            cont_copyPoints( contSegments[0].cont, cont, true );
          }
          else if ( contSegments.size() > 1 )
          {
            undoContourDataChgCC(plug.view);                  // REGISTER UNDO
            cont_copyPoints( contSegments[0].cont, cont, true );
            for( int i=1; i<contSegments.size(); i++ )
              edit_addContourToObj( obj, contSegments[i].cont, true );
          }
          
          undoContourPropChgCC(plug.view);                    // REGISTER UNDO
          setOpenFlag( cont, 1 );
          
          undoFinishUnit( plug.view );                        // FINISH UNDO
          imodSetIndex( imod, selObjIdx, -1, -1 );
          deleteContours(contSegments);
          return true;
        }
      }
    }
  }
  
  imodSetIndex( imod, selObjIdx, selContIdx, selPtIdx );
  return false;
}



//------------------------
//-- Takes the current contour and, if it is NOT simple (i.e. if it overlaps itself)
//-- then it breaks it apart into simple contours and deletes any small fragments
//-- (in this case any contour with < 5 points)

void edit_breakCurrContIntoSimpleContsAndDeleteSmallest ()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  if( isContValid(cont) )
  {
    if( cont_isSimple(cont) )      // if contour doesn't overlap itself: return
      return;
    
    //## BREAK CONTOUR INTO SIMPLE CONTOURS:
    vector<IcontPtr> conts;
    cont_breakIntoSimple( conts, cont );    // breaks contour into simple contours.
    
    //## GENERALIZE THESE CONTOURS AND DELETE ANY REALLY SMALL ONES:
    
    for( int i=0; i<(int)conts.size(); i++ )    // for each contour: reduce points
      cont_reducePtsMinArea( conts[i].cont, plug.draw_reducePtsMaxArea,
                             isContClosed(obj,cont) ); 
    
    
    for( int i=0; i<(int)conts.size(); i++ )    // for each contour: 
      if( psize( conts[i].cont ) < 5 )  // if too few points: delete it
      {
        eraseContour( conts, i );
        i--;
      }
    
    //## ADD NEW CONTOURS TO GRID:
    
    if( conts.size() > 1 )    // if there is more than one contour:
    {
      undoContourRemovalCC(plug.view);
      imodObjectRemoveContour(obj, contIdx);
      for( int i=0; i<(int)conts.size(); i++ )
      {
        edit_addContourToObj( obj, conts[i].cont, true );
      }
      int contToSelect = imodObjectGetMaxContour(obj) - conts.size();
      imodSetIndex(imod, objIdx, contToSelect, ptIdx);
    }
    
    deleteContours(conts);
  }
}


//------------------------
//-- Makes the current contour simple

void edit_makeCurrContSimple ()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( isContValid(cont) ) {
    cont_makeSimple( cont );
  }
}

//------------------------
//-- Deletes the current contour if it "too small"

void edit_deleteCurrContIfTooSmall()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  bool isTooSmall = imodContourArea(cont) < MAX( plug.draw_reducePtsMaxArea*3.0, 10.0 );
  
  if( isTooSmall )
  {
    undoContourDataChgCC( plug.view );          // REGISTER UNDO
    imodContourDefault( cont );
  }
}


//------------------------
//-- Takes current contour and joins it with any other contours
//-- in the same (current) object which it makes contact with

void edit_joinCurrContWithAnyTouching()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( !isContValid(cont) )
    return;
  
  int zSlice = imodContourZValue( cont );
  
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  for(int i=0; i<imodObjectGetMaxContour( obj ); i++)
  {
    Icont *contCompare = getCont(obj, i);
    
    if( i==contIdx || imodContourZValue(contCompare) != zSlice || isEmpty(contCompare) )
      continue;
    
    if( cont_doCountoursCross( cont, contCompare, true, true ) )
    {
      undoContourDataChgCC( plug.view );          // REGISTER UNDO
      cont_getOuterUnionPolygon( cont, cont, contCompare );
      
      imodSetIndex(imod, objIdx, i, 0);
      undoContourDataChgCC( plug.view );          // REGISTER UNDO
      imodContourDefault( contCompare );
      imodSetIndex(imod, objIdx, contIdx, ptIdx);
      
      break;      // we only want to do one at a time for now.
    }
  }
}


//------------------------
//-- Finds and selects the next contour past the selected contour which
//-- overlaps (contours cross) another contour in the model

bool edit_selectNextOverlappingCont()
{
  Imod *imod = ivwGetModel(plug.view);
  
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  //## FIND PROBLEM CONTOURS:
  
  for (int o=objIdx; o<imodGetMaxObject(imod); o++)         // for each object:
  {
    imodSetIndex( imod, o, 1, 1);
    Iobj *objO  = imodObjectGet(imod);
    
    for (int i=(contIdx+1); i<imodObjectGetMaxContour(objO); i++)   // for each contour:
    {
      Icont *contI = getCont( objO, i );
      if( isEmpty( contI ) )
        continue;
      for (int p=o; p<imodGetMaxObject(imod); p++)     // for each object ahead of that:
      {
        imodSetIndex( imod, p, 1, 1);
        Iobj *objP  = imodObjectGet(imod);
        
        int j = (o==p) ? i+1 : 0;                       // starting contour.
        for(; j<imodObjectGetMaxContour(objP); j++)     // for each contour ahead:
        {
          Icont *contJ = getCont( objP, j );
          
          if( ( getZ(contJ) == getZ(contI) )  )             // if on same slice:
          {
            int pt1Cross, pt2Cross;
            if( cont_doContsTouch( contI, contJ ) &&
                cont_doCountoursCrossAndWhere( contI, contJ,
                                               isContClosed(objO,contI),
                                               isContClosed(objP,contJ),
                                               &pt1Cross, &pt2Cross ) )
            {
              imodSetIndex( imod, o, i, ((pt1Cross+1)%psize(contI)) );
              
              if( cont_isEqual(contJ,contI) )
                wprint("\nWARNING: Two identical contours found!");
              else
                wprint("\nOverlapping contour found!\n");
              return (true);
            }
          }
        }
      }
    }
  }
  
  imodSetIndex( imod, objIdx, contIdx, ptIdx );
  
  if( objIdx == 0 && contIdx == 0)
    wprint("\nNo overlapping contours in model !\n");
  else {
    wprint("\nNo overlapping contours found past the selected contour ... ");
    wprint("(select the contour 1 of object 1 to check all contours)\n");
  }
  return (false);
}



