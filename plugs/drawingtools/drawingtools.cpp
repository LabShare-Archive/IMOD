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
    Revision 1.14  2008/07/14 08:55:51  tempuser
    Minor changes

    Revision 1.13  2008/07/10 07:43:56  tempuser
    Added functionality to sort and advance through [y] contours and points

    Revision 1.12  2008/04/07 03:12:12  tempuser
    Added free()
    
    Revision 1.11  2008/04/04 01:15:36  tempuser
    Moved gui functions elsewhere

    Revision 1.10  2008/03/17 07:22:37  tempuser
    Improved reduce and smooth contour options

    Revision 1.9  2008/03/12 02:24:34  tempuser
    Minor modifications

    Revision 1.8  2008/03/11 09:35:47  tempuser
    Added save vals

    Revision 1.7  2008/03/05 10:29:00  tempuser
    Cleaned code

    Revision 1.6  2008/03/03 06:48:01  tempuser
    Modified makefile and slice changing

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
#include "qt_dialog_customizable.h"
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
  
  
  if(ctrl &&             // if the control key is down I typically want IMOD to handle it
     keysym != Qt::Key_X && keysym != Qt::Key_V && keysym != Qt::Key_V )
    return 0;
  
  if( !plug.useNumKeys && keysym >= Qt::Key_1 && keysym <= Qt::Key_5  )
    return 0;
  
  switch(keysym)
  {
    case Qt::Key_Q:
      plug.window->changeDeformCircleRadius( (shift) ? -5 : -1 );
      plug.window->drawExtraObject(true);
      break;
    case Qt::Key_W: 
      plug.window->changeDeformCircleRadius( (shift) ? 5 : 1 );
      plug.window->drawExtraObject(true);
      break;
    case Qt::Key_R:
      if(shift)
        return 0;
      plug.window->reduceCurrentContour();
      break;
    case Qt::Key_E:
      plug.window->smoothCurrentContour();
      break;
      
    case Qt::Key_D:
      if (shift || plug.dKeyBehav == DK_NONE)
        return 0;
      plug.window->executeDAction();
      break;
      
    case Qt::Key_A:
      if(shift)
        return 0;
      plug.window->selectNextOverlappingContour();
      break;
    case Qt::Key_I:
      edit_inversePointsInContour(shift);
      break;
    case Qt::Key_Y:
      edit_goToContNextBiggestFindVal(shift,false,true);    // recalculates
      break;
    case Qt::Key_B:
      edit_goToContNextBiggestFindVal(shift,true,false,(shift)?FLOAT_MAX:FLOAT_MIN );
      break;
      
    //case Qt::Key_T:                  // temporary testing purposes - comment out
    //  plug.window->test();
    //  break;
      
    case Qt::Key_X:
      if(ctrl)
        plug.window->cut();
      else
        return 0;
      break;
    case Qt::Key_C:
      if(ctrl)
        plug.window->copy();
      else
        return 0;
      break;
    case Qt::Key_V:
      if(ctrl)
        plug.window->paste(!shift);
      else
        return 0;
      break;
      
      
    case Qt::Key_PageUp:
      if(shift)
        return 0;
      else
        edit_changeSelectedSlice( plug.pgUpDownInc,false, false );
      break;
      
    case Qt::Key_PageDown:
      if(shift)
        return 0;
      else
        edit_changeSelectedSlice( -plug.pgUpDownInc,false, false );
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
    plug.draw_reducePts           = 0;
    plug.draw_reducePtsMinArea    = 0.5;
    plug.draw_smoothMinDist       = 5;
    plug.draw_smoothTensileFract  = 0.5;
    plug.draw_deformRadius        = 30.0;
    
    plug.wheelBehav               = WH_DEFORMCIRCLE;
    plug.dKeyBehav                = DK_TOEND;
    plug.pgUpDownInc              = 1;
    plug.useNumKeys               = true;
    plug.markTouchedContsAsKey    = false;
    plug.wheelResistance          = 100;
    plug.showMouseInModelView     = false;
    plug.selectedAction           = 0;
    plug.sortCriteria             = SORT_NUMPTS;
    plug.findCriteria             = SORT_NUMPTS;
    
    plug.sortCriteriaOfVals       = -1;
    Ipoint origin;
    setPt( &origin, 0,0,0);
    plug.copiedCont = imodContourNew();
    cont_generateCircle( plug.copiedCont, 30.0f, 20, origin, false );
        // puts a circle in copiedCont until the user copies his own contour
    
    plug.window->loadSettings();
    
    plug.initialized = true;
  }
  plug.view = inImodView;
  ivwTrackMouseForPlugs(plug.view, 1);
  ivwGetImageSize(inImodView, &plug.xsize, &plug.ysize, &plug.zsize);
  
  //## INITIALIZE EXTRA OBJECT:
  
  plug.extraObjNum = ivwGetFreeExtraObjectNumber(plug.view);
  Iobj *xobj = ivwGetAnExtraObject(plug.view, plug.extraObjNum);
  imodObjectSetColor(xobj, 1.0, 0.0, 0.0);
  imodObjectSetValue(xobj, IobjFlagClosed, 1);
  ivwClearAnExtraObject(plug.view, plug.extraObjNum);  
  
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
    QWheelEvent *wheelEvent = static_cast<QWheelEvent*>(event);
    float scrollAmount    = fDivide( wheelEvent->delta(), float(plug.wheelResistance) );
    int   scrollAmountInt = floor(scrollAmount);
    
    /*
    //## ZOOM:
    
    if( wheelEvent->state() & Qt::ControlButton )     // if [ctrl] is down:
    {
      float zoom;
      int error = ivwGetTopZapZoom(plug.view, &zoom);
      if( error != 1 )
      {
        //float zoomChange = (scrollAmount < 0) ? 0.9f : 1.1f;
        float zoomChange = 1 + MAX(-0.5f,scrollAmount*0.05f);
        float newZoom = ABS(zoom * zoomChange); 
        ivwSetTopZapZoom(plug.view, newZoom);    //%%%% WILL ASK DAVID TO CREATE
        ivwRedraw(plug.view);
        
        //QKeyEvent *newEvent = new QKeyEvent(QEvent::KeyRelease,Qt::Key_Plus,61,61,"+");
        //ivwControlKey(0, newEvent);
      }
      return 0;
    }
    */
    
    switch( plug.wheelBehav )
    {
      case(WH_DEFORMCIRCLE):
      {
        if( plug.drawMode == DM_DEFORM
            || plug.drawMode == DM_JOIN
            || plug.drawMode == DM_ERASER )
        {
          plug.window->changeDeformCircleRadius( scrollAmount, plug.shiftDown );
          plug.window->drawExtraObject(true);
        }
        break;
      }
      
      case(WH_SLICES):
      {
        edit_changeSelectedSlice( scrollAmountInt, true );
        break;
      }
      
      case(WH_CONTS):
      {
        if( !isCurrObjValidAndShown() )
          return 0;
        Imod *imod  = ivwGetModel(plug.view);
        Iobj *obj   = getCurrObj();
        int objIdx, contIdx, ptIdx;
        imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
        cycleIntWithinRange( contIdx, 0, csize(obj)-1, scrollAmountInt );
        imodSetIndex(imod, objIdx, contIdx, 0);
        plug.window->drawExtraObject(false);
        ivwRedraw(plug.view);
        break;
      }
      
      case(WH_PTSIZE):
      {
        Imod *imod  = ivwGetModel(plug.view);
        if( !isCurrObjValidAndShown() || imodPointGet(imod)==NULL )
          return 0;
        Iobj *obj   = getCurrObj();
        Iobj *cont  = getCurrCont();
        int objIdx, contIdx, ptIdx;
        imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
        float ptSize = imodPointGetSize(obj,cont,ptIdx);
        ptSize = MAX(0.0f, ptSize+scrollAmount);
        imodPointSetSize(cont,ptIdx,ptSize);
        plug.window->drawExtraObject(false);
        ivwRedraw(plug.view);
        break;
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
  
      // NOTE if we get to here we have dealt with the action, so re redraw and return 1
  
  ivwDraw( plug.view, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC );    
  return (1);
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
                "Automatically applies smoothing to any contour drawn with the "
                "\n'deform' and 'join' tools upon release of the mouse button");
  gridLayout1->addMultiCellWidget(reducePtsCheckbox, 1, 1, 0, 1);
  
  
  QString minAreaStr = 
    "When a contour is reduced: wherever three consecutive points "
    "\nform a triangular area less than this many pixels squared, "
    "\nthe middle point is removed."
    "\n"
    "\nRECOMMENDED VALUE: 0.5";
  
  lblMinArea = new QLabel("reduction min area:", grpOptions);
  lblMinArea->setFocusPolicy(QWidget::NoFocus);
  QToolTip::add(lblMinArea, minAreaStr);
  gridLayout1->addWidget(lblMinArea, 2, 0);
  
  QBoxLayout *box1 = new QBoxLayout(QBoxLayout::LeftToRight);
  fMinAreaSpinner = (FloatSpinBox*)diaLabeledSpin(1,1,200,1,"",grpOptions,box1);
  diaSetSpinBox( (QSpinBox*)fMinAreaSpinner, int(plug.draw_reducePtsMinArea*10) );
  ((QSpinBox *)fMinAreaSpinner)->setFocusPolicy(QWidget::ClickFocus);
  QObject::connect( (QSpinBox*)fMinAreaSpinner,SIGNAL(valueChanged(int)),this,
                    SLOT(changeMinArea(int)));
  QToolTip::add( (QSpinBox*)fMinAreaSpinner, minAreaStr);
  gridLayout1->addLayout(box1, 2, 1);
  
  
  QString smoothPtsDistStr = 
    "When a contour is smoothed: wherever two consecutive points are"
    "\ngreater than this many pixels apart, point(s) will be added between them. "
    "\n"
    "\nRECOMMENDED VALUE: 5";
  
  lblSmoothPtsDist = new QLabel("smooth point dist:", grpOptions);
  lblSmoothPtsDist->setFocusPolicy(QWidget::NoFocus);
  QToolTip::add(lblSmoothPtsDist, smoothPtsDistStr);
  gridLayout1->addWidget(lblSmoothPtsDist, 3, 0);
  
  QBoxLayout *box2 = new QBoxLayout(QBoxLayout::LeftToRight);
  fSmoothPtsDist = (FloatSpinBox*)diaLabeledSpin(0,1,50,1,"",grpOptions,box2);
  diaSetSpinBox( (QSpinBox*)fSmoothPtsDist, plug.draw_smoothMinDist*1 );
  ((QSpinBox *)fSmoothPtsDist)->setFocusPolicy(QWidget::ClickFocus);
  QObject::connect( (QSpinBox*)fSmoothPtsDist,SIGNAL(valueChanged(int)),this,
                   SLOT(changeSmoothPtsDist(int)));
  QToolTip::add( (QSpinBox*)fSmoothPtsDist, smoothPtsDistStr);
  gridLayout1->addLayout(box2, 3, 1);
  
  
  QString smoothTensileFractStr = 
    "When a contour is smoothed: a cardinal spline agorithm is used "
    "\nwith a tensile fraction of this value. This value dictates how "
    "\n'curvy' (sensitive to direction change) the contour will be when "
    "\npoints are added --> 0 = straight line, 2 = very curvy."
    "\n"
    "\nRECOMMENDED VALUE: 0.5";
  
  lblSmoothTensileFract = new QLabel("smooth tensile value:", grpOptions);
  lblSmoothTensileFract->setFocusPolicy(QWidget::NoFocus);
  QToolTip::add(lblSmoothTensileFract, smoothTensileFractStr);
  gridLayout1->addWidget(lblSmoothTensileFract, 4, 0);
  
  QBoxLayout *box3 = new QBoxLayout(QBoxLayout::LeftToRight);
  fSmoothTensileFract = (FloatSpinBox*)diaLabeledSpin(1,0,20,1,"",grpOptions,box3);
  diaSetSpinBox( (QSpinBox*)fSmoothTensileFract, plug.draw_smoothTensileFract*10 );
  ((QSpinBox *)fSmoothTensileFract)->setFocusPolicy(QWidget::ClickFocus);
  QObject::connect( (QSpinBox*)fSmoothTensileFract,SIGNAL(valueChanged(int)),this,
                    SLOT(changeSmoothTensileFract(int)));
  QToolTip::add( (QSpinBox*)fSmoothTensileFract, smoothTensileFractStr);
  gridLayout1->addLayout(box3, 4, 1);
  mLayout->addWidget(grpOptions);
  
  
  //## Object
  
  grpActions = new QGroupBox("Smoothing Actions:", this);
  grpActions->setFocusPolicy(QWidget::NoFocus);
  grpActions->setMargin(GROUP_MARGIN);
  

  vboxLayout1 = new QVBoxLayout(grpActions);
  vboxLayout1->setSpacing(LAYOUT_SPACING);
  vboxLayout1->setMargin(LAYOUT_MARGIN);
  vboxLayout1->addItem( new QSpacerItem(1,SPACER_HEIGHT) );
  
  reduceContsButton = new QPushButton("Reduce Contours [r]", grpActions);
  reduceContsButton->setFocusPolicy(QWidget::NoFocus);
  connect(reduceContsButton, SIGNAL(clicked()), this, SLOT(reduceConts()));
  QToolTip::add(reduceContsButton,
                "Reduces (removes points from) a range of contours "
                "\nin the current object");
  vboxLayout1->addWidget(reduceContsButton);
  
  smoothContsButton = new QPushButton("Smooth Contours [e]", grpActions);
  smoothContsButton->setFocusPolicy(QWidget::NoFocus);
  connect(smoothContsButton, SIGNAL(clicked()), this, SLOT(smoothConts()));
  QToolTip::add(smoothContsButton,
                "Smooths (adds points to) a range of contours "
                "\nin the current object... (use with caution)");
  vboxLayout1->addWidget(smoothContsButton);
  
  mLayout->addWidget(grpActions);
  
  
  //## Extra Buttons
  
  widget1 = new QWidget(this);
  
  gridLayout2 = new QGridLayout(widget1);
  gridLayout2->setSpacing(LAYOUT_SPACING);
  gridLayout2->setMargin(LAYOUT_MARGIN);
  
  moreActionsButton = new QPushButton("More Actions", widget1);
  connect(moreActionsButton, SIGNAL(clicked()), this, SLOT(moreActions()));
  QToolTip::add(moreActionsButton,
                "Contains several other actions I didn't want to sqeeze "
                "into this window");
  gridLayout2->addWidget(moreActionsButton, 0, 0);
  
  moreSettingsButton = new QPushButton("More Settings", widget1);
  connect(moreSettingsButton, SIGNAL(clicked()), this, SLOT(moreSettings()));
  QToolTip::add(moreSettingsButton,
                "Contains several other settings I didn't want to sqeeze "
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

bool DrawingTools::drawExtraObject( bool redraw )
{
  if ( !plug.window )
    return false;
  
  //## CLEAR EXTRA OBJECT:
  
  Iobj *xobj = ivwGetAnExtraObject(plug.view, plug.extraObjNum);
  imodObjectSetValue(xobj, IobjFlagExtraInModv, (plug.showMouseInModelView)?1:0);
  ivwClearAnExtraObject(plug.view, plug.extraObjNum);
  Icont *xcont = imodContourNew();
  if ( !xobj || !xcont )
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
    
    imodPointAppendXYZ( xcont, xmin, ymin, currSlice );
    imodPointAppendXYZ( xcont, xmax, ymin, currSlice );
    imodPointAppendXYZ( xcont, xmax, ymax, currSlice );
    imodPointAppendXYZ( xcont, xmin, ymax, currSlice );
    
    imodPointAppendXYZ( xcont, xmin,     y, currSlice );
    imodPointAppendXYZ( xcont, xmax,     y, currSlice );
    imodPointAppendXYZ( xcont, xmin,     y, currSlice );
		
    imodContourSetFlag(xcont, ICONT_CURSOR_LIKE | ICONT_MMODEL_ONLY, 1);
    imodObjectAddContour(xobj, xcont);
    free(xcont);
    
		if( redraw )
			ivwRedraw( plug.view );
		return true;
  }
  
  //## CONSTRUCT REFERENCE CONTOUR:
  
  switch( plug.drawMode )
  {
  case (DM_NORMAL ):            // draw a tiny verticle line
    {
      imodPointAppendXYZ( xcont, x, y, z );
      imodPointAppendXYZ( xcont, x, y+1, z );
      break;
    }
  case(DM_DEFORM):            // draw deform circle
    {
      cont_generateCircle( xcont, radius, 100, plug.mouse, true );
      if( plug.shiftDown )
      {
        imodPointAppendXYZ( xcont, x+radius, y, z-1);
        imodPointAppendXYZ( xcont, x+hRadius, y+qRadius, z-1 );
        imodPointAppendXYZ( xcont, x+hRadius, y+qRadius, z );
        imodPointAppendXYZ( xcont, x+qRadius, y, z );
        imodPointAppendXYZ( xcont, x+hRadius, y-qRadius, z );
        imodPointAppendXYZ( xcont, x+hRadius, y-qRadius, z-1 );
        
        imodPointAppendXYZ( xcont, x-hRadius, y-qRadius, z-1 );
        imodPointAppendXYZ( xcont, x-hRadius, y-qRadius, z );
        imodPointAppendXYZ( xcont, x-qRadius, y, z );
        imodPointAppendXYZ( xcont, x-hRadius, y+qRadius, z );
        imodPointAppendXYZ( xcont, x-hRadius, y+qRadius, z-1 );
        imodPointAppendXYZ( xcont, x-radius,  y, z-1);
      }
      break;
    }
  case(DM_JOIN):              // draw deform circle with plus sign in middle
    {
      cont_generateCircle( xcont, radius, 100, plug.mouse, true );
      
      imodPointAppendXYZ( xcont, x+radius,    y,      z-1  );
      imodPointAppendXYZ( xcont, x+hRadius,   y,      z-1 );
      imodPointAppendXYZ( xcont, x+hRadius,   y,      z );
      imodPointAppendXYZ( xcont, x-hRadius,   y,      z );
      imodPointAppendXYZ( xcont, x,           y,      z );
      imodPointAppendXYZ( xcont, x,           y+hRadius,  z );
      imodPointAppendXYZ( xcont, x,           y-hRadius,  z );;
      imodPointAppendXYZ( xcont, x,           y,      z );  
      imodPointAppendXYZ( xcont, x,           y,      z-1 );
      imodPointAppendXYZ( xcont, x+radius,    y,      z-1);  
      break;
    }
  case(DM_TRANSFORM):         // draw rectangle around current contour or next to mouse 
    {

      Icont *cont = imodContourGet( ivwGetModel(plug.view) );
      if( isContValid(cont) )
      {
        Ipoint ll, ur;
        imodContourGetBBox( cont, &ll, &ur);
        imodPointAppendXYZ( xcont, ll.x, ll.y, z-1 );
        imodPointAppendXYZ( xcont, ll.x, ll.y, z );
        imodPointAppendXYZ( xcont, ur.x, ll.y, z );
        imodPointAppendXYZ( xcont, ur.x, ur.y, z );
        imodPointAppendXYZ( xcont, ll.x, ur.y, z );
        imodPointAppendXYZ( xcont, ll.x, ll.y, z );
        imodPointAppendXYZ( xcont, ll.x, ll.y, z-1 );
        
        if(plug.but2Down)      // draw line from point clicked to mouse
        {
          imodPointAppendXYZ( xcont, plug.mouseDownPt.x, plug.mouseDownPt.y, z-1 );
          imodPointAppendXYZ( xcont, plug.mouseDownPt.x, plug.mouseDownPt.y, z );  
          imodPointAppendXYZ( xcont, x, y, z );
          imodPointAppendXYZ( xcont, x, y, z-1 );    
        }
        else if(plug.but3Down)    // draw line from center of contour to mouse
        {
          imodPointAppendXYZ( xcont, plug.centerPt.x, plug.centerPt.y, z-1 );
          imodPointAppendXYZ( xcont, plug.centerPt.x, plug.centerPt.y, z );  
          imodPointAppendXYZ( xcont, x, y, z );      
          imodPointAppendXYZ( xcont, x, y, z-1 );      
        }
      }
      else
      {
        float rectLen = 10.0f;
        float zapZoom = 1.0f;
        int noZap = ivwGetTopZapZoom(plug.view, &zapZoom);
        if( noZap != 1 )   // if there is a top window: determine pixel length
          rectLen = fDivide( 10.0f, zapZoom);
        imodPointAppendXYZ( xcont, x+1.0f*rectLen, y+1.0f*rectLen, z );
        imodPointAppendXYZ( xcont, x+2.0f*rectLen, y+1.0f*rectLen, z );
        imodPointAppendXYZ( xcont, x+2.0f*rectLen, y+1.5f*rectLen, z );
        imodPointAppendXYZ( xcont, x+1.0f*rectLen, y+1.5f*rectLen, z );
        imodPointAppendXYZ( xcont, x+1.0f*rectLen, y+1.0f*rectLen, z );
        imodPointAppendXYZ( xcont, x+1.0f*rectLen, y+1.0f*rectLen, z-1 );
      }
      
      break;
    }
  case(DM_ERASER):            // draw deform circle with a diagonal line through it
    {
      if( plug.but2Down || plug.but3Down )  {
        cont_generateCircle( xcont, radius*0.99f, 100, plug.mouse, true );
        cont_generateCircle( xcont, radius*0.98f, 100, plug.mouse, true );
      }
      cont_generateCircle( xcont, radius, 100, plug.mouse, true );
      
      imodPointAppendXYZ( xcont, x+hRadius, y, z-1);    
      imodPointAppendXYZ( xcont, x+hRadius, y+hRadius, z-1 );
      imodPointAppendXYZ( xcont, x+hRadius, y+hRadius, z );
      imodPointAppendXYZ( xcont, x-hRadius, y-hRadius, z );
      imodPointAppendXYZ( xcont, x-hRadius, y-hRadius, z-1 );
      imodPointAppendXYZ( xcont, x+hRadius, y, z-1);    
      if( plug.shiftDown )              // draw extra arrows on diagonal line
      {
        imodPointAppendXYZ( xcont, x+hRadius, y, z-1);    
        imodPointAppendXYZ( xcont, x+hRadius, y+qRadius, z-1 );
        imodPointAppendXYZ( xcont, x+hRadius, y+qRadius, z );
        imodPointAppendXYZ( xcont, x+hRadius, y+hRadius, z );
        imodPointAppendXYZ( xcont, x-hRadius, y-hRadius, z );
        imodPointAppendXYZ( xcont, x-hRadius, y-qRadius, z );
        imodPointAppendXYZ( xcont, x-hRadius, y-qRadius, z-1 );
        imodPointAppendXYZ( xcont, x+hRadius, y, z-1);
      }
      break;
    }
  }
  
  imodContourSetFlag(xcont, ICONT_CURSOR_LIKE | ICONT_MMODEL_ONLY, 1);
  imodObjectAddContour(xobj, xcont);
  free(xcont);
  
  if( redraw )
    ivwDraw( plug.view, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC );
  
  return true;
}


//------------------------
//-- Clears all the contents of the extra object.

void DrawingTools::clearExtraObj()
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
//-- Loads most of the settings for DrawingTools from user preferences

void DrawingTools::loadSettings()
{
  
  double savedValues[NUM_SAVED_VALS];
  
  int nvals = prefGetGenericSettings("DrawingTools", savedValues, NUM_SAVED_VALS);
  
  if(nvals!=NUM_SAVED_VALS)
  {
    wprint("DrawingTools: Error loading saved values");
    return;
  }
  
  plug.drawMode                   = savedValues[0];
  plug.draw_reducePts             = savedValues[1];
  plug.draw_reducePtsMinArea      = savedValues[2];
  plug.draw_smoothMinDist         = savedValues[3];
  plug.draw_smoothTensileFract    = savedValues[4];
  plug.draw_deformRadius          = savedValues[5];
  plug.wheelBehav                 = savedValues[6];
  plug.dKeyBehav                  = savedValues[7];
  plug.pgUpDownInc                = savedValues[8];
  plug.useNumKeys                 = savedValues[9];
  plug.markTouchedContsAsKey      = savedValues[10];
  plug.wheelResistance            = savedValues[11];
  plug.selectedAction             = savedValues[12];
  plug.sortCriteria               = savedValues[13];
  plug.findCriteria               = savedValues[14];
}


//------------------------
//-- Saves most of the settings within DrawingToolsData in user preferences
//-- so they will load next time Bead Helper is started

void DrawingTools::saveSettings()
{
  double saveValues[NUM_SAVED_VALS];
  
  saveValues[0]   = plug.drawMode;
  saveValues[1]   = plug.draw_reducePts;
  saveValues[2]   = plug.draw_reducePtsMinArea;
  saveValues[3]   = plug.draw_smoothMinDist;
  saveValues[4]   = plug.draw_smoothTensileFract;
  saveValues[5]   = plug.draw_deformRadius;
  saveValues[6]   = plug.wheelBehav;
  saveValues[7]   = plug.dKeyBehav;
  saveValues[8]   = plug.pgUpDownInc;
  saveValues[9]   = plug.useNumKeys;
  saveValues[10]  = plug.markTouchedContsAsKey;
  saveValues[11]  = plug.wheelResistance;
  saveValues[12]  = plug.selectedAction;
  saveValues[13]  = plug.wheelResistance;
  saveValues[14]  = plug.selectedAction;
  
  prefSaveGenericSettings("DrawingTools",NUM_SAVED_VALS,saveValues);
}



//------------------------
//-- Reduces the number of points in the current contour.

void DrawingTools::reduceCurrentContour()
{
  if( !isCurrContValid() )
    return;
  
  undoContourDataChgCC( plug.view );      // REGISTER UNDO
  int pointsRemoved = edit_reduceCurrContour();
  if(pointsRemoved)
  {
    undoFinishUnit( plug.view );            // FINISH UNDO
    ivwRedraw( plug.view );
  }
  wprint("%d points deleted (contour reduction)\n", pointsRemoved);
}

//------------------------
//-- Smooths and increases the number of points in the current contour.

void DrawingTools::smoothCurrentContour()
{
  if( !isCurrContValid() )
    return;
    
  undoContourDataChgCC( plug.view );      // REGISTER UNDO
  int pointsAdded = edit_smoothCurrContour();
  if(pointsAdded)
  {
    undoFinishUnit( plug.view );            // FINISH UNDO
    ivwRedraw( plug.view );
  }
  wprint("%d points added (contour smoothing)\n", pointsAdded);
}


//------------------------
//-- Reduces ALL contours in the current object.

void DrawingTools::reduceConts()
{
  if( !isCurrObjValidAndShown() )
  {
    MsgBox("Current object is not valid or not displayed");
    return;
  }
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj  = getCurrObj();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  static bool interpolatedOnly = false;
  
  string msg =
    "-----"
    "\nmin area  = " + toString(plug.draw_reducePtsMinArea) + " pix sq."
    "\n-----"
    "\nWARNING: reducing contours with "
    "\n a large 'min area' can result "
    "\n in an undesirable loss of "
    "\n information/contour detail.";
  
  QString toolStr =
    "The 'min area' value represents the minimum area which must be between  "
    "\nany 3 consequtive points, else the middle point be deleted. "
    "\nA value of >5.0 is not recommended for reducing large numbers of contours";
  
	CustomDialog ds;
  int ID_DUMMY         = ds.addLabel   ( "contour range:" );
  int ID_CONTMIN       = ds.addSpinBox ( "min:", 1, nConts, 1, 1,
                                         "Only contours AFTER this contour "
                                         "(inclusive) will be changed" );
  int ID_CONTMAX       = ds.addSpinBox ( "max:", 1, nConts, nConts, 1,
                                         "Only contours BEFORE this contour "
                                         "(inclusive) will be changed" );
	int ID_INTERPOLATED  = ds.addCheckBox( "only reduce stippled contours", 
                                         interpolatedOnly );
  int ID_DUMMY2        = ds.addLabel   ( msg.c_str(), toolStr );
	GuiDialogCustomizable dlg(&ds, "Reduce Contours",false);
	dlg.exec();
	if( ds.cancelled )
		return;
  int contMin         = ds.getResultSpinBox   ( ID_CONTMIN ) - 1;
  int contMax         = ds.getResultSpinBox   ( ID_CONTMAX ) - 1;
  interpolatedOnly    = ds.getResultCheckBox  ( ID_INTERPOLATED );
  
  
  //## REDUCE ALL CONTOURS WITHING RANGE:
  
  int totalContsInspected = 0;
  int totalContsChanged   = 0;
  int totalPointsAfter    = 0;
  int totalPointsRemoved  = 0;
  
  for(int c=contMin; c<=contMax && c<csize(obj); c++)
  {
    Icont *cont = getCont( obj, c );
    if( !isContValid(cont) || isEmpty(cont) )
      continue;
    
    if( interpolatedOnly && !isInterpolated(cont) )
      continue;
    
    undoContourDataChg( plug.view, objIdx, c );           // REGISTER UNDO
    int pointsDeleted = 
      cont_reducePtsMinArea( cont, plug.draw_reducePtsMinArea, isContClosed(obj,cont) );
    
    totalPointsRemoved += pointsDeleted;
    totalPointsAfter   += psize(cont);
    
    totalContsInspected++;
    if( pointsDeleted )
      totalContsChanged++;
  }
  if(totalContsChanged)
    undoFinishUnit( plug.view );                        // FINISH UNDO
  
  
  //## PRINT RESULT:
  
  imodSetIndex(imod, objIdx, contIdx, 0);
  int totalPointsBefore = totalPointsAfter + totalPointsRemoved;
  int percentChanged   = (fDivide( totalContsChanged, totalContsInspected ) * 100);
  int percentReduction = 100 - (fDivide( totalPointsAfter, totalPointsBefore ) * 100);
  wprint("REDUCTION OF CONTOURS:\n");
  wprint(" # contours changed = %d of %d  (%d%%)\n", totalContsChanged,
         totalContsInspected, percentChanged );
  wprint(" # points removed \t= %d\n", totalPointsRemoved);
  wprint("   ... %d > %d \t= %d%% reduction\n",
         totalPointsBefore,totalPointsAfter,percentReduction);
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Smooths ALL contours in the current object.

void DrawingTools::smoothConts()
{
  if( !isCurrObjValidAndShown() )
  {
    MsgBox("Current object is not valid or not displayed");
    return;
  }
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj  = getCurrObj();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  static bool interpolatedOnly = false;
  
  
  string msg =
    "-----"
    "\nsmooth tensile fract  = " + toString(plug.draw_smoothTensileFract) +
    "\nsmooth point distance = " + toString(plug.draw_smoothMinDist) +
    "\n-----"
    "\nWARNING: smoothing contours with "
    "\n a large 'tesile fraction' can "
    "\n result in poor contours.";
  
  QString toolStr =
    "The 'tensile fraction' value represents the 'curvyness' "
    "\nof added points, and a value of >0.5 is not recommended "
    "\nfor smoothing a large number of contours";
  
	CustomDialog ds;
  int ID_DUMMY         = ds.addLabel   ( "contour range:" );
  int ID_CONTMIN       = ds.addSpinBox ( "min:", 1, nConts, 1, 1,
                                         "Only contours AFTER this contour "
                                         "(inclusive) will be changed" );
  int ID_CONTMAX       = ds.addSpinBox ( "max:", 1, nConts, nConts, 1,
                                         "Only contours BEFORE this contour "
                                         "(inclusive) will be changed" );
	int ID_INTERPOLATED  = ds.addCheckBox( "only smooth stippled contours",
                                         interpolatedOnly );
  int ID_DUMMY2        = ds.addLabel   ( msg.c_str(), toolStr );
	GuiDialogCustomizable dlg(&ds, "Smooth Contours",false);
	dlg.exec();
	if( ds.cancelled )
		return;
  int contMin         = ds.getResultSpinBox   ( ID_CONTMIN ) - 1;
  int contMax         = ds.getResultSpinBox   ( ID_CONTMAX ) - 1;
  interpolatedOnly    = ds.getResultCheckBox  ( ID_INTERPOLATED );
  
  
  //## SMOOTH ALL CONTOURS WITHING RANGE:
  
  int totalContsInspected = 0;
  int totalContsChanged   = 0;
  int totalPointsAfter    = 0;
  int totalPointsAdded    = 0;
  
  for(int c=contMin; c<=contMax && c<csize(obj); c++)
  {
    Icont *cont = getCont( obj, c );
    
    if( !isContValid(cont) || isEmpty(cont) )
      continue;
    if( interpolatedOnly && !isInterpolated(cont) )
       continue;
    
    undoContourDataChg( plug.view, objIdx, c );           // REGISTER UNDO
    int pointsAdded = 
      cont_addPtsSmooth( cont, plug.draw_smoothMinDist, plug.draw_smoothTensileFract,
                         isContClosed(obj,cont) );
    
    totalPointsAdded  += pointsAdded;
    totalPointsAfter  += psize(cont);
    
    totalContsInspected++;
    if( pointsAdded )
      totalContsChanged++;
  }
  if(totalContsChanged)
    undoFinishUnit( plug.view );
  
  
  //## PRINT RESULT:
  
  int totalPointsBefore = totalPointsAfter - totalPointsAdded;
  int percentChanged   = (fDivide( totalContsChanged, totalContsInspected ) * 100);
  int percentIncrease  = (fDivide( totalPointsAfter, totalPointsBefore ) * 100) - 100;
  wprint("SMOOTHING OF CONTOURS:\n");
  wprint(" # contours changed = %d of %d  (%d%%)\n", totalContsChanged,
         totalContsInspected, percentChanged );
  wprint(" # points added \t= %d\n", totalPointsAdded);
  wprint("   ... %d > %d \t= %d%% increase\n",
         totalPointsBefore,totalPointsAfter,percentIncrease);
  
  ivwRedraw( plug.view );
}



//------------------------
//-- Executes selected [D] action

bool DrawingTools::executeDAction()
{
  if( !isCurrPtValid() || plug.dKeyBehav == DK_NONE )
    return false;
  
  //## GET INFORMATION:
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = getCurrObj();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  Icont *cont = getCurrCont();
  int numPts = psize( cont );
  undoContourDataChgCC( plug.view );            // REGISTER UNDO
  
  int ptsChanged = 0;
  
  //## PERFORM SELECTED ACTION ON POINT/CONTOUR:
  
  switch( plug.dKeyBehav )
  {
    case( DK_TOEND ):
    {
      for( int p = ptIdx+1; p<psize(cont); )
        imodPointDelete(cont, p);
      ptsChanged = numPts - (ptIdx+1);
      break;
    }
    
    case( DK_NEARESTEND ):
    {
      int middlePt = psize(cont) / 2.0f;
      bool deleteFromStart = ptIdx <= middlePt;    // point is closest to start pt
      if( deleteFromStart )
      {
        for( int i = 0; i<ptIdx; i++ )
          imodPointDelete(cont, 0);
        imodSetIndex( imod, objIdx, contIdx, 0 );
      }
      else
      {
        for( int p = ptIdx+1; p<psize(cont); )
          imodPointDelete(cont, p);
      }
      ptsChanged = (deleteFromStart) ? (ptIdx) : (numPts - (ptIdx+1));
      break;
    }
    
    case( DK_DELETEPT ):
    {
      imodPointDelete(cont, ptIdx);
      ptsChanged = 1;
      break;
    }
    
    case( DK_REMOVEPTSIZE ):
    {
      ptsChanged = ( !isDefaultSize(obj,cont,ptIdx) ) ? 1 : 0;
      removePtSize( cont, ptIdx );
      break;
    }
    
    case( DK_REMOVEALLPTSIZES ):
    {
      for(int p=0; p<psize(cont); p++)
        if( !isDefaultSize(obj,cont,p) )
          ptsChanged++;
      removePtsSize( cont );
      break;
    }
  }
  
  
  //## FINISH:
  
  if(ptsChanged)
  {
    undoFinishUnit( plug.view );        // FINISH UNDO
  }
  
  wprint("Changed %d points\n", ptsChanged);
  ivwRedraw( plug.view );
  return true;
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
//-- Prints more detailed contour information about the current object including the
//-- average length, area and point size.

void DrawingTools::printObjectDetailedInfo()
{
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj  = getCurrObj();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  //## PRINT HEADER:
  
  bool objClosed = isObjClosed( obj );
  
  wprint("\nOBJECT %d SUMMARY:\n\n", objIdx+1);
  
  wprint("TYPE:   "); (objClosed) ? wprint("closed\n") : wprint("open\n");
  if( csize(obj)==0 )
  {
    wprint(" ... empty\n");
    return;
  }
  
  //## TALLY STATS:
  
  int    totPts  = 0;
  float  totLen  = 0;
  double totArea    = 0;
  double totPtSize  = 0;
  
  int    emptyConts     = 0;
  int    singlePtConts  = 0;
  int    openConts      = 0;
  int    cclockwiseConts = 0;
  int    stippledConts  = 0;
  
  float minX = FLOAT_MAX, maxX = 0;
  float minY = FLOAT_MAX, maxY = 0;
  float minZ = FLOAT_MAX, maxZ = 0;
  
  for( int c=0; c<csize(obj); c++ )
  {
    Icont *cont   = getCont(obj,c);
    int    closed = isContClosed( obj, cont ) ? 1 : 0;
    
    if( psize(cont)==0 )
      emptyConts++;
    else if( psize(cont)==1 ) 
      singlePtConts++;
    else if( !closed )
      openConts++;
    
    totPts += psize(cont);
    totLen += imodContourLength( cont, closed );
    
    if( isInterpolated(cont) )
      stippledConts++;
    
    if( objClosed && psize(cont)>2 )
      totArea += ABS( imodContourArea(cont) );
    
    if( imodContZDirection(cont) == IMOD_CONTOUR_COUNTER_CLOCKWISE && psize(cont)>2 )
      cclockwiseConts++;
    
    for(int p=0; p<psize(cont); p++ )
    {
      totPtSize += imodPointGetSize(obj,cont,p);
      Ipoint *pt = getPt(cont,p);
      updateMin( minX, pt->x );    updateMax( maxX, pt->x );
      updateMin( minY, pt->y );    updateMax( maxY, pt->y );
      updateMin( minZ, pt->z );    updateMax( maxZ, pt->z );
    }
  }
  
  
  //## CALCULATE AVERAGES:
  
  int nonEmptyConts = csize(obj) - emptyConts;
  
  int percentSinglePt  = int( fDivide( singlePtConts, nonEmptyConts ) * 100 );
  int percentOpen      = int( fDivide( openConts, nonEmptyConts ) * 100 );
  int percentClockwise = int( fDivide( cclockwiseConts, nonEmptyConts ) * 100 );
  int percentStippled  = int( fDivide( stippledConts, nonEmptyConts ) * 100 );
  
  float ptsPerCont = fDivide( totPts, nonEmptyConts );
  float avgDistPts = fDivide( totLen, totPts - (openConts+singlePtConts)  );
  float avgLen     = fDivide( totLen, nonEmptyConts );
  float avgArea    = fDivide( totArea, nonEmptyConts );
  float avgPtSize  = fDivide( totPtSize, nonEmptyConts );
  
  //## PRINT RESULTS:
  
  wprint("SPAN: \tx: %d\t{%d,%d}\n", int(maxX-minX), int(minX), int(maxX) );
  wprint("    \ty: %d\t{%d,%d}\n", int(maxY-minY), int(minY), int(maxY) );
  wprint("    \tz: %d\t{%d,%d}",   int(maxZ-minZ), int(minZ)+1, int(maxZ)+1 );
  wprint("\n");
  if(emptyConts)
    wprint("EMPTY CONTOURS: %d\n", emptyConts );
  wprint("CONTOURS: %d\n", nonEmptyConts );
  wprint("   single pt conts\t= %d (%d%%)\n", singlePtConts,   percentSinglePt );
  wprint("   open conts     \t= %d (%d%%)\n", openConts,       percentOpen );
  wprint("   anti-clockwise\t= %d (%d%%)\n", cclockwiseConts,  percentClockwise );
  if( stippledConts )
    wprint("   stippled    \t= %d (%d%%)\n", stippledConts,   percentStippled );
  wprint("\n");
  wprint("   total pts   \t= %d\n", totPts );
  wprint("   avg pts/cont\t= %f\n", ptsPerCont );
  wprint("   avg line seg\t= %f\n", avgDistPts );
  wprint("   avg pt size\t= %f\n", avgPtSize );
  wprint("\n");
  wprint("   total length\t= %s\n", toStringWithCommas(int(totLen)).c_str() );
  wprint("   avg length \t= %d\n", int(avgLen) );
  wprint("   total area \t= %s\n", toStringWithCommas(int(totArea)).c_str() );
  wprint("   avg area  \t= %s\n",  toStringWithCommas(int(avgArea)).c_str() );
}


//------------------------
//-- Prints some basic object information for all objects including the number of
//-- empty contours, the average number of points per contour, and the average distance
//-- between contour points.

void DrawingTools::printModelPointInfo()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  
  wprint("\nPOINT SUMMARY\n");
  
  int     totPts    = 0;
  float   totLen    = 0;
  int     totConts  = 0;
  int     totEmpty  = 0;
  
  for( int o=0; o<osize(imod); o++ )
  {
    Iobj *obj = getObj(imod,o);
    
    wprint("\nOBJECT %d\n", o+1);
    
    if( csize(obj)==0 )
    {
      wprint(" ... empty\n");
      continue;
    }
    
    int    totObjPts = 0;
    float  totObjLen  = 0;
    int    emptyConts = 0;
    
    for( int c=0; c<csize(obj); c++ )
    {
      Icont *cont   = getCont(obj,c);
      int    closed = isContClosed( obj, cont ) ? 1 : 0;
      
      totObjPts += psize( cont );
      totObjLen += imodContourLength( cont, closed );
      
      if(psize==0)
        emptyConts++;
    }
    
    totPts   += totObjPts;
    totLen   += totObjLen;
    totConts += csize(obj);
    totEmpty += emptyConts;
    
    float ptsPerCont = fDivide( totObjPts, csize(obj) );
    float avgDistPts = fDivide( totObjLen, totObjPts  );    
          // NOTE: not accurate for open contours and single points (no length)
    
    if(emptyConts)
      wprint(" # EMPTY CONTOURS = %d\n", emptyConts );
    wprint(" # conts  = %d\n", totConts );
    wprint(" # pts    = %d\n", totObjPts );
    wprint(" avg pts/cont = %f\n", ptsPerCont );
    wprint(" avg dist between pts = %f\n", avgDistPts );
  }
  
  float ptsPerContAll = fDivide( totPts, totConts );
  float avgDistPtsAll = fDivide( totLen, totPts );
  
  wprint("\n------------\n");
  wprint("OVERALL:\n");
  wprint(" # empty contours = %d\n", totEmpty );
  wprint(" # conts  = %d\n", totConts );
  wprint(" # pts    = %d\n", totPts );
  wprint(" avg pts/cont = %f\n", ptsPerContAll );
  wprint(" avg dist between pts = %f\n", avgDistPtsAll );
}



//------------------------
//-- Gives a choice of several other options for the user.

void DrawingTools::moreActions()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds;
  int ID_ACTION = ds.addRadioGrp( "action:",
                                  "find next overlapping contour [a],"
                                  "sort contours,"
                                  "find contours/points [y],"
                                  "print basic model info,"
                                  "print detailed object info",
                                  plug.selectedAction,
                                  "",
                                  "Finds the next contour which crosses another "
                                    "contour's path. In most situations (eg: closed "
                                    "membranes) no two lines should never intersect - "
                                    "especially a line crossing itself - so it's a good "
                                    "idea to use this tool before converting your "
                                    "contours into the final mesh model.,"
                                  "Physically sorts contours using the criteria you "
                                    "select,"
                                  "Use [y] to find the contour or point with the "
                                    "next biggest value based on the criteria you "
                                    "select,"
                                  "Prints some basic information about the current "
                                    "object including the average distance between "
                                    "points; average points contour; and number of "
                                    "empty contours.,"
                                  "Prints more detailed information about the "
                                    "current object");
	GuiDialogCustomizable dlg(&ds, "Perform Action", this);
	dlg.exec();
	if( ds.cancelled )
		return;
	plug.selectedAction = ds.getResultRadioGrp	( ID_ACTION );
  
  switch(plug.selectedAction)
  {
    case(0):      // find next overlapping contour [a]
      selectNextOverlappingContour();
      break;
    case(1):      // sort contours
      sortContours();
      break;
    case(2):      // find contours
      findContours();
      break;
    case(3):      // print basic model info
      printModelPointInfo();
      break;
    case(4):      // print detailed object info
      printObjectDetailedInfo();
      break;
  }
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Allows user to change other plugin values/settings.

void DrawingTools::moreSettings()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds;
  int ID_WHEELRESIST    = ds.addSpinBox ( "wheel resistance:",
                                          10, 1000, plug.wheelResistance, 10,
                                          "The higher the value, the slower "
                                          "mouse scrolling works" );
  int ID_USENUMKEYS     = ds.addCheckBox( "use number keys to change mode", 
                                          plug.useNumKeys,
                                          "If on: will intercept the number keys "
                                          "[1]-[5] to change the drawing mode, "
                                          "\nif off: can use number keys to move points "
                                          "as per normal");
  int ID_MARKTOUCHEDS   = ds.addCheckBox( "mark contours unstippled after deform", 
                                          plug.markTouchedContsAsKey,
                                          "If on: any stippled contour selected and/or "
                                          "\ndeformed using the 'deform' or 'join' "
                                          "\ntool will become unstippled." );
  int ID_WHEELBEHAV     = ds.addComboBox( "wheel behavior:",
                                          "none,"
                                          "resize deform circle,"
                                          "scroll slices,"
                                          "scroll contours,"
                                          "resize curr point", plug.wheelBehav,
                                          "The action performed by the mouse wheel" );
  int ID_DEFORMCIRCLE    = ds.addLineEdit( "deform circle radius:", 
                                           toString(plug.draw_deformRadius).c_str(),
                                           "The radius (in pixels) of the circle used "
                                           "in deform and join drawing mode."
                                           "\nNOTE: You can also change this using "
                                           "[q], [w] and the mouse wheel.");
  int ID_DKEYACTION     = ds.addComboBox( "on [d] remove:",
                                          "do nothing,"
                                          "pts to end,"
                                          "to nearest end,"
                                          "current pt,"
                                          "pt size current pt,"
                                          "pt sizes current cont", plug.dKeyBehav,
                                          "Action performed when [d] is pressed."
                                          "\n"
                                          "\n > do nothing - as it sounds"
                                          "\n > pts to end - deletes all points beyond "
                                          "the current point (to the last point)"
                                          "\n > to nearest end - deletes all points in "
                                          "the current contour from the selected "
                                          "point to the closest end (not inclusive)"
                                          "\n > current pt - deletes current point "
                                          "(same as [delete]) "
                                          "\n > pt size current pt - resets the sphere "
                                          "size of the current point to the default "
                                          "\n > pt sizes current cont - resets the "
                                          "sphere size (and any fine grain info) of all "
                                          "points in the current contour");
  int ID_PGINCREMENT    = ds.addSpinBox ( "[PgUp]/[PgDown] increment:",
                                          1, 1000, plug.pgUpDownInc, 1,
                                          "NOTE: Holding [Shift] when you press "
                                          "[Page Up] or [Page Down] will cause it to "
                                          "increment one slice (as normal)" );
  int ID_SHOWMOUSEINMV  = ds.addCheckBox( "show mouse in model view", 
                                          plug.showMouseInModelView,
                                          "Will show the mouse in any Model View "
                                          "windows as you move it in the ZAP window. "
                                          "\nWARNING: This will reduce performance!");
  
	GuiDialogCustomizable dlg(&ds, "More Settings", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  plug.wheelResistance       = ds.getResultSpinBox  ( ID_WHEELRESIST );
  plug.useNumKeys            = ds.getResultCheckBox ( ID_USENUMKEYS );
  plug.markTouchedContsAsKey = ds.getResultCheckBox ( ID_MARKTOUCHEDS );
  plug.wheelBehav            = ds.getResultComboBox ( ID_WHEELBEHAV );
  plug.dKeyBehav             = ds.getResultComboBox ( ID_DKEYACTION );
  plug.pgUpDownInc           = ds.getResultSpinBox  ( ID_PGINCREMENT );
  plug.showMouseInModelView  = ds.getResultCheckBox ( ID_SHOWMOUSEINMV );
  string deformRadiusStr     = ds.getResultLineEdit ( ID_DEFORMCIRCLE );
  
  float newDeformRadius      = string_getFloatFromString( deformRadiusStr );
  if( newDeformRadius != 0 && newDeformRadius > 0.2 && newDeformRadius < 5000 )
    plug.draw_deformRadius = newDeformRadius;
  
  ivwRedraw( plug.view );
}



//------------------------
//-- Prompts for a criteria for reordering, and reorderes specified range of contours
//-- using this criteria.

void DrawingTools::sortContours()
{
  if( !isCurrObjValidAndShown() )  {
    wprint("Must select valid object\n");
    return;
  }
  
  int nConts = csize(getCurrObj());
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool reverseOrder = false;
  static bool printVals    = true;
  static bool calcValsOnly = false;
  
	CustomDialog ds;
  int ID_DUMMY         = ds.addLabel   ( "contours to sort (inclusive):" );
  int ID_CONTMIN       = ds.addSpinBox ( "min:", 1, nConts, 1, 1,
                                         "Only contours after this contour "
                                         "(inclusive) will be reordered" );
  int ID_CONTMAX       = ds.addSpinBox ( "max:", 1, nConts, nConts, 1,
                                         "Only contours BEFORE this contour "
                                         "(inclusive) will be reordered" );
	int ID_SORTCRITERIA  = ds.addRadioGrp( "sort contours by:      (sort criteria)",
                                         "number points,"
                                         "contour length,"
                                         "area,"
                                         "clockwise area,"
                                         "avg point size,"
                                         "avg gray value,"
                                         "stippled,"
                                         "random,"
                                         "mean x,"
                                         "mean y,"
                                         "mean z,"
                                         "min x,"
                                         "min y,"
                                         "min z",
                                         plug.sortCriteria,
                                         "",
                                         "Sorts by the number of points (empty first),"
                                         "Length of the contours (open or closed - "
                                            "depending on object/contour),"
                                         "Area of the contour (smallest first),"
                                         "From largest anti-clockwise to no area to "
                                           "largest clockwise area,"
                                         "Average point size over all points in the "
                                           "contour (using object default if not set),"
                                         "Uses the average gray value of the pixel "
                                           "closest to each point,"
                                         "Stippled contours first,"
                                         "Uses a random number for each contour,"
                                         "Contour's center of mass in X,"
                                         "Contour's center of mass in Y,"
                                         "Contour's center of mass in Z"
                                          );
  int ID_CALVALS       = ds.addCheckBox( "calc values only (don't reorder)",
                                         calcValsOnly,
                                         "No contours will be reordered, but "
                                         "values will be calculated and you "
                                         "can iterate from largest to smallest "
                                         "by pressing [y]" );
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
  
  edit_reorderConts( plug.sortCriteria, contMin, contMax, calcValsOnly,
                     reverseOrder, printVals );
  
  ivwRedraw( plug.view );
}



//------------------------
//-- Prompts for a criteria for reordering, and reorderes specified range of contours
//-- using this criteria.

void DrawingTools::findContours()
{
  if( !isCurrObjValidAndShown() )  {
    wprint("Must select valid object\n");
    return;
  }
  
  //int nConts = csize(getCurrObj());
  static bool  useStartVal = true;
  static float startVal = 0;
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds;
	int ID_FINDCRITERIA  = ds.addComboBox( "Find property:",
                                         "contour number of points,"
                                         "contour length,"
                                         "contour area,"
                                         "contour clockwise area,"
                                         "contour avg point size,"
                                         "contour avg gray value,"
                                         "contour stippled,"
                                         "contour random,"
                                         "contour mean x value,"
                                         "contour mean y value,"
                                         "contour mean z value,"
                                         "contour min x value,"
                                         "contour min y value,"
                                         "contour min z value,"
                                         "point x value,"
                                         "point y value,"
                                         "point z value,"
                                         "point size,"
                                         "point gray value",
                                         plug.findCriteria,
                                         "The criteria used to advance through contours"
                                         "and/or points using [y]\n" );
  int ID_USESTARTVAL   = ds.addCheckBox( "start search using value below",
                                         useStartVal,
                                         "If not ticked will use the (find value) "
                                         "of the selected contour/point instead." );
  int ID_STARTVAL      = ds.addLineEdit( "Value to find:               ",
                                          toString(startVal).c_str(),
                                          "The value to start searching for" );
  int ID_DUMMY         = ds.addLabel   ( "-----\n"
                                         "NOTE: Use [y] and [Y] to advance through\n"
                                         "   points/contours with increasing\n"
                                         "   value - according to the specified\n"
                                         "   'find property' - compared to the\n"
                                         "   currently selected point/contour." );
  
  GuiDialogCustomizable dlg(&ds, "Find Options", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  
	plug.findCriteria		= ds.getResultComboBox	( ID_FINDCRITERIA );
  useStartVal         = ds.getResultCheckBox  ( ID_USESTARTVAL );
  string startValStr  = ds.getResultLineEdit  ( ID_STARTVAL );
  startVal            = string_getFloatFromString( startValStr );
  
  edit_goToContNextBiggestFindVal( false, true,
                                   !useStartVal, startVal );
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
  
  
  imodContourCenterOfMass( plug.copiedCont, &plug.copiedCenterPt );
  Ipoint centroidPt;
  centroidPt.x = -plug.copiedCenterPt.x;
  centroidPt.y = -plug.copiedCenterPt.y;
  centroidPt.z = -plug.copiedCenterPt.z;
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
  
  imodContourCenterOfMass( plug.copiedCont, &plug.copiedCenterPt );
  Ipoint centroidPt;
  centroidPt.x = -plug.copiedCenterPt.x;
  centroidPt.y = -plug.copiedCenterPt.y;
  centroidPt.z = -plug.copiedCenterPt.z;
  cont_translate( plug.copiedCont, &centroidPt );
  
  
  wprint("Contour has been copied\n");
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Method used for testing new routines.

void DrawingTools::paste(bool centerOnMouse)
{  
  if( !isContValid(plug.copiedCont) || isEmpty(plug.copiedCont) )  {
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
  if( centerOnMouse )
  {
    cont_translate( contNew, &plug.mouse );
  }
  else
  {
    Ipoint centroidPt;
    centroidPt.x = plug.copiedCenterPt.x;
    centroidPt.y = plug.copiedCenterPt.y;
    centroidPt.z = -plug.copiedCenterPt.z + plug.mouse.z;
    cont_translate( contNew, &centroidPt );    
  }
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
//-- Change draw_reducePtsMinArea

void DrawingTools::changeMinArea( int value ) { 
  plug.draw_reducePtsMinArea = (float)value / 10.0f;
}

//------------------------
//-- Change draw_smoothMinDist

void DrawingTools::changeSmoothPtsDist( int value ) {
  plug.draw_smoothMinDist = (float)value / 1.0f;
}

//------------------------
//-- Change draw_smoothTensileFract

void DrawingTools::changeSmoothTensileFract( int value ) {
  plug.draw_smoothTensileFract = (float)value / 10.0f;
}

//------------------------
//-- Change changeReducePts

void DrawingTools::changeReducePts() {
  plug.draw_reducePts = reducePtsCheckbox->isChecked() ? 1 : 0;
}

//------------------------
//-- Change changeDeformCircleRadius

void DrawingTools::changeDeformCircleRadius( float value, bool accel )
{  
  if(!accel)
    plug.draw_deformRadius += value;
  else
    plug.draw_deformRadius *= (1 + (value*0.01) );
  
  keepWithinRange( plug.draw_deformRadius, 2.0f, 500.0f );
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
  plug.window->saveSettings();
  
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

int edit_changeSelectedSlice( int changeZ, bool redraw, bool snapToEnds )
{
  int ix, iy, iz;
  ivwGetLocation( plug.view, &ix, &iy, &iz );
  int newZ = iz+changeZ;
  if( !snapToEnds && newZ < 0 || newZ >= plug.zsize )
    return iz;
  edit_setZapLocation( ix, iy, newZ, redraw );
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
  
  int nConts = csize(obj);
  
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
    
    int numIntermediates = fDivide(mouseMoveDist+2.0f,plug.draw_deformRadius);
            // the number of extra circles we will have to add between the last
            // and current mouse position to deform smoothly.
    
    if( numIntermediates > 10 ) {
      wprint("\aYou are moving the mouse to fast!\n");
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
      float distFromCenterSq = line_sqDistBetweenPts2D( &center, getPt(cont,i) );
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
    
    float maxDistAllowedBetweenPts = MIN( plug.draw_smoothMinDist, radius );
    
    int extra = isContClosed(obj, cont) ? 0 : -1;
    for (int i=0; i<psize(cont)+extra; i++)
    {
      bool thisPtShifted = ( getPt(cont,i)->z   == POINT_SHIFTED);
      bool nextPtShifted = ( getPt(cont,i+1)->z == POINT_SHIFTED);
      
      if( thisPtShifted || nextPtShifted )  // if the current pt OR next pt was shifted:
      {
        float line_distBetweenPts = imodPointDistance( getPt(cont,i), getPt(cont,i+1) );
        
        //if( line_distBetweenPts > radius*5 )
        //  wprint("CRAP!!!!\n");
        
        // if BOTH points have been shifted: add a point in the middle
        
        if     ( thisPtShifted && nextPtShifted )   
        {
          if ( line_distBetweenPts > maxDistAllowedBetweenPts )
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
          if ( line_distBetweenPts < maxDistAllowedBetweenPts ) {
            int idxToRemove = (i+1) % psize(cont);
            imodPointDelete(cont, idxToRemove);    // remove next point
            i--;
          }
        }
        else if( nextPtShifted && !thisPtShifted )
        {
          if ( line_distBetweenPts < maxDistAllowedBetweenPts ) {
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
      float distFromCenterSq = line_sqDistBetweenPts2D( &center, getPt(cont,i) );
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
  
  if( plug.markTouchedContsAsKey && isCurrContValid() && isInterpolated( getCurrCont() ) )
  {
    undoContourPropChgCC( plug.view );        // REGISTER UNDO
    setInterpolated( getCurrCont(), 0 );
  }
  
  undoFinishUnit( plug.view );        // FINISH UNDO
}

//------------------------
//-- Compleles a join action by merging the contour
//-- with any other contours it touches.... and breaking it into
//-- multiple contours if a contour was split apart.

void edit_executeJoinEnd()
{
  if (plug.draw_reducePts)
    edit_reduceCurrContour();
  
  edit_joinCurrContWithAnyTouching();
  edit_breakCurrContIntoSimpleContsAndDeleteSmallest();
  edit_makeCurrContSimple();
  edit_deleteCurrContIfTooSmall();
  
  if (plug.draw_reducePts)
    edit_reduceCurrContour();
  
  if( plug.markTouchedContsAsKey && isCurrContValid() && isInterpolated( getCurrCont() ) )
  {
    undoContourPropChgCC( plug.view );        // REGISTER UNDO
    setInterpolated( getCurrCont(), 0 );
  }
  
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
//-- and returns the number of points it deleted (or 0)

int edit_reduceCurrContour()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( isContValid(cont) )
  {
    return cont_reducePtsMinArea( cont, plug.draw_reducePtsMinArea,
                                  isContClosed(obj,cont) );
  }
  return 0;
}

//------------------------
//-- Tries to smooth and increase the number of points in the
//-- current contour and returns the number of points added (or 0)

int edit_smoothCurrContour()
{
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( isContValid(cont) )
  {
    if( !isContClosed(obj,cont) )
      wprint("NOT CLOSED!");
    
    return cont_addPtsSmooth( cont, plug.draw_smoothMinDist, plug.draw_smoothTensileFract,
                       isContClosed(obj,cont) );
  }
  return 0;
}


//------------------------
//-- Erases all contours in the given radius and returns the
//-- number of contours removed.

bool edit_doesBBoxTouchCircle( Ipoint *ll, Ipoint *ur, Ipoint *center, float radius )
{
  if( center->z < ll->z || center->z > ur->z )
    return false;
  
  return (mbr_distToBBox2D(center, ll, ur) < radius);
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
  
  for (int o=0; o<osize(imod); o++ )
  {
    imodSetIndex( imod, o, 1, 1);
    Iobj *obj  = imodObjectGet(imod);
    
    if (!isObjectValidAndShown(obj))
      continue;
    
    for(int c=0; c<csize(obj); c++)    // for each contour:
    {
      Icont *cont = getCont(obj, c);
      
      if( getZ(cont) == center.z )        // if contour on desired slice:
      {
        for(int p=0; p<psize(cont); p++)    // for each point in that contour:
        {
          float distSq = line_sqDistBetweenPts2D( &center, getPt(cont,p) );
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
  
  for (int o=0; o<osize(imod); o++ )
  {
    imodSetIndex( imod, o, 1, 1);
    Iobj *obj  = imodObjectGet(imod);
    
    for(int c=0; c<csize(obj); c++)    // for each contour:
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
  
  for (int o=0; o<osize(imod); o++ )
  {
    imodSetIndex( imod, o, 1, 1);
    Iobj *obj  = imodObjectGet(imod);
    
    for(int c=0; c<csize(obj); c++)    // for each contour:
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
      cont_reducePtsMinArea( conts[i].cont, plug.draw_reducePtsMinArea,
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
      int contToSelect = csize(obj) - conts.size();
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
  
  bool isTooSmall = imodContourArea(cont) < MAX( plug.draw_reducePtsMinArea*3.0, 10.0 );
  
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
  
  for(int i=0; i<csize( obj ); i++)
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
  int nObjs = osize(imod);
  
  //## FIND PROBLEM CONTOURS:
  
  for (int o=objIdx; o<(nObjs+objIdx+1); o++)         // for each object:
  {
    if( o == nObjs )
      wprint("Starting from beginning...\n");
    int oNum = (o%nObjs);
    
    Iobj *objO  = getObj(imod,oNum);
    if( !isObjClosed(objO) )
      continue;
    
    
    int i = (o==objIdx) ? (contIdx+1) : 0;            // starting contour
    for (; i<csize(objO); i++)      // for each contour:
    {
      Icont *contI = getCont( objO, i );
      if( isEmpty(contI) )
        continue;
      
      bool isSimple = cont_isSimple( contI, isContClosed(objO,contI) );
      if( !isSimple )
      {
        imodSetIndex( imod, o%nObjs, i, 0 );
        wprint("\aNon-simple polygon found!");
        return (true);
      }
      
      Ipoint contIll, contIur;                // get bounding box for contI
      imodContourGetBBox( contI, &contIll, &contIur );
      
      for (int p=(oNum); p<osize(imod); p++)          // for each object ahead of that:
      {
        Iobj *objP  = getObj(imod,p);
        if( !isObjClosed(objP) )
          continue;
        
        int j = ((o%nObjs)==p) ? i+1 : 0;               // starting contour
        for(; j<csize(objP); j++)     // for each contour ahead:
        {
          Icont *contJ = getCont( objP, j );
          if( isEmpty(contJ) )
            continue;
          
          if( ( getZInt(contJ) == getZInt(contI) )  )     // if on same slice:
          {
            Ipoint contJll, contJur;                // get bounding box for contJ
            imodContourGetBBox( contJ, &contJll, &contJur );
            
            int pt1Cross, pt2Cross;
            if( mbr_doBBoxesOverlap2D( &contIll, &contIur, &contJll, &contJur )
                && cont_doCountoursCrossAndWhere( contI, contJ,
                                               isContClosed(objO,contI),
                                               isContClosed(objP,contJ),
                                               &pt1Cross, &pt2Cross ) )
            {
              imodSetIndex( imod, o, i, ((pt1Cross+1)%psize(contI)) );
              
              if( cont_isEqual(contJ,contI) )
                wprint("\aWARNING: Two identical contours found!");
              else
                wprint("Overlapping contour found.\n");
              return (true);
            }
          }
        }
      }
    }
  }
  
  wprint("No overlapping contours found.\n");
  
  return (false);
}







//------------------------
//-- Returns a string corresponding to the sort criteria (see: enum sortcriteria)

string edit_getSortValString(int sortCriteria)
{
  string returnStr;
  
  switch(sortCriteria)
  {
    case(SORT_NUMPTS):          returnStr = "CONTOUR'S NUMBER OF POINTS";  break;
    case(SORT_LENGTH):          returnStr = "CONTOUR'S LENGTH";  break;
    case(SORT_AREA):            returnStr = "CONTOUR'S AREA";  break;
    case(SORT_CLOCKWISEAREA):   returnStr = "CONTOUR'S CLOCKWISE AREA";  break;
    case(SORT_AVGPTSIZE):       returnStr = "CONTOUR'S MEAN PT RADIUS";  break;
    case(SORT_AVGGRAY):         returnStr = "CONTOUR'S MEAN PT GRAY VALUE";  break;
    case(SORT_STIPPLED):        returnStr = "CONTOUR'S STIPPLED VALUE";  break;
    case(SORT_RANDOM):          returnStr = "CONTOUR RANDOM VALUE";  break;
    case(SORT_AVGX):            returnStr = "CONTOUR'S MEAN X VALUE";  break;
    case(SORT_AVGY):            returnStr = "CONTOUR'S MEAN Y VALUE";  break;
    case(SORT_AVGZ):            returnStr = "CONTOUR'S MEAN Z VALUE";  break;
    case(SORT_MINX):            returnStr = "CONTOUR'S MIN X VALUE";  break;
    case(SORT_MINY):            returnStr = "CONTOUR'S MIN Y VALUE";  break;
    case(SORT_MINZ):            returnStr = "CONTOUR'S MIN Z VALUE";  break;
    case(SORT_PTX):             returnStr = "POINT'S X VALUE";  break;
    case(SORT_PTY):             returnStr = "POINT'S Y VALUE";  break;
    case(SORT_PTZ):             returnStr = "POINT'S Z VALUE";  break;
    case(SORT_PTSIZE):          returnStr = "POINT'S RADIUS";  break;
    case(SORT_PTGREY):          returnStr = "POINT'S GRAY VALUE";  break;
  }
  
  return returnStr;
}




//------------------------
//-- Returns the grey scale value in memory of the pixel nearest to the given point

float edit_getGreyValue( Ipoint *pt )
{
  int x = int(pt->x + 0.5);
  int y = int(pt->y + 0.5);
  int z = int(pt->z + 0.5);
  
  float greyVal = ivwGetFileValue( plug.view, x,y,z );
  return greyVal;
}

//------------------------
//-- Returns the average grey value of the points in the contour.

float edit_avgGreyValueOfPts( Icont *cont )
{
  if( isEmpty(cont) )
    return 0;
  
  float totalGreyVal = 0;
  
  for(int p=0; p<psize(cont); p++)
    totalGreyVal += edit_getGreyValue( getPt(cont,p) );
  
  float avgGreyVal = totalGreyVal / (float)psize(cont);
  
  return (avgGreyVal);
}


//------------------------
//-- Returns the average point size for the contour.

float edit_getAvgPtSize( Iobj *obj, Icont *cont )
{
  if( isEmpty(cont) )
    return 0;
  
  float totPtSize = 0;
  
  for(int p=0; p<psize(cont); p++)
    totPtSize += imodPointGetSize(obj,cont,p);
  
  float avgGreyVal = totPtSize / (float)psize(cont);
  
  return (avgGreyVal);
}



//------------------------
//-- Go to the contour with the "next biggest" sort value
//-- within "plugs.sortVals", and will recalculate these values
//-- if "findNextSmallest" is false, or the number of contours
//-- doesn't match the number of sort values.
//-- Returns true if a contour is found and selected.

float previousValue = 0;
int   previousPtIdx    = 0;

bool edit_goToContNextBiggestFindVal( bool findNextSmallest, bool recalc,
                                      bool useCurrentValue, float chosenTarget )
{
  if( !isCurrObjValidAndShown() )
    return false;
  
  if( csize( getCurrObj() ) == 0 ) {
    wprint("\aThere are no contours\n");
    return false;
  }
  
  
  //## SETUP VARIABLES:
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  bool pointwiseOperation = (plug.findCriteria >= SORT_PTX);
  
  
  //## IF NEEDED, RECALCULATE SORT VALUES:
  
  if ( recalc || plug.sortVals.size() != csize(obj) 
       || plug.sortCriteriaOfVals != plug.findCriteria )
  {
    wprint("\nFinding %s value\n", (findNextSmallest) ? "smallest" : "biggest" );
    wprint(" --> RECALCULATING VALUES ...\n");
    edit_reorderConts( plug.findCriteria, 0, csize(obj)-1, true, false, false );
  }
  else
  {
    wprint("\nFinding next %s value\n", (findNextSmallest) ? "smallest" : "biggest" );
  }
  
  
  //## DETERMINE TARGET VALUE:
  
  float targetVal;      // the value we want to find (not necessarily the current value)
  float closestVal;     // the closest value (to the target value) found thus far
  
  if( useCurrentValue )
  {
    targetVal = edit_getSortValue( plug.findCriteria, obj, getCurrCont(), ptIdx );
    if( pointwiseOperation )
      wprint(" --> prev: %f \t(pt %d,%d)\n",   targetVal, contIdx+1, ptIdx+1 );
    else
      wprint(" --> prev: %f \t(cont %d)\n",   targetVal, contIdx+1 );  
  }
  else
  {
    targetVal = chosenTarget;
    wprint(" --> target value: %f\n", targetVal );
    contIdx = -1;
    ptIdx = -1;
  }
  
  closestVal = (findNextSmallest) ? FLOAT_MIN : FLOAT_MAX;
  
  
  //## FIND THE POINT WITH THE NEXT BIGGEST Y JUMP:
  
  int   closestContIdx = -1;              // the contour with the smallest value >minVal
  int   closestPtIdx   = 0;               // the point in "minContIdx" to select
  
  
  if( !pointwiseOperation )   // if we want the next CONTOUR:
  {
    
    if(findNextSmallest)        // if we want the next smallest:
    {
      for (int c = plug.sortVals.size()-1; c>=0; c--)
      {
        int cIdx = plug.sortVals[c].idx;
        if( cIdx == contIdx )                      // don't select same contour
          continue;
        float val = plug.sortVals[c].float1; 
        if( val == targetVal && cIdx < contIdx )   // if exact match found
        {                                          // in earlier contour: select it
          closestVal = val;
          closestContIdx = cIdx;
          break;
        }
        else if( val < targetVal && val > closestVal )  // if closer match found: update
        {
          closestVal = val;
          closestContIdx = cIdx;
        }
      }
    }
    else                       // else (if we want the next biggest):
    {
      for (int c = 0; c<plug.sortVals.size(); c++)
      {
        int cIdx = plug.sortVals[c].idx;
        if( cIdx == contIdx )                        // don't select same contour
          continue;
        float val = plug.sortVals[c].float1; 
        if( val == targetVal && cIdx > contIdx )     // if exact match found
        {                                            // in later contour: select it
          closestVal = val;
          closestContIdx = cIdx;
          break;
        }
        else if( val > targetVal && val < closestVal )  // if closer match found: update
        {
          closestVal = val;
          closestContIdx = cIdx;
        }
      }
    }
    
  }
  
  
  else                        // if we want the next POINT:
  {
    
    if(findNextSmallest)        // if we want the next smallest:
    {
      for (int i = plug.sortPtVals.size()-1; i>=0; i--)
      {
        int cIdx  = plug.sortPtVals[i].idx;
        int pIdx  = (int)plug.sortPtVals[i].float2;
        float val = plug.sortPtVals[i].float1;
        if( cIdx == contIdx && pIdx == ptIdx)      // don't select same point
          continue;
        if( val == targetVal && ( cIdx < contIdx       // if exact match found
            || (cIdx == contIdx && pIdx < ptIdx) ) )   // in earlier point: select it
        {
          closestVal = val;
          closestContIdx = cIdx;
          closestPtIdx   = pIdx;
          break;
        }
        else if( val < targetVal && val > closestVal ) // if better match found: update
        {
          closestVal = val;
          closestContIdx = cIdx;
          closestPtIdx   = pIdx;
        }
      }
    }
    
    else                       // else (if we want the next biggest):
    {
      for (int i = 0; i<plug.sortPtVals.size(); i++)
      {
        int cIdx  = plug.sortPtVals[i].idx;
        int pIdx  = (int)plug.sortPtVals[i].float2;
        float val = plug.sortPtVals[i].float1;
        if( cIdx == contIdx && pIdx == ptIdx)      // don't select same point
          continue;
        if( val == targetVal && ( cIdx > contIdx       // if exact match found
            || (cIdx == contIdx && pIdx > ptIdx) ) )   // in later point: select it
        {
          closestVal = val;
          closestContIdx = cIdx;
          closestPtIdx   = pIdx;
          break;
        }
        else if( val > targetVal && val < closestVal ) // if better match found: update
        {
          closestVal = val;
          closestContIdx = cIdx;
          closestPtIdx   = pIdx;
        }
      }
    }
    
  }
  
  
  //## PRINT RESULT:
  
  if( closestContIdx == -1 )
  {
    wprint("\aNo more 'next %s' value found - press 'B' to reset.\n",
           (findNextSmallest) ? "smallest" : "biggest" );
    return false;
  }
  
  if( pointwiseOperation )
    wprint(" --> new:  %f \t(pt %d,%d)\n",
           closestVal, closestContIdx+1, closestPtIdx+1 );
  else
    wprint(" --> new:  %f \t(cont %d)\n", closestVal, closestContIdx+1 );
  
  previousValue = closestVal;
  
  //## SELECT CONTOUR:
  
  imodSetIndex(imod, objIdx, closestContIdx, closestPtIdx);
  ivwRedraw( plug.view );
  return true;
}


//------------------------
//-- Gets a float value of the specified contour using the specified "sortCriteria"
//-- (see: enum contsortcriteria). In cases were sortCriteria involves searching
//-- each point (not just the whole contour), it returns the value of the
//-- point at index "ptIdx".

float edit_getSortValue( int sortCriteria, Iobj *obj, Icont *cont, int ptIdx )
{
  if ( psize(cont)==0 )
    return 0;
  
  
  float returnValue = INT_MAX;
  
  switch(sortCriteria)
  {
    case(SORT_NUMPTS):
      returnValue = psize(cont);
      break;
    
    case(SORT_LENGTH):
    {
      int    closed = isContClosed( obj, cont ) ? 1 : 0;
      returnValue = imodContourLength(cont, closed);
    }
      break;
      
    case(SORT_AREA):
      returnValue = imodContourArea(cont);
      break;
      
    case(SORT_CLOCKWISEAREA):
    {
      returnValue = imodContourArea(cont);
      if( returnValue > 0 && imodContZDirection(cont) == IMOD_CONTOUR_COUNTER_CLOCKWISE )
        returnValue *= -1;
    }
      break;
        
    case(SORT_AVGPTSIZE):
      returnValue = edit_getAvgPtSize(obj,cont);
      break;
      
    case(SORT_STIPPLED):
      returnValue = isInterpolated(cont) ? 0 : 1;
      break;
      
    case(SORT_RANDOM):
      returnValue = rand();
      break; 
      
    case(SORT_AVGGRAY):
      returnValue = edit_avgGreyValueOfPts(cont);
      break;
      
      
    case(SORT_AVGX):
    {
      Ipoint pt;
      cont_getCenterOfMBR(cont,&pt);
      returnValue = pt.x;
    }
      break;
      
    case(SORT_AVGY):
    {
      Ipoint pt;
      cont_getCenterOfMBR(cont,&pt);
      returnValue = pt.y;
    }
      break;
      
    case(SORT_AVGZ):
    {
      Ipoint pt;
      cont_getCenterOfMBR(cont,&pt);
      returnValue = pt.z;
    }
      break;
      
      
    case(SORT_MINX):
    {
      for( int p=0; p<psize(cont); p++)
        if( getPt(cont,p)->x < returnValue )
          returnValue = getPt(cont,p)->x;
    }
      break;
      
    case(SORT_MINY):
    {
      for( int p=0; p<psize(cont); p++)
        if( getPt(cont,p)->y < returnValue )
          returnValue = getPt(cont,p)->y;
    }
      break;
      
    case(SORT_MINZ):
    {
      for( int p=0; p<psize(cont); p++)
        if( getPt(cont,p)->z < returnValue )
          returnValue = getPt(cont,p)->z;
    }
      break;
      
      
      
      
      
    case(SORT_PTX):
      returnValue = getPt(cont,ptIdx)->x;
      break;
      
    case(SORT_PTY):
      returnValue = getPt(cont,ptIdx)->y;
      break;
      
    case(SORT_PTZ):
      returnValue = getPt(cont,ptIdx)->z;
      break;
      
      
    case(SORT_PTSIZE):
      returnValue = imodPointGetSize(obj,cont,ptIdx);;
      break;
      
    case(SORT_PTGREY):
      returnValue = edit_getGreyValue( getPt(cont,ptIdx) );
      break;
  }
  
  return returnValue;
}


//------------------------
//-- Sorts and reorderes all contours past "minCont" according to the specified
//-- "sortCriteria" (see: enum contsortcriteria).
//-- Setting "reverse" will sort the value in descending instead of descending.
//-- Setting "printVals" will output the sort values for each contour.

void edit_reorderConts( int sortCriteria, int minCont, int maxCont,        
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
    plug.sortVals[c].float2 = c;
    Icont *cont = getCont(obj,c);
    int    closed = isContClosed( obj, cont ) ? 1 : 0;
    
    plug.sortVals[c].float1 = edit_getSortValue( sortCriteria, obj, cont );
  }
  
  
  //## SORT CONTOURS BY SORT VALUES:
  
  int numContsChanged = 0;
  
  if( !calcValsOnly )
  {
    //## SORT THE CHOSEN RANGE OF VALUES WITHIN THE SORT VECTOR:
    
    plug.sortVals = vector_sort( plug.sortVals, minC, maxC );
    
    if( reverse )
      plug.sortVals = vector_reverse( plug.sortVals, minC, maxC );
    
    
    //## REORDER THE CONTOURS WITHIN THE OBJECT USING THE NOW SORTED SORT VECTOR:
    
    Iobj *objCopy = imodObjectDup( obj );
    
    for( int c=minC; c<=maxC; c++)
    {
      if( plug.sortVals[c].idx != c )
      {
        Icont *cont = getCont(obj,c);
        imodSetIndex(imod, objIdx, c, 0);
        undoContourDataChg( plug.view, objIdx, c );      // REGISTER UNDO
        Icont *newCont = getCont( objCopy, plug.sortVals[c].idx );
        imodContourSwap(cont,newCont);
        numContsChanged++;
      }
    }
    
    if(numContsChanged>0)
      undoFinishUnit( plug.view );              // FINISH UNDO
    
    imodObjectDelete( objCopy );
    imodSetIndex(imod, objIdx, nConts-1, 0);
  }
  
  
  //## CALCULATE POINT VALUES IN VECTOR:
  
  plug.sortPtVals.clear();
  bool pointwiseOperation = (sortCriteria >= SORT_PTX);
  
  if( pointwiseOperation )
  {
    for( int c=0; c<nConts; c++)
    {
      Icont *cont = getCont(obj,c);
      for(int p=0; p<psize(cont); p++)
      {
        IdxToSort newPtVal;
        newPtVal.idx = c;
        newPtVal.float2 = p;
        newPtVal.float1 = edit_getSortValue( sortCriteria, obj, cont, p );
        plug.sortPtVals.push_back( newPtVal );
      }
    }
  }
  
  plug.sortCriteriaOfVals = sortCriteria;
  
  //## OUTPUT RESULTS:
  
  if( printVals )
  {
    wprint("\n%s:\n", edit_getSortValString(sortCriteria).c_str() );
        
    for( int c=0; c<nConts; c++)
    {
      wprint( "cont %d \t= %f", c+1, plug.sortVals[c].float1 );
      if( plug.sortVals[c].idx != c )
        wprint( "\t(*%d)", plug.sortVals[c].idx+1 );
      wprint("\n");
    }
  }
  
  if( calcValsOnly )
  {
    wprint("  calculating values: %s\n", edit_getSortValString(sortCriteria).c_str() );
  }
  else
  {
    wprint("\n%d contours have been reordered\n", numContsChanged);
  }
}

