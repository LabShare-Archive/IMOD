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
    Revision 1.19  2008/07/28 05:27:59  tempuser
    *** empty log message ***

    Revision 1.18  2008/07/28 01:58:15  tempuser
    Made imodContourReduce the default smoothing algorithm

    Revision 1.17  2008/07/24 23:57:30  tempuser
    Better labels

    Revision 1.16  2008/07/24 23:56:51  tempuser
    Better labels

    Revision 1.15  2008/07/24 07:20:18  tempuser
    Better labels

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
     keysym != Qt::Key_X && keysym != Qt::Key_C && keysym != Qt::Key_V )
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
      edit_selectNextOverlappingCont();
      break;
    case Qt::Key_I:
      edit_inversePointsInContour(shift);
      break;
    case Qt::Key_Y:
      edit_goToContNextBiggestFindVal(shift,false,true); 
          // next biggest/smallest value
      break;
    case Qt::Key_B:
      edit_goToContNextBiggestFindVal(shift,true,false,(shift)?FLOAT_MAX:FLOAT_MIN );
          // recalculates
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
    plug.draw_reducePtsTol        = 0.05;
    plug.draw_reducePtsMinArea    = 0.5;
    plug.draw_reducePtsOpt        = RD_TOL;
    plug.draw_smoothMinDist       = 5;
    plug.draw_smoothTensileFract  = 0.5;
    plug.draw_deformRadius        = 30.0;
    
    plug.wheelBehav               = WH_DEFORMCIRCLE;
    plug.dKeyBehav                = DK_TOEND;
    plug.pgUpDownInc              = 1;
    plug.useNumKeys               = true;
    plug.markTouchedContsAsKey    = false;
    plug.selectVisibleOnly        = false;
    plug.wheelResistance          = 100;
    plug.showMouseInModelView     = false;
    plug.testIntersetAllObjs      = true;
    plug.selectedAction           = 0;
    plug.sortCriteria             = SORT_NUMPTS;
    plug.findCriteria             = SORT_NUMPTS;
    
    plug.sortCriteriaOfVals       = -1;
    Ipoint origin;
    setPt( &origin, 0,0,0);
    plug.copiedCont = imodContourNew();
    cont_generateCircle( plug.copiedCont, 40.0f, 500, origin, false );
    /*
    for(int i=0;i<25;i++)
      imodPointAppendXYZ(plug.copiedCont,0,i,0);
    for(int i=0;i<25;i++)
      imodPointAppendXYZ(plug.copiedCont,i,25,0);
    for(int i=0;i<25;i++)
      imodPointAppendXYZ(plug.copiedCont,25,25-i,0);
    for(int i=0;i<25;i++)
      imodPointAppendXYZ(plug.copiedCont,25-i,0,0);
    */
    //cont_generateCircle( plug.copiedCont, 40.0f, 100, origin, false );
        // puts a circle in copiedCont until the user copies his own contour
    
    plug.window->loadSettings();
    
    plug.initialized = true;
  }
  plug.view = inImodView;
  ivwTrackMouseForPlugs(plug.view, 1);
  ivwEnableStipple( plug.view, 1 );     // enables the display of stippled lines
  ivwGetImageSize(inImodView, &plug.xsize, &plug.ysize, &plug.zsize);
  
  //## INITIALIZE EXTRA OBJECT:
  
  plug.extraObjNum = ivwGetFreeExtraObjectNumber(plug.view);
  Iobj *xobj = ivwGetAnExtraObject(plug.view, plug.extraObjNum);
  imodObjectSetColor(xobj, 1.0, 0.0, 0.0);
  imodObjectSetValue(xobj, IobjFlagClosed, 1);
  ivwClearAnExtraObject(plug.view, plug.extraObjNum);  
  
  //## CREATE THE PLUGIN WINDOW:
  
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
      
      case(WH_PTS):
      {
        if( !isCurrContValid() )
          return 0;
        Imod *imod  = ivwGetModel(plug.view);
        Icont *cont = getCurrCont();
        int objIdx, contIdx, ptIdx;
        imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
        cycleIntWithinRange( ptIdx, 0, psize(cont)-1, scrollAmountInt );
        imodSetIndex(imod, objIdx, contIdx, ptIdx);
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
        Icont *cont = getCurrCont();
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
  
  
  //## IF BUTTON 1 AND SHIFT ARE DOWN: SCROLL SLICES
  
  if( plug.but1Down && plug.shiftDown )
  {
    float zapZoom = 1.0f;
    ivwGetTopZapZoom(plug.view, &zapZoom);
    edit_changeSelectedSlice( plug.changeY * zapZoom, true );
    return (1);
  }
  
  //## IF BUTTON 1 PRESSED, SELECT VISIBLE CONTOUR:   //%%%%% NOT WORKING PROPERLY YET
  
  if ( plug.selectVisibleOnly && plug.but1Pressed && !plug.but2Down && !plug.but3Down )
  {
    edit_setZapLocation( imx, imy, plug.mouse.z, false );//return 0;
    
    bool ptSelected = edit_selectVisiblePtNearCoords( &plug.mouse, 6.0f);
    //ivwDraw( plug.view, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC );    
    //ivwRedraw( plug.view );
    //if(!ptSelected)
    //  edit_setZapLocation( imx, imy, plug.mouse.z, false );//return 0;
    //float zoom = 1.0f;
    //ivwGetTopZapZoom(plug.view, &zoom);
    //ivwSetTopZapZoom(plug.view, zoom);    //%%%% WILL ASK DAVID TO CREATE
    
    //ivwSetLocationPoint(plug.view, &plug.mouse);
    ivwDraw( plug.view, IMOD_DRAW_XYZ );
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
  
  QString tolStr = 
    "When a contour is reduced: the higher this tolerance value "
    "\nthe more points are removed. "
    "\n"
    "\nRECOMMENDED VALUE: 0.05";
  
  lblTol = new QLabel("reduction tolerance:", grpOptions);
  lblTol->setFocusPolicy(QWidget::NoFocus);
  QToolTip::add(lblTol, tolStr);
  gridLayout1->addWidget(lblTol, 2, 0);
  
  QBoxLayout *box1 = new QBoxLayout(QBoxLayout::LeftToRight);
  fSmoothSpinner = (FloatSpinBox*)diaLabeledSpin(2,1,2000,1,"",grpOptions,box1);
  diaSetSpinBox( (QSpinBox*)fSmoothSpinner, int(plug.draw_reducePtsTol*100) );
  ((QSpinBox *)fSmoothSpinner)->setFocusPolicy(QWidget::ClickFocus);
  QObject::connect( (QSpinBox*)fSmoothSpinner,SIGNAL(valueChanged(int)),this,
                    SLOT(changeSmoothTol(int)));
  QToolTip::add( (QSpinBox*)fSmoothSpinner,
                 "The higher this value, the more point are removed "
                 "when you press [r] or 'Reduce Contours'"
                 "\n... a value of >1 is not recommended.");
  gridLayout1->addLayout(box1, 2, 1);
  
  setReducePtsOptionAndChangeDisplay( plug.draw_reducePtsOpt ); 
      // shows appropriate label and fSmoothSpinner value  
  
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
  plug.draw_reducePtsTol          = savedValues[2];
  plug.draw_reducePtsMinArea      = savedValues[3];
  plug.draw_reducePtsOpt          = savedValues[4];
  plug.draw_smoothMinDist         = savedValues[5];
  plug.draw_smoothTensileFract    = savedValues[6];
  plug.draw_deformRadius          = savedValues[7];
  plug.wheelBehav                 = savedValues[8];
  plug.dKeyBehav                  = savedValues[9];
  plug.pgUpDownInc                = savedValues[10];
  plug.useNumKeys                 = savedValues[11];
  plug.markTouchedContsAsKey      = savedValues[12];
  plug.selectVisibleOnly          = savedValues[13];
  plug.wheelResistance            = savedValues[14];
  plug.selectedAction             = savedValues[15];
  plug.selectedAction             = savedValues[16];
  plug.testIntersetAllObjs        = savedValues[17];
  plug.findCriteria               = savedValues[18];
}


//------------------------
//-- Saves most of the settings within DrawingToolsData in user preferences
//-- so they will load next time Bead Helper is started

void DrawingTools::saveSettings()
{
  double saveValues[NUM_SAVED_VALS];
  
  saveValues[0]   = plug.drawMode;
  saveValues[1]   = plug.draw_reducePts;
  saveValues[2]   = plug.draw_reducePtsTol;
  saveValues[3]   = plug.draw_reducePtsMinArea;
  saveValues[4]   = plug.draw_reducePtsOpt;
  saveValues[5]   = plug.draw_smoothMinDist;
  saveValues[6]   = plug.draw_smoothTensileFract;
  saveValues[7]   = plug.draw_deformRadius;
  saveValues[8]   = plug.wheelBehav;
  saveValues[9]   = plug.dKeyBehav;
  saveValues[10]  = plug.pgUpDownInc;
  saveValues[11]  = plug.useNumKeys;
  saveValues[12]  = plug.markTouchedContsAsKey;
  saveValues[13]  = plug.selectVisibleOnly;
  saveValues[14]  = plug.wheelResistance;
  saveValues[15]  = plug.selectedAction;
  saveValues[16]  = plug.selectedAction;
  saveValues[17]  = plug.testIntersetAllObjs;
  saveValues[18]  = plug.selectedAction;
  
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
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj  = getCurrObj();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int includeCType = 0;
  int contMin = 1;
  int contMax = nConts;
  
  string msg = ( plug.draw_reducePtsOpt == RD_TOL ) ?
    "-----"
    "\ntolerance  = " + toString(plug.draw_reducePtsTol) +
    "\n-----"
    "\nWARNING: reducing contours with "
    "\n a large 'tolerance' can result "
    "\n in an undesirable loss of "
    "\n information/contour detail."
    :
    "-----"
    "\nmin area  = " + toString(plug.draw_reducePtsMinArea) + " pix sq."
    "\n-----"
    "\nWARNING: reducing contours with "
    "\n a large 'min area' can result "
    "\n in an undesirable loss of "
    "\n information/contour detail.";
  
  QString toolStr = ( plug.draw_reducePtsOpt == RD_TOL ) ?
    "The 'tolerance' value represents how thorough the smoothing "
    "\nalgorithm will be while still trying to preserve the overall shape. "
    "\nA value of >1.0 is not recommended for reducing large numbers of contours"
    :
    "The 'min area' value represents the minimum area which must be between  "
    "\nany 3 consequtive points, else the middle point be deleted. "
    "\nA value of >5.0 is not recommended for reducing large numbers of contours";
  
	CustomDialog ds;
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour (inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addComboBox( "include:",
                  "all contours,"
                  "only key contours,"
                  "only interpolated",
                  &includeCType );
  ds.addLabel   ( msg.c_str(), toolStr );
	GuiDialogCustomizable dlg(&ds, "Reduce Contours",false);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  contMin -= 1;
  contMax -= 1;
  
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
    
    if( (includeCType == 1 && isInterpolated(cont)) ||
        (includeCType == 2 && !isInterpolated(cont)) )
      continue;
    
    undoContourDataChg( plug.view, objIdx, c );           // REGISTER UNDO
    int pointsDeleted;
    
    if( plug.draw_reducePtsOpt == RD_TOL )
    {
      pointsDeleted = cont_reducePtsTol( cont, plug.draw_reducePtsTol );
    }
    else
    {
      pointsDeleted = cont_reducePtsMinArea( cont, plug.draw_reducePtsMinArea,
                                    isContClosed(obj,cont) );
    }
    
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
  
  static int  includeCType     = 0;
  static bool roundZOpenPts    = true;
  static bool addPtEveryZ      = true;
  
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
  
  int contMin         = 1;
  int contMax         = nConts;
  
  CustomDialog ds;
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour (inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addComboBox( "include:",
                  "all contours,"
                  "only key contours,"
                  "only interpolated",
                  &includeCType );
	ds.addCheckBox( "round Z values (for open contours)", &roundZOpenPts );
  ds.addCheckBox( "add pt every Z (for open contours)", &addPtEveryZ );
  ds.addLabel   ( msg.c_str(), toolStr );
	GuiDialogCustomizable dlg(&ds, "Smooth Contours",false);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  contMin  -= 1;
  contMax  -= 1;
  
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
    if( (includeCType == 1 && isInterpolated(cont)) ||
        (includeCType == 2 && !isInterpolated(cont)) )
         continue;
    
    undoContourDataChg( plug.view, objIdx, c );           // REGISTER UNDO
    int pointsAdded = 
      cont_addPtsSmooth( cont, plug.draw_smoothMinDist, plug.draw_smoothTensileFract,
                         isContClosed(obj,cont), roundZOpenPts, addPtEveryZ );
    
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
      wprint("%d points deleted\n", ptsChanged);
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
      wprint("%d points deleted\n", ptsChanged);
      break;
    }
    
    case( DK_DELETEPT ):
    {
      undoPointRemovalCP(  plug.view );            // REGISTER UNDO
      imodPointDelete(cont, ptIdx);
      ptsChanged = 1;
      break;
    }
    
    case( DK_DELETECONT ):
    {
      undoContourRemoval( plug.view, objIdx, contIdx );  // REGISTER UNDO
      imodObjectRemoveContour( obj, contIdx );
      ptsChanged = 1;
      break;
    }
    
    case( DK_REMOVEPTSIZE ):
    {
      ptsChanged = ( !isDefaultSize(obj,cont,ptIdx) ) ? 1 : 0;
      removePtSize( cont, ptIdx );
      ptsChanged = 1;
      break;
    }
    
    case( DK_REMOVEALLPTSIZES ):
    {
      for(int p=0; p<psize(cont); p++)
        if( !isDefaultSize(obj,cont,p) )
          ptsChanged++;
      removePtsSize( cont );
      wprint("%d point sizes removed\n", ptsChanged);
      break;
    }
  }
  
  
  //## FINISH:
  
  if(ptsChanged)
    undoFinishUnit( plug.view );        // FINISH UNDO
  
  ivwRedraw( plug.view );
  return true;
}



//------------------------
//-- Selects the next contour after the currently selected one which overlaps
//-- another contour.

void DrawingTools::selectNextOverlappingContour()
{
  if( !isCurrContValid() )
  {
    MsgBox("No contour selected");
    return;
  }
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = getCurrObj();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int  overlapAction          = 1;
  
	CustomDialog ds;
  ds.addLabel   ( "action:" );
  ds.addRadioGrp( "action:",
                  "find next intersecting edge [a],"
                  "list all intersecting edges,"
                  "delete intersecting contours in current object",
                  &overlapAction,
                  "",
                  "Selects the first non-simple contour or \n"
                  "first contour intersecting another contour \n"
                  "beyond the currently selected contour - \n"
                  "searching all objects,"
                  "Prints out all contours in the model \n"
                  "which have edges intersecting some other contour,"
                  "Deletes any contour in the current object \n"
                  "which overlapping another contour in the \n"
                  "SAME object"
                  );
	ds.addCheckBox( "test edges with all object",
                  &plug.testIntersetAllObjs,
                  "if true each contour will only be tested for "
                  "crossing paths with other contours in all "
                  "closed objects, not just the same object." );
  ds.addLabel   ( "-----\n"
                  "NOTE:\n"
                  " > Only closed contours are tested for intersection \n"
                  " > A contour 'intersects' another only if their edges \n"
                  "    cross - not if one is completely inside the other\n"
                  " > Testing may take several minutes for large models" );
	GuiDialogCustomizable dlg(&ds, "Find Overlapping Contours", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  
  switch( overlapAction )
  {
    case(0):
    {
      edit_selectNextOverlappingCont();
      break;
    }
    
    case(1):
    {
      int numOverlappingConts = 0;
      int numNonSimpleConts = 0;
      wprint( "\nINTERSECTING CONTOURS:\n" );
      
      for (int o=0; o<osize(imod); o++)         // for each object:
      {
        Iobj *objO  = getObj(imod,o);
        if( !isObjClosed(objO) )
          continue;
        
        for (int c=0; c<csize(objO); c++)      // for each contour:
        {
          Icont *cont = getCont(objO, c);
          int pCross, objCross, contCross;
          bool samePts;
          int startObjIdx = (plug.testIntersetAllObjs) ? 0 : o;
          int endObjIdx   = (plug.testIntersetAllObjs) ? osize(imod)-1 : o;
          
          if( !cont_isSimpleSeg( cont, isContClosed(objO, cont), &pCross ) )
          {
            wprint("  obj %d cont %d \tAND ITSELF @ pt %d\n",
                   o+1, c+1, pCross+1 );
            numNonSimpleConts++;
          }
          if( edit_findOverlappingCont( o, c, &pCross, &objCross, &contCross, &samePts,
                                        startObjIdx, endObjIdx, false ) )
          {
            wprint("  obj %d cont %d \tAND   obj %d cont %d @ pt %d%s\n",
                   o+1, c+1, objCross+1, contCross+1, pCross+1, (samePts) ? " (*)" : "");
            numOverlappingConts++;
          }
        }
      }
      
      wprint("\n%d non-simple contours found\n", numNonSimpleConts);
      wprint("%d intersecting contours found\n", numOverlappingConts);
      
      break;
    }
    
    case(2):
    {
      int numContoursDeleted = 0;
      
      for (int c=csize(obj)-1; c>=0; c--)      // for each contour:
      {
        int startObjIdx = (plug.testIntersetAllObjs) ? 0 : objIdx;
        int endObjIdx   = (plug.testIntersetAllObjs) ? osize(imod)-1 : objIdx;
        
        int pCross, objCross, contCross;
        bool samePts;
        
        if( edit_findOverlappingCont( objIdx, c, &pCross, &objCross, &contCross, &samePts,
                                      startObjIdx, endObjIdx, false ) )
        {
          undoContourRemoval( plug.view, objIdx, c );            // REGISTER UNDO
          imodObjectRemoveContour( obj, c );
          numContoursDeleted++;
        }
      }
      
      if( numContoursDeleted )
        undoFinishUnit( plug.view );                      // FINISH UNDO
      
      wprint("%d overlapping contours deleted\n", numContoursDeleted);
      
      break;
    }
  }
  

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
    wprint("   interpolated \t= %d (%d%%)\n", stippledConts,   percentStippled );
  wprint("\n");
  wprint("   total pts   \t= %d\n", totPts );
  wprint("   avg pts/cont\t= %g\n", ptsPerCont );
  wprint("   avg line seg\t= %g\n", avgDistPts );
  wprint("   avg pt size\t= %g\n", avgPtSize );
  wprint("\n");
  wprint("   total length\t= %s\n", toStringWithCommas(int(totLen)).c_str() );
  wprint("   avg length \t= %d\n", int(avgLen) );
  wprint("   total area \t= %s\n", toStringWithCommas(int(totArea)).c_str() );
  wprint("   avg area  \t= %s\n",  toStringWithCommas(int(avgArea)).c_str() );
}


//------------------------
//-- Prints more detailed information about the current contour or range
//-- of contours including the position of points and segment length.

void DrawingTools::printContourDetailedInfo()
{
  if( !isCurrContValid() )
  {
    MsgBox("No contour selected");
    return;
  }
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = getCurrObj();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int contMin              = contIdx+1;
  int contMax              = contIdx+1;
  static int  printID      = 1;
  static bool usePixelLen  = true;
  
	CustomDialog ds;
  ds.addLabel   ( "contours to sort (inclusive):" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours after this contour "
                  "(inclusive) will be reordered" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour "
                  "(inclusive) will be reordered" );
  ds.addRadioGrp( "print:",
                  "summary only,"
                  "points,"
                  "segment lengths",
                  &printID,
                  "",
                  "Only prints basic info (area, length etc),"
                  "Lists the position of all points,"
                  "List the line segment distance for every point"
                  "to the next point"
                  );
	ds.addCheckBox( "show distances in pixels", &usePixelLen,
                  "If true measures all lengths etc in pixels \n"
                  "Else use the units in the model header." );
	GuiDialogCustomizable dlg(&ds, "Contour Printing", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  contMin  -= 1;
  contMax  -= 1;
  
  int numConts = contMax - contMin;
  if( numConts > 10 )
  {
    if( !MsgBoxYesNo(this,
                     "You are about to print ALL points over "
                     + toString(numConts) + " contours. \n"
                     "This could take could take MANY minutes... \n\n"
                     "Are you REALLY sure you want to print this many values?!" ) )
      return;
  }
  
  float unitMult  = (usePixelLen) ? 1.0f : imodGetPixelSize(imod);
  string unitsStr = (usePixelLen) ? "pixels" : toString( imodUnits(imod) );
  
  
  //## PRINT HEADER:
  
  wprint("\nCONTOUR SUMMARY (object %d):\n", objIdx+1);
  
  
  for(int c=contMin; c<=contMax; c++)
  {
    Iobj *cont  = getCont( obj, c );
    
    wprint("\nCONTOUR: %d\n", c+1);
    if( isEmpty(cont) ) {
      wprint(" ... empty\n");
      continue;
    }
    wprint("\n");
    
    //## CALCULATE PROPERTIES:
    
    Ipoint ll, ur;
    imodContourGetBBox( cont, &ll, &ur );
    Ipoint center = line_getPtHalfwayBetween( &ll, &ur );
    
    bool closed = isContClosed( obj, cont );
    bool clockwise = (imodContZDirection(cont) == IMOD_CONTOUR_COUNTER_CLOCKWISE);
    
    float area = imodContourArea(cont) * (unitMult*unitMult);
    float length = imodContourLength( cont, closed ) * unitMult;
    float lengthClosed = imodContourLength( cont, false ) * unitMult;
    float lengthOpen = imodContourLength( cont, true  ) * unitMult;
    
    //## PRINT POINTS OR LINE SEGMENT:
    
    if( printID == 1 )
    {
      wprint( " points:\n" );
      for( int p=0; p<psize(cont); p++ ) {
        Ipoint *pt = getPtNoWrap(cont,p);
        wprint("  %d \t(%g, %g, %g)\n", p+1,pt->x*unitMult,pt->y*unitMult,pt->z*unitMult );
      }
    }
    else if( printID == 2 )
    {
      wprint( " segments lengths:\n" );
      float cumulativeLen = 0;
      for( int p=0; p<psize(cont); p++ ) {
        float segLen  = imodPointDistance( getPt(cont,p), getPt(cont,p+1) ) * unitMult;
        cumulativeLen += segLen;
        int   segPercent = fDivide( cumulativeLen, length) * 100;
        wprint("  %i \t%g \t%g \t(%d%%)\n", p+1, segLen, cumulativeLen, segPercent );
      }
    }
    else
    {
      wprint( " # points: %d\n", psize(cont) );
    }
    
    //## PRINT CONTOUR SUMMARY:
    
    wprint("\n");
    wprint(" bounding box:\n");
    wprint("   min: %g,%g,%g\n", ll.x,ll.y,ll.z);
    wprint("   max: %g,%g,%g\n", ur.x,ur.y,ur.z);
    wprint("   center: %g,%g,%g\n", center.x,center.y,center.z);
    wprint("\n");
    wprint(" type: " );
    wprint( (closed) ? "(closed), " : "(open), " );
    wprint( (clockwise) ? " (clockwise)\n" : " (anti-clockwise)\n" );
    wprint("\n");
    wprint( " area:   \t%g square %s\n",  area, unitsStr.c_str() );
    wprint( " length: \t%g %s (OPEN)\n",  lengthClosed, unitsStr.c_str() );
    wprint( "         \t%g %s (CLOSED)\n", lengthOpen, unitsStr.c_str() );
  }
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
    wprint(" avg pts/cont = %g\n", ptsPerCont );
    wprint(" avg dist between pts = %g\n", avgDistPts );
  }
  
  float ptsPerContAll = fDivide( totPts, totConts );
  float avgDistPtsAll = fDivide( totLen, totPts );
  
  wprint("\n------------\n");
  wprint("OVERALL:\n");
  wprint(" # empty contours = %d\n", totEmpty );
  wprint(" # conts  = %d\n", totConts );
  wprint(" # pts    = %d\n", totPts );
  wprint(" avg pts/cont = %g\n", ptsPerContAll );
  wprint(" avg dist between pts = %g\n", avgDistPtsAll );
}



//------------------------
//-- Gives a choice of several other options for the user.

void DrawingTools::moreActions()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds;
  ds.addRadioGrp( "action:",
                  "clean model and fix contours,"
                  "find intersecting edges [a],"
                  "sort contours,"
                  "find contours [y],"
                  "delete contours,"
                  "copy contours,"
                  "transform contours,"
                  "move point,"
                  "expand contours,"
                  "print basic model info,"
                  "print detailed object info,"
                  "print detailed contour info",
                  &plug.selectedAction,
                  "",
                  "Contains a number of options to clean multiple \n"
                    "objects and 'fix' contours by removing bad or \n"
                    "points etc,"
                  "Finds closed contours which cross theie own "
                    "path or the path of other contours. \n"
                    "In most situations (eg: closed membranes) "
                    "no two lines should ever intersect, \n"
                    "especially a line crossing itself, "
                    "so it's a good idea to use this tool before \n"
                    "converting your contours into the final mesh model.,"
                  "Physically sorts contours using the criteria you "
                    "select,"
                  "Use [y] to find the contour or point with the "
                    "next biggest value based on the criteria you "
                    "select,"
                  "Allows you to delete any contours which meet your "
                    "specified criteria,"
                  "Use this to copy or move a range of contours from the "
                    "current object to another object,"
                  "Use this to precisely translate, scale and/or rotate "
                    "a range of contours in the current object,"
                  "Use this to move the current point or the current "
                    "object to a precise position,"
                  "Use this to expand a ring around a range of "
                    "open or closed contours within the current object,"
                  "Prints some basic information about the current "
                    "object including the average distance between "
                    "points; average points contour; and number of "
                    "empty contours.,"
                  "Prints detailed information about the "
                    "current object"
                  "Prints detailed information about the "
                    "current contour, or a range of them");
	GuiDialogCustomizable dlg(&ds, "Perform Action", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  //## EXECUTE ACTION:
  
  switch(plug.selectedAction)
  {
    case(0):      // clean model and fix contours
      cleanModelAndFixContours();
      break;
    case(1):      // find intersecting edges [a]
      selectNextOverlappingContour();
      break;
    case(2):      // sort contours
      sortContours();
      break;
    case(3):      // find contours
      findContours();
      break;
    case(4):      // delete contours
      deleteContours();
      break;
    case(5):      // copy or move contours
      copyOrMoveContourRange();
      break;
    case(6):      // transform contours
      tranformContourRange();
      break;
    case(7):      // move point(s)
      movePoint();
      break;
    case(8):      // expand contours
      expandContourRange();
      break;
    case(9):      // print basic model info
      printModelPointInfo();
      break;
    case(10):      // print detailed object info
      printObjectDetailedInfo();
      break;
    case(11):      // print detailed contour info
      printContourDetailedInfo();
      break;
  }
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Allows user to change other plugin values/settings.

void DrawingTools::moreSettings()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int newReducePtsOpt = plug.draw_reducePtsOpt;
  
	CustomDialog ds;
  ds.addLabel   ( "--- MOUSE ---" );
  ds.addComboBox( "wheel behavior:",
                  "none,"
                  "resize deform circle,"
                  "scroll slices,"
                  "scroll contours,"
                  "scroll pts,"
                  "resize curr point", &plug.wheelBehav,
                  "The action performed by the mouse wheel" );
  ds.addSpinBox ( "wheel resistance:",
                  10, 1000, &plug.wheelResistance, 10,
                  "The higher the value, the slower "
                  "mouse scrolling works" );
  ds.addCheckBox( "select visible contours only", 
                  &plug.selectVisibleOnly,
                  "If on: only visible contours will be selected \n"
                  "when [button 1] is pressed in all drawing modes." );
  ds.addLabel   ( "\n--- KEYBOARD ---" );
  ds.addCheckBox( "use number keys to change mode", 
                  &plug.useNumKeys,
                  "if on: number keys [1]-[5] are intercepted \n"
                  "and used to change the drawing mode, \n"
                  "if off: can use number keys to move points \n"
                  "as per normal");
  ds.addComboBox( "on [d] remove:",
                  "do nothing,"
                  "pts to end,"
                  "to nearest end,"
                  "current pt,"
                  "current contour,"
                  "pt size current pt,"
                  "pt sizes current cont", &plug.dKeyBehav,
                  "Action performed when [d] is pressed. \n"
                  "\n"
                  " > do nothing - as it sounds \n"
                  " > pts to end - deletes all points beyond the"
                  "current point (to the last point) \n"
                  " > to nearest end - deletes all points in "
                  "the current contour from the selected "
                  "point to the closest end (not inclusive)\n"
                  " > current pt - deletes current point "
                  "(same as [delete]) \n"
                  " > current pt - deletes current contour "
                  "(same as [shift]+[d], but easier to press) \n"
                  " > pt size current pt - resets the sphere "
                  "size of the current point to the default \n"
                  " > pt sizes current cont - resets the "
                  "sphere size (and any fine grain info) of all "
                  "points in the current contour");
  ds.addSpinBox ( "[PgUp]/[PgDown] increment:",
                  1, 1000, &plug.pgUpDownInc, 1,
                  "NOTE: Holding [Shift] when you press \n"
                  "[Page Up] or [Page Down] will cause it \n"
                  "to increment one slice (as normal)" );
  ds.addLabel   ( "\n--- DEFORM CIRCLE ---" );
  ds.addCheckBox( "mark contours as key after deform", 
                  &plug.markTouchedContsAsKey,
                  "If on: any stippled contour selected and/or "
                  "\ndeformed using the 'deform' or 'join' "
                  "\ntool will become unstippled." );
  ds.addLineEditF( "deform circle radius:",
                  &plug.draw_deformRadius, 0, 200, 3,
                  "The radius (in pixels) of the circle used "
                  "in deform and join drawing mode. \n"
                  "NOTE: You can also change this using "
                  "[q], [w] and the mouse wheel.");
  ds.addLabel   ( "\n--- OTHER ---" );
  ds.addComboBox( "smoothing meth:",
                  "segment threshold,"
                  "min area",
                  &newReducePtsOpt,
                  "The method used when [r] or 'Reduce Contours' "
                  "is used.\n"
                  "\n"
                  " > segment threshold - better adjusted "
                  "to preserve the curvature of contour segments \n"
                  " > min area - better adjusted "
                  "to remove sharp contours of nearby points" );
  ds.addCheckBox( "show mouse in model view", 
                  &plug.showMouseInModelView,
                  "Will show the mouse in any Model View "
                  "windows as you move it in the ZAP window. \n"
                  "WARNING: This will reduce performance!");
  
	GuiDialogCustomizable dlg(&ds, "More Settings", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  /*
  float newDeformRadius      = string_getFloatFromString( deformRadiusStr );
  if( newDeformRadius != 0 && newDeformRadius > 0.2 && newDeformRadius < 5000 )
    plug.draw_deformRadius = newDeformRadius;
  */
  
  if( newReducePtsOpt != plug.draw_reducePtsOpt )
    setReducePtsOptionAndChangeDisplay( newReducePtsOpt );
  
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
   
  int         contMin      = 1;
  int         contMax      = nConts;
  static bool reverseOrder = false;
  static bool printVals    = true;
  static bool calcValsOnly = false;
  
	CustomDialog ds;
  ds.addLabel   ( "contours to sort (inclusive):" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours after this contour "
                  "(inclusive) will be reordered" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour "
                  "(inclusive) will be reordered" );
  ds.addRadioGrp( "sort contours by:      (sort criteria)",
                  "number points,"
                  "contour length,"
                  "area,"
                  "clockwise area,"
                  "avg point size,"
                  "avg gray value,"
                  "interpolated,"
                  "random,"
                  "mean x,"
                  "mean y,"
                  "mean z,"
                  "min x,"
                  "min y,"
                  "min z",
                  &plug.sortCriteria,
                  "",
                  "Sorts by the number of points (empty first),"
                  "Length of the contours (open or closed - depending on object/contour),"
                  "Area of the contour (smallest first),"
                  "From largest anti-clockwise to no area to largest clockwise area,"
                  "Average point size over all points in the contour "
                    "(using object default if not set),"
                  "Uses the average gray value of the pixel closest to each point,"
                  "Stippled contours first,"
                  "Uses a random number for each contour,"
                  "Contour's center of mass in X,"
                  "Contour's center of mass in Y,"
                  "Contour's center of mass in Z"
                  );
  ds.addCheckBox( "calc values only (don't reorder)",
                  &calcValsOnly,
                  "No contours will be reordered, but "
                  "values will be calculated and you "
                  "can iterate from largest to smallest "
                  "by pressing [y]" );
  ds.addCheckBox( "reverse order", &reverseOrder );
  ds.addCheckBox( "print values", &printVals );
	GuiDialogCustomizable dlg(&ds, "Sorting Options", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  contMin -= 1;
  contMax -= 1;
  
  int numConts = contMax - contMin;
  if( printVals && numConts > 100 )
  {
    if( !MsgBoxYesNo(this,
                     "You are about to print values for "
                     + toString(numConts) + " contours. \n"
                     "Thich could take SEVERAL minutes... \n\n"
                     "Are you sure you want to print this many values?" ) )
      return;
  }
  
  edit_reorderConts( plug.sortCriteria, contMin, contMax, calcValsOnly,
                     reverseOrder, printVals );
  
  ivwRedraw( plug.view );
}



//------------------------
//-- Prompts for a criteria for reordering, and reorderes the specified range of 
//-- contours using this criteria.

void DrawingTools::findContours()
{
  if( !isCurrObjValidAndShown() )  {
    wprint("Must select valid object\n");
    return;
  }
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  
  static bool   useStartVal = true;
  static float  startVal = 0;
  
  CustomDialog ds;
  ds.addComboBox( "Find property:",
                  "contour number of points,"
                  "contour length,"
                  "contour area,"
                  "contour clockwise area,"
                  "contour avg point size,"
                  "contour avg gray value,"
                  "contour interpolated,"
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
                  &plug.findCriteria,
                  "The criteria used to advance through contours"
                  "and/or points using [y]\n" );
  ds.addCheckBox( "start search using value below",
                  &useStartVal,
                  "If not ticked will use the (find value) "
                  "of the selected contour/point instead." );
  ds.addLineEditF( "Value to find:               ",
                  &startVal, INT_MIN, INT_MAX, 5,
                  "The value to start searching for" );
  ds.addLabel   ( "-----\n"
                  "NOTE: Use [y] and [Y] to advance through\n"
                  "   points/contours with increasing\n"
                  "   value - according to the specified\n"
                  "   'find property' - compared to the\n"
                  "   currently selected point/contour." );
  
  GuiDialogCustomizable dlg(&ds, "Find Options", this);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  edit_goToContNextBiggestFindVal( false, true, !useStartVal, startVal );
}




//------------------------
//-- Allows the user to delete contours which on certain slices,
//-- and/or meet the specified criteria

void DrawingTools::deleteContours()
{
  if( !isCurrObjValidAndShown() )  {
    wprint("\aMust select valid object\n");
    return;
  }
  
  Imod  *imod  = ivwGetModel(plug.view);
  Iobj  *obj   = getCurrObj();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int oObjs  = osize(imod);
  int nConts = csize(obj);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int         contMin         = 1;
  int         contMax         = nConts;
  int         objMin          = 1;
  int         objMax          = oObjs;
  static int  includeCType    = 0;
  static int  includeRange    = 0;
  static bool criteriaSlice   = true;
  static bool criteriaPoints  = false;
  
	CustomDialog ds;
  ds.addLabel   ( "-----\n"
                  "contours to consider:" );
  ds.addComboBox( "include:",
                  "all contours (in range),"
                  "only key contours,"
                  "only interpolated",
                  &includeCType );
  ds.addRadioGrp( "include:",
                  "(1) object range;  all contours,"
                  "(2) contour range; current object",
                  &includeRange );
  ds.addLabel   ( "(1) object range:" );
  ds.addSpinBox ( "min:", 1, oObjs, &objMin, 1 );
  ds.addSpinBox ( "max:", 1, oObjs, &objMax, 1 );
  ds.addLabel   ( "(2) contour range (current obj):" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1 );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1 );
  ds.addLabel   ( "-----\n"
                  "criteria for deletion:" );
  ds.addCheckBox( "slices / z value (*)", &criteriaSlice );
  ds.addCheckBox( "number of points (*)", &criteriaPoints );
  ds.addLabel   ( "-----\n"
                  "(*) = if ticked, a new window \n"
                  "will appear for more input" );
  GuiDialogCustomizable dlg(&ds, "Contours to Delete",false);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  contMin -= 1;
  contMax -= 1;
  objMin -= 1;
  objMax -= 1;
  
  //## EVALUATE RANGE:
  
  if( includeRange == 0 )
  {
    contMin = 0;
    contMax = INT_MAX;
  }
  else
  {
    objMin = objIdx;
    objMax = objIdx;
  }
  
  static bool sliceIgnoreEnds  = true;
  static bool sliceSkipN       = true;
  static int  sliceN           = 2;
  vector<int> slicesToDelete;
  
  
  //## PROMPT FOR SLICE CRITERIA (IF WAS SELECTED):
  
  if( criteriaSlice )
  {
    int         sliceMin          = 1;
    int         sliceMax          = plug.zsize;
    
    CustomDialog ds1;
    ds1.addLabel    ( "delete contours on these slices:" );
    ds1.addSpinBox ( "min:", -10, plug.zsize, &sliceMin, 1 );
    ds1.addSpinBox ( "max:", 1, plug.zsize+10, &sliceMax, 1 );
    ds1.addCheckBox( "ignore top and bottom slice", &sliceIgnoreEnds );
    ds1.addCheckBox( "skip every Nth slice", &sliceSkipN );
    ds1.addSpinBox ( "  where N = ", 2, 100, &sliceN, 1 );
    GuiDialogCustomizable dlg1(&ds1, "Slices to Delete Contours",false);
    dlg1.exec();
    if( ds1.cancelled )
      return;
    
    sliceMin -= 1;
    sliceMax -= 1;
    
    
    int numSlices = 0;
    string listStr;
    for(int s=sliceMin; s<=sliceMax; s++)
    {
      if( sliceIgnoreEnds && (s == 0 || s == plug.zsize-1) )
        continue;
      if( sliceSkipN && (intMod(s+sliceMin,sliceN)==0) )
        continue;
      
      listStr += (numSlices==0) ? "    " : ((numSlices%10==0) ? ",\n    " : ", ");
      listStr += toString(s+1);
      slicesToDelete.push_back(s);
      numSlices++;
    }
    
    string msg = "Delete contours from " + toString(numSlices) + " slices\n"
      "List of slices: \n" + listStr;
    
    if( !MsgBoxYesNo( this, msg.c_str() ) )
      return;
  }
  
  
  //## PROMPT FOR POINT CRITERIA (IF WAS SELECTED):
  
  static int pointsMin  = 0;
  static int pointsMax  = 1;
  
  if( criteriaPoints )
  {
    CustomDialog ds2;
                           ds2.addLabel   ( "delete contours with" );
    int ID_PTSMIN        = ds2.addSpinBox ( "between:", 0, 999999, &pointsMin, 1 );
    int ID_PTSMAX        = ds2.addSpinBox ( "and:",     0, 999999, &pointsMax, 1 );
                           ds2.addLabel   ( "points (inclusive)" );
    GuiDialogCustomizable dlg2(&ds2, "Contours to Delete - Number Points",false);
    dlg2.exec();
    if( ds2.cancelled )
      return;
  }
  
  
  
  //## FIND AND DELETE CONTOURS WHICH MEET CRITERIA:
  
  int numContsDeleted = 0;
  int numCandidateConts = 0;
  
  for( int o=objMin; o<=objMax && o<osize(imod); o++ )
  {
    Iobj *objO = getObj(imod,o);
    
    for( int c=MIN(contMax,csize(objO)-1); c>=contMin; c-- )
    {
      Icont *cont = getCont(objO, c);
      numCandidateConts++;
      
      if( includeCType == 1 && isInterpolated( cont ) )
        continue;
      if( includeCType == 2 && !isInterpolated( cont ) )
        continue;
      
      if( criteriaSlice )
      {
        int z = getZInt( cont );
        if( !vector_doesElementExistInVector(slicesToDelete,z) )
          continue;
      }
      
      if( criteriaPoints )
      {
        if( psize(cont) > pointsMax && psize(cont) < pointsMin )
          continue;
      }
      
			undoContourRemoval( plug.view, o, c );              // REGISTER UNDO
			imodObjectRemoveContour( objO, c );
      numContsDeleted++;
    }
  }
  
  undoFinishUnit( plug.view );                      // FINISH UNDO
  
  int percentDeleted = numContsDeleted / numCandidateConts * 100.0f;
  wprint("%d contours deleted (%d%%)\n", numContsDeleted, percentDeleted );
}




//------------------------
//-- Allows user to select a range of contours in the current object
//-- to move or copy to another object

void DrawingTools::copyOrMoveContourRange()
{
  if( !isCurrObjValidAndShown() )  {
    wprint("\aMust select valid object\n");
    return;
  }
  
  Imod  *imod  = ivwGetModel(plug.view);
  Iobj  *obj   = getCurrObj();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int nConts = csize(obj);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int         contMin       = 1;
  int         contMax       = nConts;
  int         objToIdx      = osize(imod);
  static int  includeCType  = 0;
  static bool copy          = true;
  static bool createNewObj  = true;
  char *objName = imodObjectGetName(obj); 
  string newObjStr = toString(objName) + " 2";
  
	CustomDialog ds;
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour (inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addComboBox( "include:",
                  "all contours,"
                  "only key contours,"
                  "only interpolated",
                  &includeCType );
  ds.addLabel   ( "-----\n" );
  ds.addCheckBox( "copy contours", &copy );
  ds.addCheckBox( "create new object", &createNewObj,
                  "if true, will duplicate object properties \n"
                  "into a new object at the location below" );
  ds.addSpinBox ( "object to move to:", 1, osize(imod), &objToIdx, 1 );
  ds.addLineEdit( "duplicate object name:", &newObjStr,
                  "if a new object is created, this name will \n"
                  "be used in the duplicated object" );
	GuiDialogCustomizable dlg(&ds, "Copy or Move Contour Range",false);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  contMin  -= 1;
  contMax  -= 1;
  objToIdx -= 1;
  
  
  if( newObjStr.length() <= 0 ) 
    newObjStr = "dup";
    
  if( !createNewObj && objToIdx == objIdx )
  {
    wprint( "\aYou are copying/moving between the same object" );
  }
  
  
  //## CREATE NEW OBJECT AND COPY PROPERTIES (IF SPECIFED):
  
  Iobj *objTo;
  
  if( createNewObj )
  {
    //undoObjectAddition( plug.view, osize(imod) );                 // REIGSTER UNDO
    int error = imodNewObject(imod);
    if(error == 1)
    {
      wprint("\aError creating new object");
      return;
    }
    objToIdx = osize(imod)-1;
    objTo = getObj(imod, objToIdx );
    
    // NOTE: I tried using imodObjectDup and imodObjectCopy, but had problems,
    //       so instead I copy *most* of the objects properties individually.
    
    imodObjectSetValue( objTo, IobjLineWidth, imodObjectGetValue(obj,IobjLineWidth) );
    imodObjectSetValue( objTo, IobjLineWidth2, imodObjectGetValue(obj,IobjLineWidth2) );
    imodObjectSetValue( objTo, IobjPointSize, imodObjectGetValue(obj,IobjPointSize) );
    imodObjectSetValue( objTo, IobjFlagClosed, imodObjectGetValue(obj,IobjFlagClosed) );
    //imodObjectSetValue( objTo, IobjFlagConnected, imodObjectGetValue(obj,IobjFlagConnected) );
    //imodObjectSetValue( objTo, IobjFlagFilled, imodObjectGetValue(obj,IobjFlagFilled) );
    
    float red, green, blue;
    imodObjectGetColor( obj, &red, &green, &blue );
    imodObjectSetColor( objTo, red, green, blue );
    
    //char* text = (char *)newObjStr.c_str();
    //imodObjectSetName( objTo, (char *)newObjStr.c_str() );
    
    imodObjectSetName( objTo, imodObjectGetName(obj) );
    
    int nContsCopied = 0;
    
    for( int c=contMin; c<=contMax && c<csize(obj); c++ )
    {
      Icont *cont = getCont(obj, c);
      if( isEmpty(cont) )
        continue;
      if( includeCType == 1 && isInterpolated( cont ) )
        continue;
      if( includeCType == 2 && !isInterpolated( cont ) )
        continue;
      
      Icont *newCont = imodContourDup( cont );
      imodObjectAddContour( objTo, newCont );
      
      nContsCopied++;
    }
    
    undoFinishUnit( plug.view );                      // FINISH UNDO
    
    return;
  }
  else
  {
    objTo = getObj(imod,objToIdx);
  }
  
  
  //## EXECUTE COPY / MOVE:
  
  int nContsCopied = 0;
  
  for( int c=contMin; c<=contMax && c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj, c);
    setDeleteFlag( cont, 0 );
    
    if( isEmpty(cont) )
      continue;
    if( includeCType == 1 && isInterpolated( cont ) )
      continue;
    if( includeCType == 2 && !isInterpolated( cont ) )
      continue;
    
    Icont *newCont = imodContourDup( cont );
    undoContourAddition( plug.view, objToIdx, osize(objTo) );     // REGISTER UNDO
    imodObjectAddContour( objTo, newCont );
    
    nContsCopied++;
    if( !copy )
      setDeleteFlag( cont, 1 );
  }
  
  if(!copy) {
    removeAllDeleteFlaggedContoursFromObj(obj, objIdx);
  }
  
  undoFinishUnit( plug.view );                      // FINISH UNDO
  
  wprint("%d contours %s\n", nContsCopied, ((copy)?"copied":"moved") );
}


//------------------------
//-- Allows the user to precisely translate and/or rotate and/or scale a chosen
//-- range of contours from the current object.

void DrawingTools::tranformContourRange()
{
  if( !isCurrObjValidAndShown() && isCurrContValid() )  {
    wprint("\aMust select valid contour\n");
    return;
  }
  
  Imod  *imod  = ivwGetModel(plug.view);
  Iobj  *obj   = getCurrObj();
  Icont *cont  = getCurrCont();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int nConts = csize(obj);
  
  int   contMin       = contIdx+1;
  int   contMax       = contIdx+1;
  float translateX    = 0;
  float translateY    = 0;
  float translateZ    = 0;
  float scaleX        = 1;
  float scaleY        = 1;
  float rotateDegrees = 0;
  
  static int  includeCType  = 0;
  static bool copy          = true;
  static bool useMBRcenter  = false;
  
	CustomDialog ds;
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour \n"
                  "(inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour \n"
                  "(inclusive) will be changed" );
  ds.addLabel   ( "-----\n"
                  "translate:" );
  ds.addLineEditF( "x:", &translateX, INT_MIN, INT_MAX, 10 );
  ds.addLineEditF( "y:", &translateY, INT_MIN, INT_MAX, 10  );
  ds.addLineEditF( "z:", &translateZ, INT_MIN, INT_MAX, 10  );
  ds.addLabel   ( "-----\n"
                  "scale:" );
  ds.addLineEditF( "x:", &scaleX, INT_MIN, INT_MAX, 10  );
  ds.addLineEditF( "y:", &scaleY, INT_MIN, INT_MAX, 10  );
  ds.addLabel   ( "-----\n"
                  "rotate:" );
  ds.addLineEditF( "degrees:", &rotateDegrees, INT_MIN, INT_MAX, 10  );
  ds.addLabel   ( "-----\n"
                  "other options:" );
  ds.addCheckBox( "copy contours",  &copy );
  ds.addCheckBox( "scale and translate around MBR center", &useMBRcenter);
  
	GuiDialogCustomizable dlg(&ds, "Copy or Move Contour Range",false);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  contMin    -= 1;
  contMax    -= 1;
  
  //## DETERMINE IF TRANSFORM IS NECESSARY:
  
  bool doTranslate  = translateX!=0 || translateY!=0 || translateZ!=0;
  bool doScale      = scaleX!=1 || scaleY!=1;
  bool doRotate     = rotateDegrees!=0;
  
  if( !doTranslate && !doScale && !doRotate )
  {
    wprint("\aNo transforms were specified\n");
    return;
  }
  
  Ipoint translate;
  translate.x = translateX;
  translate.y = translateY;
  translate.z = (int)translateZ;
  
  Ipoint origin;
  origin.x = 0;
  origin.y = 0;
  origin.z = 0;
  
  
  //## EXECUTE TRANFORMS:
  
  int numChanged = 0;
  
  for( int c=contMin; c<=contMax && c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj, c);
    
    if( isEmpty(cont) )
      continue;
    
    if(copy)
    {
      Icont *contNew = imodContourDup(cont);
      undoContourAddition( plug.view, objIdx, osize(obj) );     // REGISTER UNDO
      imodObjectAddContour( obj, contNew );
    }
    
    undoContourDataChg( plug.view, objIdx, c );      // REGISTER UNDO
    
    if( useMBRcenter )
      cont_getCenterOfMBR( cont, &origin );
    
    if( doTranslate )
      cont_translate( cont, &translate );
    if( doScale )
      cont_scaleAboutPtXY( cont, &origin, scaleX, scaleY );
    if( doRotate )
      cont_rotateAroundPoint2D( cont, &origin, rotateDegrees );
    
    numChanged++;
  }
  
  undoFinishUnit( plug.view );                      // FINISH UNDO
  
  wprint("%d contours tranformed\n", numChanged );
}


//------------------------
//-- Allows the user to precisely translate and/or rotate and/or scale a chosen
//-- range of contours from the current object.

void DrawingTools::movePoint()
{
  if( !isCurrObjValidAndShown() && isCurrPtValid() )  {
    wprint("\aMust select a valid point\n");
    return;
  }
  
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = getCurrObj();
  Icont *cont = getCurrCont();
  Ipoint *pt  = getCurrPt();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  Ipoint ll, ur;
  imodContourGetBBox( cont, &ll, &ur );
  Ipoint center = line_getPtHalfwayBetween( &ll, &ur );
  
  string msg =
    "-----"
    "\nCurrent point:  " + toString(pt->x) + "," + 
                           toString(pt->y) + "," + toString(pt->z) +
    "\nCurrent contour: "
    "\n  min:" + toString(ll.x) + "," + toString(ll.y) + "," + toString(ll.z) +
    "\n  max:" + toString(ur.x) + "," + toString(ur.y) + "," + toString(ur.z) +
    "\n  center:" + toString(center.x) + "," + 
                    toString(center.y) + "," + toString(center.z);
  
  static bool moveRelative   = false;
  static bool moveWholeCont  = false;
  static bool moveMBRcenter  = false;
  
  float posX = (moveRelative) ? 0 : pt->x;
  float posY = (moveRelative) ? 0 : pt->y;
  float posZ = (moveRelative) ? 0 : pt->z;
  
  
	CustomDialog ds;
  ds.addLabel   ( "point position:" );
  ds.addLineEditF( "x:", &posX, INT_MIN, INT_MAX, 5 );
  ds.addLineEditF( "y:", &posY, INT_MIN, INT_MAX, 5 );
  ds.addLineEditF( "z:", &posZ, INT_MIN, INT_MAX, 5 );
  ds.addLabel   ( "-----\n"
                  "other options:" );
  ds.addCheckBox( "move relative to"
                  "\ncurrent position",  &moveRelative );
  ds.addCheckBox( "move whole contour",  &moveWholeCont );
  ds.addCheckBox( "move contour's MBR to here", &moveMBRcenter );
  ds.addLabel   ( msg.c_str() );
	GuiDialogCustomizable dlg(&ds, "Move Point(s)",false);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  
  //## DETERMINE TRANSLATE AMOUNT
  
  Ipoint translate;
  
  if( moveRelative )  // if we want to move relative to the current position:
  {
    translate.x = posX;
    translate.y = posY;
    translate.z = (int) (posZ);
  }
  else                // if we want to move to this absolute position:
  {
    translate.x = posX - pt->x;
    translate.y = posY - pt->y;
    translate.z = (int) ( posZ - pt->z);
  }
  
  if( moveWholeCont && moveMBRcenter )  // if we want to move the center of the MBR here:
  {
    translate.x = posX - center.x;
    translate.y = posY - center.y;
    translate.z = (int) (posZ - center.z);
  }
  
  
  if( translate.x==0 && translate.y==0 && translate.z==0 )
  {
    wprint("Translation by {0,0,0} - no action\n");
    return;
  }
  
  
  //## EXECUTE TRANFORMS:
  
  if( moveWholeCont )
  {
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    cont_translate( cont, &translate );
  }
  else
  {
    undoPointShiftCP( plug.view );
    pt->x += translate.x;
    pt->y += translate.y;
    pt->z += translate.z;
  }
  undoFinishUnit( plug.view );                      // FINISH UNDO
}




//------------------------
//-- Allows the user to specify a range of contours in the current object
//-- by a certain thickness. The contours which result from pushing the
//-- edge in and out are added to the object specified by the user.

void DrawingTools::expandContourRange()
{
  if( !isCurrObjValidAndShown() && isCurrContValid() )  {
    wprint("Must select valid contour\n");
    return;
  }
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = getCurrObj();
  Icont *cont = getCurrCont();
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int nConts = csize(obj);
  
  int           contMin          = contIdx+1;
  int           contMax          = contIdx+1;
  int           objToIdx         =  osize(imod);
  static float  radius   = 10;
  static int    minAngleChamfers = 20;
  static bool   open = true;
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds;
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour (inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addLabel   ( "-----\n"
                  "options:" );
  ds.addLineEditF( "radius to expand by:", &radius, 0, INT_MAX, 5 );
  ds.addSpinBox( "min angle for chamfers:",1,360,&minAngleChamfers,1 );
  ds.addCheckBox( "treat contours as open", &open );
  ds.addSpinBox ( "object for new contours", 1, osize(imod),&objToIdx, 1 );
  GuiDialogCustomizable dlg(&ds, "Expand Contours",false);
  dlg.exec();
  if( ds.cancelled )
    return;
  
  contMin    -= 1;
  contMax    -= 1;
  objToIdx    -= 1;
  
  //## EXECUTE EXPAND:
  
  Iobj *objTo = getObj(imod,objToIdx);
  int numChanged = 0;
  
  for( int c=contMin; c<=contMax && c<csize(obj); c++ )
  {
    Icont *cont = getCont(obj, c);
    
    if( isEmpty(cont) )
      continue;
    
    if( open )
    {
      Icont *newCont = imodContourNew();
      cont_expandOpenCont( cont, newCont, radius, minAngleChamfers, true );
      
      if( !isEmpty(newCont) )  {
        undoContourAddition( plug.view, objToIdx, osize(objTo) );     // REGISTER UNDO
        imodObjectAddContour( objTo, newCont );
      }
    }
    else
    {
      Icont *innerCont = imodContourNew();
      Icont *outerCont = imodContourNew();
      cont_expandClosedCont( cont, innerCont, outerCont, radius, minAngleChamfers );
      
      if( !isEmpty(innerCont) )  {
        undoContourAddition( plug.view, objToIdx, osize(objTo) );     // REGISTER UNDO
        imodObjectAddContour( objTo, innerCont );
      }
      if( !isEmpty(outerCont) )  {
        undoContourAddition( plug.view, objToIdx, osize(objTo) );     // REGISTER UNDO
        imodObjectAddContour( objTo, outerCont );
      }
    }
    numChanged++;
  }
  
  undoFinishUnit( plug.view );                      // FINISH UNDO
  
  wprint("%d contours were expanded\n", numChanged );

}




//------------------------
//-- Provides options to clean objects and make contours clockwise etc.

void DrawingTools::cleanModelAndFixContours()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  int nObjects = osize(imod);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int         objMin         = 1;
  int         objMax         = nObjects;
  static bool cleanConts     = true;
  static bool deleteDupPts   = true;
  static bool deleteRedPts   = true;
  static bool deleteOutPts   = true;
  static bool roundPts       = false;
  static bool makeCW         = true;
  static bool antiCW         = false;
  static bool makeSimple     = false;
  
	CustomDialog ds;
  ds.addLabel   ( "object range:" );
  ds.addSpinBox ( "min object:", 1, nObjects, &objMin, 1 );
  ds.addSpinBox ( "max object:", 1, nObjects, &objMax, 1 );
  ds.addLabel   ( "-----\n"
                  "point-wise options:" );
  ds.addCheckBox( "remove duplicate points", &deleteDupPts,
                  "deletes any point at the same coordinates \n"
                  "as the next point in the contour" );
  ds.addCheckBox( "remove straight points", &deleteRedPts,
                  "deletes any point which form a straight \n"
                  "line with the point before and after it" );
  ds.addCheckBox( "delete points outside tomogram",
                  &deleteOutPts,
                  "deletes any point which fall outside the "
                  "tomogram's boundaries in X, Y or Z ." );
  ds.addCheckBox( "round points to nearest slice",
                  &roundPts,
                  "rounds the Z value to the nearest slice "
                  "for any point not quite on a slice. \n"
                  "USE CAREFULLY on open contours" );
  ds.addLabel    ( "-----\n"
                   "contour options:" );
  ds.addCheckBox( "remove empty contours", &cleanConts,
                  "same effect as Edt >> Object >> Clean");
  ds.addCheckBox( "make contours clockwise", &makeCW,
                  "makes all contours clockwise or "
                  "anti-clockwise (if ticked below).\n"
                  "NOTE: not applied to contours "
                  "spanning multiple slices." );
  ds.addCheckBox( "    ... make anti-clockwise ", &antiCW,
                  "if above is ticked, all contours are made \n"
                  "counter-clockwise, instead of clockwise." );
  ds.addCheckBox( "fix non-simple closed contours *", &makeSimple,
                  "for any CLOSED contour path which intersects/overlaps itself, \n"
                  "the smaller enclosed areas are removed to make the contour simple.\n"
                  "USE CAREFULLY\n"
                  "CAN TAKE SEVERAL MINUTES" );
  
	GuiDialogCustomizable dlg(&ds, "Clean Model Options",false);
	dlg.exec();
	if( ds.cancelled )
		return;
  
  objMin    -= 1;
  objMax    -= 1;
  
  bool removePts    = deleteDupPts || deleteRedPts;
  int  contDir      = (antiCW) ? IMOD_CONTOUR_COUNTER_CLOCKWISE : IMOD_CONTOUR_CLOCKWISE;
  int  wrongContDir = (antiCW) ? IMOD_CONTOUR_CLOCKWISE : IMOD_CONTOUR_COUNTER_CLOCKWISE;
  
  Ipoint tomoll;
  Ipoint tomour;
  
  setPt( &tomoll, 0,0,0 );
  setPt( &tomour, plug.xsize, plug.ysize, plug.zsize );
  
  //## EXECUTE CLEAN:
  
  int totDupPtsRemoved = 0;
  int totRedPtsRemoved = 0;
  int totOutPtsRemoved = 0;
  int totPtsRounded    = 0;
  int totContsDeleted  = 0;
  int totContsReversed = 0;
  int totContsMadeSimp = 0;
  
  for( int o=objMin; o<=objMax && o<osize(imod); o++ )
  {
    Iobj *obj = getObj(imod,o);
    
    for( int c=csize(obj)-1; c>=0; c-- )
    {
      Icont *cont = getCont(obj, c);
      bool closed = isContClosed(obj,cont);
      
      if( cleanConts && isEmpty(cont) )
      {
        undoContourRemoval( plug.view, o, c );     // REGISTER UNDO
        imodObjectRemoveContour( obj, c );         // delete contour
        totContsDeleted++;
      }
      
      if( makeCW && imodContZDirection(cont) == wrongContDir && getZRange(cont)==0 )
      {
        undoContourDataChg( plug.view, o, c );     // REGISTER UNDO
        imodContourMakeDirection( cont, contDir );
        totContsReversed++;
      }
      
      if( deleteDupPts && cont_removeRedundantPts(cont,false, closed,false) )
      {
        undoContourRemoval( plug.view, o, c );     // REGISTER UNDO
        totDupPtsRemoved += cont_removeRedundantPts(cont,false, closed,true);
      }
      
      if( deleteRedPts && cont_removeRedundantPts(cont,true, closed,false) )
      {
        undoContourRemoval( plug.view, o, c );     // REGISTER UNDO
        totRedPtsRemoved += cont_removeRedundantPts(cont,true, closed,true);
      }
      
      if( deleteOutPts )
      {
        bool ptsOutside = false;
        for( int p=psize(cont)-1; p>0; p-- )
          if( !mbr_isPtInsideBBox( getPt(cont,p), &tomoll, &tomour ) )
            ptsOutside = true;
        
        if( ptsOutside )
        {
          undoContourDataChg( plug.view, o, c );     // REGISTER UNDO
          
          for( int p=psize(cont)-1; p>0; p-- )
            if( !mbr_isPtInsideBBox( getPt(cont,p), &tomoll, &tomour ) )
            {
              imodPointDelete(cont,p);
              totOutPtsRemoved++;
            }
        }
      }
      
      if( roundPts )
      {
        bool roundingNeeded = false;
        for( int p=psize(cont)-1; p>0; p-- )
          if( getPt(cont,p)->z != (float)((int)getPt(cont,p)->z) )
            roundingNeeded = true;
        
        if( roundingNeeded )
        {
          undoContourDataChg( plug.view, o, c );     // REGISTER UNDO
          
          for( int p=psize(cont)-1; p>0; p-- )
            if( getPt(cont,p)->z != float((int)getPt(cont,p)->z) )
            {
              getPt(cont,p)->z = roundToInt( getPt(cont,p)->z );
              totPtsRounded++;
            }
        }
      }
      
      if( makeSimple && closed && !cont_isSimple(cont,closed) )
      {
        undoContourDataChg( plug.view, o, c );     // REGISTER UNDO
        cont_makeSimple( cont );
        totContsMadeSimp++;
      }
    }
  }
  
  undoFinishUnit( plug.view );                      // FINISH UNDO
  
  wprint("\nCLEAN UP SUMMARY:\n" );
  if(deleteDupPts) wprint("  %d duplicate points removed\n", totDupPtsRemoved );
  if(deleteRedPts) wprint("  %d redundant points removed\n", totRedPtsRemoved );
  if(deleteOutPts) wprint("  %d point outside tomogram removed\n", totOutPtsRemoved );
  if(roundPts)     wprint("  %d point rounded in Z\n", totPtsRounded );
  if(cleanConts)   wprint("  %d empty contours removed\n", totContsDeleted );
  if(makeCW)       wprint("  %d contours made %s\n", totContsReversed,
                          (antiCW)? "anti-clockwise" : "clockwise" );
  if(makeSimple)   wprint("  %d non-simple contours fixed\n", totContsMadeSimp );
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
    centroidPt.z = plug.mouse.z;
    cont_translate( contNew, &centroidPt );    
  }
  int newContPos = edit_addContourToObj( getCurrObj(), contNew, true );
  undoFinishUnit( plug.view );        // FINISH UNDO
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
//-- Change draw_reducePtsTol or draw_reducePtsMinArea
//-- (depending on the value of draw_reducePtsOpt)

void DrawingTools::changeSmoothTol( int value ) { 
  if( plug.draw_reducePtsOpt == RD_TOL )
  {
    plug.draw_reducePtsTol = (float)value / 100.0f;
  }
  else
  {
    plug.draw_reducePtsMinArea = (float)value / 100.0f;
  }
}


//------------------------
//-- Change draw_reducePtsOpt

void DrawingTools::setReducePtsOptionAndChangeDisplay( int value )
{
  plug.draw_reducePtsOpt = value;
  
  if( plug.draw_reducePtsOpt == RD_TOL )
  {
    lblTol->show();
    ((QSpinBox *)fSmoothSpinner)->setValue( plug.draw_reducePtsTol * 100.0f );
  }
  else
  {
    lblTol->hide();
    ((QSpinBox *)fSmoothSpinner)->setValue( plug.draw_reducePtsMinArea * 100.0f );
  }
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




//------------------------
//-- Removes all contours in the object which have their delete flag set to 1

int removeAllDeleteFlaggedContoursFromObj( Iobj *obj, int objIdx )
{
	Icont *cont;
	int numRemoved = 0;
	for( int c=csize(obj)-1; c>=0; c-- )
	{
		cont = getCont(obj, c);
		if( isDeleteFlag( cont ) && isInterpolated( cont ) )
		{
      //undoContourRemovalCO( plug.view, objIdx, c );              // REGISTER UNDO
			undoContourRemoval( plug.view, objIdx, c );              // REGISTER UNDO
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
//-- Searches all contours in the current object and tries to find and select a
//-- point and contour with the given z value and within the specified distance of 
//-- the x, y coordinates and returns true if a point is actually found.
//-- NOTE: "distTolerance" represents the maximum distance (in tomogram pixels)
//--       the x, y coordinates must be from our point
//-- NOTE: This function is called by edit_executeDeformStart.

bool edit_selectContourPtNearCoordsCurrObj(float x, float y, int z, float distTolerance)
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
//-- Searches all visible objects and tries to find and select a point and contour
//-- with the given x, y, z coordinates and returns true if a point has been selected.
//-- NOTE: This function is called when the user right-clicks the ZAP
//--       window to select a different point.

bool edit_selectVisiblePtNearCoords( Ipoint *mouse, float distScreenPix)
{
  Imod *imod = ivwGetModel(plug.view);
  
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  if( objIdx < 0 )
    objIdx = 0;
  
  float zoom = 1.0f;
  ivwGetTopZapZoom(plug.view, &zoom);
  float dist = fDivide( distScreenPix, zoom );
  float sqDist = dist*dist;
  
  //## TRY CURRENT CONTOUR FIRST:
  
  if( isCurrContValid() )
  {
    Icont *cont = imodContourGet(imod);
    
    float closestDist;
    Ipoint closestPt;
    int closestPtIdx;
    cont_findClosestPtInContToGivenPt(mouse, cont,
                                      &closestDist, &closestPt, &closestPtIdx);
    if( closestDist < dist )
    {
      imodSetIndex(imod, objIdx, contIdx, closestPtIdx);
      return true;
    }
  }
  
  //## SEARCH ALL VISIBLE OBJECTS (STARTING WITH CURRENT OBJECT):
  
  for( int o=0; o<osize(imod); o++ )    // for each object:
  {
    int oIdx  = (o+objIdx) % osize(imod);    // search current object first
    Iobj *obj = getObj( imod, oIdx );
    bool objClosed = isObjClosed( obj );
    if( !isObjectValidAndShown(obj) )
      continue;
    
    for( int c=0; c<csize(obj); c++ )    // for each contour:
    {
      Icont *cont = getCont( obj, c );
      if( objClosed && getZInt(cont) != mouse->z )
        continue;
      
      for( int p=0; p<psize(cont); p++ )    // for each point:
      {
        if( !objClosed && getPt(cont,p)->z != mouse->z )
          continue;
        float sqDistBetweenPts = line_sqDistBetweenPts2D( mouse, getPt(cont,p) );
        if( sqDistBetweenPts < sqDist )
        {
          imodSetIndex(imod, oIdx, c, p);
          return true;
        }
      }
    }
  }
  
  return false;
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
    if(edit_selectContourPtNearCoordsCurrObj( plug.mouse.x, plug.mouse.y,
                                              (int)plug.mouse.z, minDist))
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
    
    float maxDistAllowedBetweenPts = MIN( plug.draw_smoothMinDist, radius*0.25f );
    
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
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
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
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
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
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( isContValid(cont) )
  {
    if( plug.draw_reducePtsOpt == RD_TOL )
    {
      return cont_reducePtsTol( cont, plug.draw_reducePtsTol );
    }
    else
    {
      return cont_reducePtsMinArea( cont, plug.draw_reducePtsMinArea,
                                    isContClosed(obj,cont) );
    }
  }
  return 0;
}

//------------------------
//-- Tries to smooth and increase the number of points in the
//-- current contour and returns the number of points added (or 0)

int edit_smoothCurrContour()
{
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
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
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
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
    
    //## GENERALIZE THESE CONTOURS:
    
    for( int i=0; i<(int)conts.size(); i++ )    // for each contour: reduce points
    {
      cont_reducePtsTol( conts[i].cont, MAX(plug.draw_reducePtsTol, 0.8f) );
    }
    
    //## DELETE ANY REALLY SMALL CONTOURS:
    
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
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  if( isContValid(cont) ) {
    cont_makeSimple( cont );
  }
}

//------------------------
//-- Deletes the current contour if it "too small"

void edit_deleteCurrContIfTooSmall()
{
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
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
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
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
      
      int ptCross;
      bool isSimple = cont_isSimpleSeg( contI, isContClosed(objO,contI), &ptCross  );
      if( !isSimple )
      {
        int p = (ptCross == 0) ? 0 : ((ptCross+1)%psize(contI));
        imodSetIndex( imod, o%nObjs, i, p );
        wprint("Non-simple polygon found!\n");
        
        ivwRedraw( plug.view );
        return (true);
      }
      
      int pCross, objCross, contCross;
      bool samePts;
      
      int endObjIdx = (plug.testIntersetAllObjs) ? osize(imod)-1 : oNum;
      
      if( edit_findOverlappingCont( oNum, i, &pCross, &objCross, &contCross, &samePts,
                                    oNum, endObjIdx, true ) )
      {
        imodSetIndex( imod, o, i, ((pCross+1)%psize(contI)) );
        
        if( samePts )
          wprint("\aTwo identical contours found!\n");
        else
          wprint("Intersecting contour found\n");
        
        ivwRedraw( plug.view );
        return (true);
      }
    }
  }
  
  wprint("No intersecting edges found.\n");
  
  return (false);
}


//------------------------
//-- Takes a contour, and returns true if it finds another contour within
//-- the specified range of objects which overlap it.
//-- It also retunrs the object index (objCross) and contour index (objCross)
//-- of the overlapping contour, plus the point index (pCross) of the first
//-- line segment in the origional contour which overlaps it.
//-- If (skipPrevContsInObj) is true, then it will skip all contours in the
//-- current object (objIdx) beyond the current contour (contIdx).

bool edit_findOverlappingCont( int objIdx, int contIdx,
                               int *pCross, int *objCross, int *contCross, bool *samePts,
                               int minObj, int maxObj, bool skipPrevContsInObj )
{
  Imod *imod  = ivwGetModel(plug.view);
  Imod *objI  = getObj(imod,objIdx);
  Imod *contI = getCont(objI,contIdx);
  
  if( isEmpty(contI) )
    return (false);
  
  Ipoint contIll, contIur;                // get bounding box for contI
  imodContourGetBBox( contI, &contIll, &contIur );
  
  bool contIClosed = isContClosed(objI,contI);
  
  for (int o=minObj; o<=maxObj && o<osize(imod); o++)  // for each object in range:
  {
    Iobj *obj  = getObj(imod,o);
    if( !isObjClosed(obj) )
      continue;
    
    int c = (skipPrevContsInObj && o==objIdx) ? contIdx+1 : 0;    // starting contour
    for(; c<csize(obj); c++)     // for each contour ahead:
    {
      Icont *contJ = getCont( obj, c );
      if( isEmpty(contJ) )
        continue;
      
      if( ( getZInt(contJ) == getZInt(contI) )  )     // if on same slice:
      {
        Ipoint contJll, contJur;                // get bounding box for contJ
        imodContourGetBBox( contJ, &contJll, &contJur );
        
        int pt1Cross, pt2Cross;
        if( mbr_doBBoxesOverlap2D( &contIll, &contIur, &contJll, &contJur )
            && cont_doCountoursCrossAndWhere( contI, contJ,
                                              contIClosed, isContClosed(obj,contJ),
                                              &pt1Cross, &pt2Cross )
            && !(o==objIdx && c == contIdx) )
        {
          *pCross = pt1Cross;
          *objCross = o;
          *contCross = c;
          *samePts = cont_isEqual(contI, contJ);
          
          return (true);
        }
      }
    }
  }
  
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
    if(targetVal == FLOAT_MAX)
      wprint("--> selecting LARGEST value...\n");
    else if(targetVal == FLOAT_MIN)
      wprint("--> selecting SMALLEST value...\n");
    else
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
    Icont *cont   = getCont(obj,c);
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

