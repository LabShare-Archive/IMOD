/*
 *  drawingtools.cpp -- Special plugin for contour drawing tools
 *
 */

/*****************************************************************************
 *   Copyright (C) 2007 by Andrew Noske from the Institute for Molecular     *
 *   Bioscience at the University of Queensland (Australia)                  *
 *****************************************************************************/

/*  
 *  $Id$
 *   Use hg log to see log
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
#include <QTime>
#include <QVBoxLayout>
#include <QWheelEvent>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QGridLayout>
#include <QKeyEvent>
#include <QEvent>
#include <QHBoxLayout>
#include <qtoolbutton.h>
#include <QDesktopServices>
#include <QUrl>
#include <qcompleter.h>
#include "../../3dmod/pegged.xpm"
#include "../../3dmod/unpegged.xpm"

#include "_common_functions.h"
#include "customdialog.h"
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

const char *imodPlugInfo(int *type)
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
  
  int keysym  = event->key();            // key value (Key_A, Key_Space... etc)
  int ctrl    = event->modifiers() & Qt::ControlModifier;   // ctrl modifier
  int shift   = event->modifiers() & Qt::ShiftModifier;     // shift modifier
  
  
  if(ctrl &&             // if the control key is down I typically want IMOD to handle it
     keysym != Qt::Key_X && keysym != Qt::Key_C && keysym != Qt::Key_V )
    return 0;
  
  if( !plug.useNumKeys && keysym >= Qt::Key_1 && keysym <= Qt::Key_9
      && event->modifiers() & Qt::KeypadModifier )    // if user presses 1-9 on keypad
    return 0;
  
  switch(keysym)
  {
    case Qt::Key_Q:
      plug.window->changeSculptCircleRadius( (shift) ? -5 : -1 );
      plug.window->drawExtraObject(true);
      break;
    case Qt::Key_W: 
      plug.window->changeSculptCircleRadius( (shift) ? 5 : 1 );
      plug.window->drawExtraObject(true);
      break;
    case Qt::Key_R:
      if(shift)
        return 0;
      plug.window->reduceCurrentContour();
      break;
    case Qt::Key_E:
      plug.window->smoothCurrentContour( !shift );
      break;
      
    case Qt::Key_D:
      if (shift || plug.dKeyBehav == DK_NONE)
        return 0;
      plug.window->executeDAction();
      break;
      
    case Qt::Key_A:
      if(shift)
        return 0;
      edit_selectNextIntersectingCont();
      break;
    case Qt::Key_I:
      edit_inversePointsInContour(shift);
      break;
    case Qt::Key_Y:
      edit_goToContNextBiggestFindVal(shift,false,true); 
          // next biggest/smallest value
      break;
    case Qt::Key_L:
        edit_goToContNextBiggestFindVal(shift,true,false,(shift)?FLOAT_MAX:FLOAT_MIN );
          // recalculates
      break;
      
    //case Qt::Key_N:           // can't seem to intercept N
    //  if( plug.drawMode==DM_CURVE && !shift && !ctrl )
    //    edit_executeCurve( true );
    //  break;
      
    case Qt::Key_Question:
    case Qt::Key_Space:
      if( plug.drawMode==DM_CURVE && !shift && !ctrl )
        edit_executeCurve( true );
      else
        return plug.window->copyCurrContToView(shift);
      break;
      
    //case Qt::Key_T:                  // temporary testing purposes - comment out
    //  if(ctrl)
    //    plug.window->test();
    //  else
    //    return 0;
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
      
      
    case Qt::Key_Up:
      if( plug.useArrowKeys )
				//edit_changeZTopZap( 1 );
        edit_changeSelectedSlice( 1,false, false );
      else
        return 0;
      break;
    case Qt::Key_Down:
      if( plug.useArrowKeys )
        edit_changeSelectedSlice( -1,false, false );
      else
        return 0;
      break;
      
    case Qt::Key_Left:
      if( plug.useArrowKeys && plug.pgUpDownInc>1 )
        edit_changeSelectedSlice( -plug.pgUpDownInc,false, false );
      else
        return 0;
      break;
    case Qt::Key_Right:
      if( plug.useArrowKeys && plug.pgUpDownInc>1 )
        edit_changeSelectedSlice( plug.pgUpDownInc,false, false );
      else
        return 0;
      break;
      
    case Qt::Key_PageUp:
      if(shift || plug.pgUpDownInc==1)
        return 0;
      else
        edit_changeSelectedSlice( plug.pgUpDownInc,false, false );
      break;
    case Qt::Key_PageDown:
      if(shift || plug.pgUpDownInc==1)
        return 0;
      else
        edit_changeSelectedSlice( -plug.pgUpDownInc,false, false );
      break;
      
    case Qt::Key_1:      plug.window->changeModeSelected( 0 );       break;
    case Qt::Key_2:      plug.window->changeModeSelected( 1 );       break;
    case Qt::Key_3:      plug.window->changeModeSelected( 2 );       break;
    case Qt::Key_4:      plug.window->changeModeSelected( 3 );       break;
    case Qt::Key_5:      plug.window->changeModeSelected( 4 );       break;
    case Qt::Key_6:      plug.window->changeModeSelected( 5 );       break;
    case Qt::Key_7:      plug.window->changeModeSelected( 6 );       break;
    case Qt::Key_8:      plug.window->changeModeSelected( 7 );       break;
      
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
    
    if( plug.sculptRadius <=0 || plug.warpRadius <=0 )
    {
      wprint( "\aWARNING: Bad values may have been loaded into DrawingTools" );
      plug.sculptRadius = 30;
      plug.warpRadius   = 30;
    }
    
    plug.initialized = true;
  }
	
	if( plug.copiedCont==NULL )
	{
		Ipoint origin;
    setPt( &origin, 0,0,0);
		plug.copiedCont = imodContourNew();
    cont_generateCircle( plug.copiedCont, 40.0f, 500, origin, false );
		// puts a circle in copiedCont until the user copies his own contour
	}
	
	
  plug.view = inImodView;
  ivwTrackMouseForPlugs(plug.view, 1);
  ivwEnableStipple( plug.view, 1 );     // enables the display of stippled lines
  ivwGetImageSize(inImodView, &plug.xsize, &plug.ysize, &plug.zsize);
  
  //## INITIALIZE EXTRA OBJECT:
  
  plug.extraObjText = ivwGetFreeExtraObjectNumber(plug.view);
  Iobj *xobjT = ivwGetAnExtraObject(plug.view, plug.extraObjText);
  imodObjectSetColor(xobjT, 0.0f, 0.0f, 0.0f);		// white
  imodObjectSetValue(xobjT, IobjLineWidth2, 2);
  imodObjectSetValue(xobjT, IobjFlagClosed, 0);		// open
  
	plug.extraObjWPts = ivwGetFreeExtraObjectNumber(plug.view);
  Iobj *xobjWP = ivwGetAnExtraObject(plug.view, plug.extraObjWPts);
  imodObjectSetColor(xobjWP, 0.0f, 0.0f, 1.0f);			// blue
	imodObjectSetValue(xobjWP, IobjFlagConnected, 0);	// scattered point object
	imodObjectSetValue(xobjWP, IobjPointSize, 0);			// zero sphere size
  imodObjectSetValue(xobjWP, IobjLineWidth2, 1);		// square
	imodObjectSetValue(xobjWP, IobjSymType, 2);
	imodObjectSetValue(xobjWP, IobjSymSize, 1);
	
  plug.extraObjNum = ivwGetFreeExtraObjectNumber(plug.view);
  Iobj *xobj = ivwGetAnExtraObject(plug.view, plug.extraObjNum);
  imodObjectSetColor(xobj, 1.0f, 0.0f, 0.0f);			// red
  imodObjectSetValue(xobj, IobjLineWidth2, plug.lineDisplayWidth);
  imodObjectSetValue(xobj, IobjFlagClosed, 1);		// closed
  
	plug.extraObjLW = ivwGetFreeExtraObjectNumber(plug.view);
  Iobj *xobjL = ivwGetAnExtraObject(plug.view, plug.extraObjLW);
  imodObjectSetColor(xobjL, 1.0f, 1.0f, 0.0f);		// yellow
  imodObjectSetValue(xobjL, IobjLineWidth2, 2);
  imodObjectSetValue(xobjL, IobjFlagClosed, 1); 	// open
	
	plug.extraObjLWPts = ivwGetFreeExtraObjectNumber(plug.view);
  Iobj *xobjP = ivwGetAnExtraObject(plug.view, plug.extraObjLWPts);
  imodObjectSetColor(xobjP, 1.0f, 0.0f, 0.0f);		// red
	imodObjectSetValue(xobjP, IobjFlagConnected, 0);		// scattered point object
	imodObjectSetValue(xobjP, IobjPointSize, 2);				// sphere size
  imodObjectSetValue(xobjP, IobjLineWidth2, 2);
	imodObjectSetValue(xobjP, IobjSymType, 1);
	imodObjectSetValue(xobjP, IobjSymSize, 4);
	
  //## CREATE THE PLUGIN WINDOW:
  
  plug.window  = new DrawingTools(imodDialogManager.parent(IMOD_DIALOG),"Drawing Tools");
  plug.window->setWindowTitle("Drawing Tools (*by SLASH*)");		// to help with our grant
	
  imodDialogManager.add((QWidget *)plug.window, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)plug.window, IMOD_DIALOG );
  
  plug.window->checkForNamelessObjects(false);    // check for objects without names
  
  //plug.window->show();
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
    float scrollAmount    = fDiv( wheelEvent->delta(), float(plug.wheelResistance) );
    int   scrollAmountInt = floor(scrollAmount);
    
    //## ZOOM:
    
    if( wheelEvent->modifiers() & Qt::ControlModifier )     // if [ctrl] is down:
    {
      float zoom;
      int error = ivwGetTopZapZoom(plug.view, &zoom);
      if( error != 1 )
      {
        //float zoomChange = (scrollAmount < 0) ? 0.9f : 1.1f;
        float zoomChange = 1 + MAX(-0.5f,scrollAmount*0.05f);
        float newZoom = ABS(zoom * zoomChange);
        
        if( newZoom > 100 )
          newZoom = 100;
        if( newZoom < 0.0001 )
          newZoom = 0.0001;
        
        if( newZoom != zoom )
          ivwSetTopZapZoom( plug.view, newZoom, true );
      }
      return 1;
    }
    
    //## SMART POINT RESIZE MODE:
    
    if ( plug.smartPtResizeMode && isCurrPtValid() )
    {
      Imod *imod  = ivwGetModel(plug.view);
      Iobj *obj   = imodObjectGet(imod);
      Icont *cont = imodContourGet(imod);
      if( cont!=NULL && obj!=NULL && !isObjClosed(obj) && imodPointGet(imod)!=NULL)
      {
        int objIdx, contIdx, ptIdx;
        imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
        float ptSize = imodPointGetSize(obj,cont,ptIdx);
        ptSize = MAX(0.0f, ptSize+(0.1f*scrollAmount));
				if( ptIdx >= 0 && ptIdx<psize(cont) )
					imodPointSetSize(cont,ptIdx,ptSize);
        //plug.window->drawExtraObject(false);
        ivwRedraw(plug.view);
        return (1);
      }
    }
    
    //## SWITCH WHEEL BEHAVIOR:
    
    switch( plug.wheelBehav )
    {
      case(WH_SCULPTCIRCLE):
      {
        if( plug.drawMode == DM_SCULPT
            || plug.drawMode == DM_WARP
					  || plug.drawMode == DM_JOIN
					  || plug.drawMode == DM_ERASER
					  || plug.drawMode == DM_WAND
					  || plug.drawMode == DM_CORRECT
					  || plug.drawMode == DM_JOIN )
        {
          plug.window->changeSculptCircleRadius( scrollAmount, plug.shiftDown );
          plug.window->drawExtraObject(true);
          return 1;
        }
        break;
      }
      
      case(WH_SLICES):
      {
        edit_changeSelectedSlice( scrollAmountInt, true );
        return 1;
        break;
      }
      
      case(WH_CONTS):
      {
        if( !isCurrObjValidAndShown() )
          return 0;
        Imod *imod  = ivwGetModel(plug.view);
        Iobj *obj   = imodObjectGet(imod);
        int objIdx, contIdx, ptIdx;
        imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
        cycleIntWithinRange( contIdx, 0, csize(obj)-1, scrollAmountInt );
        imodSetIndex(imod, objIdx, contIdx, 0);
        plug.window->drawExtraObject(false);
        ivwRedraw(plug.view);
        return 1;
        break;
      }
      
      case(WH_PTS):
      {
        if( !isCurrContValid() )
          return 0;
        Imod *imod  = ivwGetModel(plug.view);
        Icont *cont = imodContourGet(imod);
        int objIdx, contIdx, ptIdx;
        imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
        cycleIntWithinRange( ptIdx, 0, psize(cont)-1, scrollAmountInt );
        imodSetIndex(imod, objIdx, contIdx, ptIdx);
        plug.window->drawExtraObject(false);
        ivwRedraw(plug.view);
        return 1;
        break;
      }
      
      case(WH_PTSIZE):
      {
        Imod *imod  = ivwGetModel(plug.view);
        if( !isCurrObjValidAndShown() || imodPointGet(imod)==NULL )
          return 0;
        Iobj *obj   = imodObjectGet(imod);
        Icont *cont = imodContourGet(imod);
        int objIdx, contIdx, ptIdx;
        imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
        float ptSize = imodPointGetSize(obj,cont,ptIdx);
        ptSize = MAX(0.0f, ptSize+scrollAmount);
				if( ptIdx >= 0 && ptIdx<psize(cont) )
					imodPointSetSize(cont,ptIdx,ptSize);
        //plug.window->drawExtraObject(false);
        ivwRedraw(plug.view);
        return 1;
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
  
  //## REGENERATE SCULPT CIRCLE:
  
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
  
  plug.shiftDown = (event->modifiers() & Qt::ShiftModifier);
  
  
  //## IF BUTTON 1 AND SHIFT ARE DOWN: SCROLL SLICES
  
  if( plug.but1Down && plug.shiftDown )
  {
    float zapZoom = 1.0f;
    ivwGetTopZapZoom(plug.view, &zapZoom);
    edit_changeSelectedSlice( plug.changeY * zapZoom, true );
    return (1);
  }
	
  if( plug.but1Down && !plug.but2Down && !plug.but3Down &&
		  !(plug.but1Pressed && plug.drawMode==DM_LIVEWIRE) )
    return (0);         // to fix Pete's problem.
  
	
	
  //## IF ANY BUTTON WAS JUST PRESSED: GET THE CENTROID OF THE CURRENT CONTOUR
  
  if ( plug.but3Pressed || plug.but2Pressed || plug.but1Pressed )
  {
    Icont *cont = imodContourGet( ivwGetModel(plug.view) );
    if( isContValid(cont) ) {
      Ipoint ll, ur;
      imodContourGetBBox(cont, &ll, &ur);
      plug.centerPt = line_getPtHalfwayBetween( &ll, &ur );
    }
    plug.mouseDownPrev = plug.mouseDownPt;
		plug.mouseDownPt   = plug.mouse;
  }
  
  
  //## EXIT EARLY IF NO ACTION IS NEEDED:
  
  bool but2Used = plug.but2Pressed || plug.but2Down  || plug.but2Released;
  bool but3Used = plug.but3Pressed || plug.but3Down  || plug.but3Released;
  
  bool actionNeeded = but2Used ||
										 (plug.but1Pressed && plug.drawMode==DM_LIVEWIRE) ||
	                   (but3Used &&
                              ( (plug.drawMode==DM_SCULPT && plug.scupltBut3Warp)
                              || plug.drawMode==DM_JOIN
                              || plug.drawMode==DM_TRANSFORM
                              || plug.drawMode==DM_ERASER  
                              || plug.drawMode==DM_WARP
															|| plug.drawMode==DM_WAND
															|| plug.drawMode==DM_CIRCLE
                              || plug.drawMode==DM_MEASURE) );
  
  if ( !(actionNeeded) )            // if no action is needed: do nothing
    return (2);
  
  
	
	Imod *imod  = ivwGetModel(plug.view);
	Icont *cont = imodContourGet(imod);
	
  //## PERFORM ACTION:
  
  switch( plug.drawMode )
  {
    case (DM_SCULPT):
    {
      if( plug.but2Pressed ) {
        edit_executeSculptStart();
      }
      else if ( plug.but2Down ) {
        edit_executeSculpt();
      }
      else if( plug.but2Released ) {
        edit_executeSculptEnd();
      }
      
      else if( plug.scupltBut3Warp ) 
      {
        if( plug.but3Pressed ) {
          edit_executeWarpStart();
        }
        else if ( plug.but3Down ) {
          edit_executeWarp();
        }
        else if( plug.but3Released ) {
          edit_executeWarpEnd();
        }
      }
      
      break;
    }
    
    case (DM_JOIN):
    {
      if( plug.but2Pressed ) {
        edit_executeSculptStart();
      }
      else if ( plug.but2Down ) {
        edit_executeSculpt();
      }
      else if( plug.but2Released ) {
        edit_executeJoinEnd();
      }
      else if( plug.but3Released ) {
        edit_executeJoinRectEnd();
      }
      break;
    }
    
    case (DM_TRANSFORM):
    {
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
          else if ( plug.but3Down )       // scale currently selected contour
          {
            undoContourDataChgCC( plug.view );
            float scaleX = 1.0f + (plug.changeX / 100.0f);
            float scaleY = 1.0f + (plug.changeY / 100.0f);
            if( plug.transformBut3Unif )
              scaleX = scaleY;
            cont_scaleAboutPtXY( cont, &plug.centerPt, scaleX, scaleY );
          }
          else if (plug.but2Released || plug.but3Released )
          {
            undoFinishUnit( plug.view );          // FINISH UNDO
          }
        }
        else
        {
          if ( plug.but2Down )            // rotate currently selected contour
          {
            undoContourDataChgCC( plug.view );
            cont_rotateAroundPoint2D( cont, &plug.centerPt, plug.changeY );
          }
          else if ( plug.but3Down )       // strech currently selected contour
          {
            float distMovedAway = line_distBetweenPts2D(&plug.centerPt,&plug.mouse) -
              line_distBetweenPts2D(&plug.centerPt,&plug.mousePrev);
            float  stretchFactor = 1.0 + (fDiv((distMovedAway),
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
    
		case(DM_LIVEWIRE):
		{
			if( plug.but2Pressed )							// execute livewire
			{
				edit_executeLivewireClick();
			}
			else if( plug.but1Pressed )					// execute livewire select
			{
				if (plug.livewire==NULL || edit_executeLivewireSelectPt() == false);
					return (2);
			}
			break;
		}
		
		case(DM_WAND):
		{
			if( plug.but2Down && plug.but2Pressed )		// execute wand click
			{
				plug.wandAvgGrayVal = ivwGetValue( plug.view, plug.mouse.x,
																					 plug.mouse.y, plug.mouse.z );
			}
			else if( plug.but2Released )							// execute wand complete
			{
				edit_executeWandAdd();
			}
			
			if( isCurrContValid() && but3Used )				// little correct circle
			{
				float correctCircleRadius = MAX(plug.sculptRadius / 5.0f, 5.0f);
				
				if( plug.but3Pressed )
					undoContourDataChgCC( plug.view );      // REGISTER UNDO
				if ( plug.but3Down )
					edit_executeSculptPush( plug.mouse, correctCircleRadius );
				else if( plug.but3Released )
					edit_executeMinorCorrectEnd( correctCircleRadius );
			}
			
			break;
		}
		
    case (DM_ERASER):
    {
      if( !plug.shiftDown )
      {
        if ( plug.but2Down )              // delete contours touching sculpt circle
        {
          edit_eraseContsInCircle(plug.mouse, plug.sculptRadius);
        }
        else if (plug.but3Down )          // delete points within sculpt circle
        {  
          edit_erasePointsInCircle(plug.mouse, plug.sculptRadius);
        }
      }
      else
      {
        if ( plug.but2Down||plug.but3Down ) // break closed contours using sculpt circle  
        {
          edit_breakPointsInCircle(plug.mouse, plug.sculptRadius);
        }
      }
      break;
    }
    
    case (DM_WARP):
    {
      if( plug.but2Pressed || plug.but3Pressed ) {
        edit_executeWarpStart();
      }
      else if ( plug.but2Down || plug.but3Down ) {
        edit_executeWarp();
      }
      else if( plug.but2Released || plug.but3Released ) {
        edit_executeWarpEnd();
      }
      break;
    }      
    
    case (DM_CURVE):
    {
      if( plug.but2Pressed || plug.but2Down ) {
        edit_executeCurve( false );
      }
      break;
    }
		
		case (DM_CIRCLE):
    {
      if( plug.but2Released ) {
        edit_executeCircleEnd();
      }
			else
			{
				if( isContValid(cont) && plug.but3Down )   // strech currently selected contour
				{
					float distMovedAway = line_distBetweenPts2D(&plug.centerPt,&plug.mouse) -
					line_distBetweenPts2D(&plug.centerPt,&plug.mousePrev);
					float stretchFactor = 1.0 + (fDiv((distMovedAway),
															(line_distBetweenPts2D(&plug.centerPt,&plug.mouse)+0.1f)));
					float angle = line_getAngle2D( &plug.centerPt, &plug.mouse );
					undoContourDataChgCC( plug.view );
					cont_stretchAlongAngle( cont, &plug.centerPt, angle, stretchFactor );
				}
				else if ( plug.but3Released )
				{
					undoFinishUnit( plug.view );          // FINISH UNDO
				}
			}
      break;
    } 
		
		case (DM_CORRECT):
    {
      if( plug.but2Pressed && isCurrContValid() )
			{
				float correctCircleRadius = MAX(plug.sculptRadius / 5.0f, 5.0f);
				float distMouseCurrPt = (cont==NULL) ? FLOAT_MAX
				                      : cont_minDistPtAndContourPts2D( &plug.mouse, cont, true );
				bool isMouseInRange = distMouseCurrPt < (correctCircleRadius*2.0f);
				
				if( isMouseInRange )
				{
					Icont *newCont  = imodContourDup( cont );
					int ptsAfterCorrection = edit_contCorrectCircle( newCont, plug.mouse,
																													 correctCircleRadius, 10,
																													 plug.mouseDownPrev );
					if( ptsAfterCorrection > 10 )
					{
						undoContourDataChgCC( plug.view );        // REGISTER UNDO
						cont_copyPts( newCont, cont, true );
						ivwRedraw( plug.view );
					}
					free(newCont);
				}
				
				
        edit_executeCircleEnd();
				undoFinishUnit( plug.view );          // FINISH UNDO
			}
			
			else if( isCurrContValid() && but3Used )				// little correct circle
			{
				float correctCircleRadius = MAX(plug.sculptRadius / 5.0f, 5.0f);
				
				if( plug.but3Pressed )
					undoContourDataChgCC( plug.view );      // REGISTER UNDO
				if ( plug.but3Down )
					edit_executeSculptPush( plug.mouse, correctCircleRadius );
				else if( plug.but3Released )
					edit_executeMinorCorrectEnd( correctCircleRadius );
			}
			
      break;
    } 
		
    case (DM_MEASURE):
    {
      if( plug.but2Released )
      {
        float pixelSize = imodGetPixelSize(imod);
        char *unitsChs = imodUnits(imod);
        Ipoint scalePt;        setPt( &scalePt, 1,1, imodGetZScale(imod) );
        float dist = imodPoint3DScaleDistance(&plug.mouseDownPt, &plug.mouse, &scalePt);
        float unitDist = dist * pixelSize;
        
        wprint( "LENGTH = %f pixels\n", dist );
        wprint( "       = %f %s\n", unitDist, unitsChs );
      }
      break;
    }
    
    default:    // DM_NORMAL
    {
      if( plug.smartPtResizeMode && plug.but2Pressed && isCurrPtValid() )
      {
        Iobj *obj   = imodObjectGet(imod);
        int objIdx, contIdx, ptIdx;
        imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
        
        if( !cont || !obj || isObjClosed(obj) )
          return (2);
        
        float prevPtSize = imodPointGetSize(obj,cont,ptIdx);
        
        if( prevPtSize==0 )
          return (2);
        
        undoPointAdditionCC( plug.view, ptIdx+1 );        // REGISTER UNDO
        imodPointAdd( cont, &plug.mouse, ptIdx+1 );
				if( (ptIdx+1) >= 0 && (ptIdx+1)<psize(cont) )
					imodSetIndex(imod, objIdx, contIdx, ptIdx+1);
        undoFinishUnit( plug.view );                      // FINISH UNDO
        
				if( (ptIdx+1) >= 0 && (ptIdx+1)<psize(cont) )
					imodPointSetSize(cont, ptIdx+1, prevPtSize);
        imodPointSetSize(cont, ptIdx,   prevPtSize);
        
        ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );    
        
        return (1);
      }
      
      return (2);
    }
  }
  
      // NOTE if we get to here we have dealt with the action, so we redraw and return 1
  
  ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );    
  return (1);
}


//############################################################

//----------------------------------------------------------------------------
//
//          DrawingTools METHODS:
//
//----------------------------------------------------------------------------


//## WINDOW CLASS CONSTRUCTOR:

static const char *buttonLabels[] = {(char*)"Done", (char*)"Video", (char *)"Help"};
static const char *buttonTips[]   = {(char*)"Close this plugin window",
	                             (char*)"See SLASH help videos showing \n"
															   			"how to use this plugin",
														   (char*)"Open help window"};

DrawingTools::DrawingTools(QWidget *parent, const char *name) :
  DialogFrame(parent, 3, buttonLabels, buttonTips, true, "Drawing Tools", "", name)
{
  const int LAY_MARGIN   = 4;
  const int LAY_SPACING  = 4;
  const int GROUP_MARGIN    = 1;
  
	//## RECOLOR MIDDLE "Video" BUTTON:
	
	mButtons[1]->setStyleSheet("color: rgb(150, 180, 255);");
	mButtons[1]->setFlat( true );
	mButtons[1]->setCursor( Qt::PointingHandCursor );
	mButtons[2]->setCursor( Qt::PointingHandCursor );
	
	
  //## Drawing Mode:
  
  QGroupBox *typeGbox = new QGroupBox("Drawing Mode:", this);
  typeButtonGroup = new QButtonGroup(this);
  QVBoxLayout *gbLayout = new QVBoxLayout(typeGbox);
  gbLayout->setSpacing(0);
  gbLayout->setContentsMargins(2, 2, 2, 2);
  
  connect(typeButtonGroup, SIGNAL(buttonClicked(int)), this, SLOT(changeMode(int)));
  
	typeLabel[DM_NORMAL]    = "Normal";					// 1 (defaults to 1st slow)
	typeLabel[DM_WARP]      = "Warp";						// 2
	typeLabel[DM_SCULPT]    = "Sculpt";					// 3
	typeLabel[DM_JOIN]      = "Join";						// 4
	typeLabel[DM_LIVEWIRE]  = "Livewire";				// 5
	typeLabel[DM_WAND]      = "The Wand";				// 6
	typeLabel[DM_ERASER]    = "Eraser";					// 7
	typeLabel[DM_MEASURE]   = "Measure";				// 8
	typeLabel[DM_TRANSFORM] = "Transform";				// |-- extra drawing modes 
	typeLabel[DM_CURVE]     = "Curve";						// |   (hidden by default)
	typeLabel[DM_CIRCLE]    = "Circle";						// | 
	typeLabel[DM_CORRECT]   = "Correct";					// | 
	
	typeTooltip[DM_NORMAL]    = "Contours are drawn normally";
	typeTooltip[DM_WARP]      = "Quickly correct bad regions of contour by "
															"dragging/warping the edges";
	typeTooltip[DM_SCULPT]    = "Draw and modify closed contours quickly using "
															"the sculpt circle to push or pinch lines";
	typeTooltip[DM_JOIN]      = "Join or split contours quickly by making overlaps";
	typeTooltip[DM_LIVEWIRE]  = "An edge detection tool to help you quickly "
															"trace around dark membranes";
	typeTooltip[DM_ERASER]    = "Erase contours instantly by clicking them";
	typeTooltip[DM_MEASURE]   = "Quickly measure the distance between two points";
	typeTooltip[DM_TRANSFORM] = "Lets you move, rotate and scale the selected contour";
	typeTooltip[DM_CURVE]     = "Click points and then have these stright lines turn into "
															"a curve when you connect back to the first point";
	typeTooltip[DM_CIRCLE]    = "Quickly draw a circle by dragging point to point";
	typeTooltip[DM_WAND]      = "Click and drag to auto-contour a small";
	typeTooltip[DM_CORRECT]   = "Allows you to quickly delete segments of a contour "
	                            "within, or cut off by the circle";
	
	
	for(int i=0; i<NUM_TOOLS_SHOWN; i++)
	{
		widType[i] = new QWidget( this );
		layType[i] = new QHBoxLayout( widType[i] );
		layType[i]->setSpacing(0);
		layType[i]->setContentsMargins(0, 0, 0, 0);
		widType[i]->setLayout( layType[i] );
		
		radType[i] = new QRadioButton(typeGbox);
		radType[i]->setText( typeLabel[i] );
		radType[i]->setFixedHeight(20);
		typeButtonGroup->addButton( radType[i], i);								
		
		btnType[i] = new QPushButton( this );
		btnType[i]->setText( "["  + QStr(i+1) + "]" );
		btnType[i]->setFixedSize(30,20);
		btnType[i]->setFlat(true);
		
		layType[i]->addWidget( radType[i] );
		layType[i]->addWidget( btnType[i] );
		
		gbLayout->addWidget( widType[i] );
	}
	
	changeRadioOptions();
	changeRadioToMatchMode( plug.drawMode );
	
  
  
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
  toolBut->setToolTip("Keep drawing tools window on top");
  
  pinLay->setAlignment( Qt::AlignTop );
  pinLay->addWidget(toolBut);
  topLay->addWidget(typeGbox);
  topLay->addLayout(pinLay);
  mLayout->addLayout(topLay);
  
  //## Actions:
  
  grpActions = new QGroupBox("Modify Contours:", this);
  //grpActions->setFocusPolicy(Qt::NoFocus);
  //grpActions->setMargin(GROUP_MARGIN);
  
  vboxLayout1 = new QVBoxLayout(grpActions);
  vboxLayout1->setSpacing(LAY_SPACING);
  vboxLayout1->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  
  reduceContsButton = new QPushButton("Reduce [r]", grpActions);
  reduceContsButton->setFocusPolicy(Qt::NoFocus);
  connect(reduceContsButton, SIGNAL(clicked()), this, SLOT(reduceConts()));
  reduceContsButton->setToolTip(
                "Reduces (removes points from) a range of contours "
                "\nin the current object");
  vboxLayout1->addWidget(reduceContsButton);
  
  smoothContsButton = new QPushButton("Smooth [e]", grpActions);
  smoothContsButton->setFocusPolicy(Qt::NoFocus);
  connect(smoothContsButton, SIGNAL(clicked()), this, SLOT(smoothConts()));
  smoothContsButton->setToolTip(
                "Smooths (adds points to) a range of contours "
                "\nin the current object... (use with caution)");
  vboxLayout1->addWidget(smoothContsButton);
  
  mLayout->addWidget(grpActions);
  
  
  //## Extra Buttons:
  
  widget1 = new QWidget(this);
  
  gridLayout2 = new QGridLayout(widget1);
  gridLayout2->setSpacing(LAY_SPACING);
  gridLayout2->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  
  keyboardSettingsButton = new QPushButton("Mouse / Keyboard", widget1);
  connect(keyboardSettingsButton, SIGNAL(clicked()), this, SLOT(keyboardSettings()));
  keyboardSettingsButton->setToolTip( "Contains several mouse & keyboard "
                                      "related settings");
  gridLayout2->addWidget(keyboardSettingsButton, 0, 0, 1, 2);
  
  moreActionsButton = new QPushButton("Actions", widget1);
  connect(moreActionsButton, SIGNAL(clicked()), this, SLOT(moreActions()));
  moreActionsButton->setToolTip( "Contains several other actions I didn't "
                                 "want to sqeeze into this window");
  gridLayout2->addWidget(moreActionsButton, 1, 0);
  
  moreSettingsButton = new QPushButton("Settings", widget1);
  connect(moreSettingsButton, SIGNAL(clicked()), this, SLOT(moreSettings()));
  moreSettingsButton->setToolTip(
                "Contains several other settings I didn't want to sqeeze "
                "into this window");
  gridLayout2->addWidget(moreSettingsButton, 1, 1); 
  
  mLayout->addWidget(widget1);
  
  mLayout->addStretch();
  this->adjustSize();
  
  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
}

//------------------------
//-- Used to set the radio buttons representing option types in the order
//-- specified by the user.

void DrawingTools::changeRadioOptions()
{
	for(int i=0; i<NUM_TOOLS_SHOWN; i++)
	{
		if( plug.modeOrder[i] < 0 || plug.modeOrder[i] >= NUM_TOOLS )
			plug.modeOrder[i] = i;
		
		int newMode = plug.modeOrder[i];
		radType[i]->setText( typeLabel[newMode] );
		
		bool hasOptions = ( newMode==DM_LIVEWIRE || newMode==DM_WAND );
		btnType[i]->setFlat( !hasOptions );
		disconnect( btnType[i], 0, 0, 0);			// disconnect anything already connected.
		
		if( newMode==DM_LIVEWIRE )
		{
			connect( btnType[i], SIGNAL(clicked()), this, SLOT( showLivewireOptions() ) );
			btnType[i]->setToolTip( "Change livewire settings" );
		}
		else if( newMode==DM_WAND )
		{
			connect( btnType[i], SIGNAL(clicked()), this, SLOT( showWandOptions()) );
			btnType[i]->setToolTip( "Change wand settings" );
		}
		else
		{
			connect( btnType[i], SIGNAL(clicked()), this, SLOT( customizeToolOrder() ) );
			btnType[i]->setToolTip( "Click to change what tools appear" );
		}
		
		bool hideRow = (newMode==DM_NORMAL && i>0);
		widType[i]->setVisible( !hideRow );
	}
}



//## SLOTS:


//------------------------
//-- Accesses the extra object and draw a red sculpt circle and/or other
//-- reference contour at the last recorded position of the mouse. What is
//-- drawn depends on what drawing mode is selected.

bool DrawingTools::drawExtraObject( bool redraw )
{
  if ( !plug.window )
    return false;
  
  //## CLEAR EXTRA OBJECT:
  
  Iobj *xobj  = ivwGetAnExtraObject(plug.view, plug.extraObjNum);
  Iobj *xobjT = ivwGetAnExtraObject(plug.view, plug.extraObjText);
	Iobj *xobjL = ivwGetAnExtraObject(plug.view, plug.extraObjLW);
	Iobj *xobjP = ivwGetAnExtraObject(plug.view, plug.extraObjLWPts);
	Iobj *xobjW = ivwGetAnExtraObject(plug.view, plug.extraObjWPts);
	
  if ( !xobj || !xobjT || !xobjL || !xobjP || !xobjW )
    return false;
  imodObjectSetValue(xobj,  IobjFlagExtraInModv, (plug.showMouseInModelView)?1:0);
  imodObjectSetValue(xobjT, IobjFlagExtraInModv, (plug.showMouseInModelView)?1:0);
	imodObjectSetValue(xobjL, IobjFlagExtraInModv, (plug.showMouseInModelView)?1:0);
	imodObjectSetValue(xobjP, IobjFlagExtraInModv, (plug.showMouseInModelView)?1:0);
	imodObjectSetValue(xobjW, IobjFlagExtraInModv, 0);
	
  ivwClearAnExtraObject(plug.view, plug.extraObjNum);
  ivwClearAnExtraObject(plug.view, plug.extraObjText);
	ivwClearAnExtraObject(plug.view, plug.extraObjLW);
	ivwClearAnExtraObject(plug.view, plug.extraObjLWPts);
	ivwClearAnExtraObject(plug.view, plug.extraObjWPts);
	
  Icont *xcont  = imodContourNew();    // primary closed contour used in extra object
  Icont *xcont2 = imodContourNew();    // open   contour used in extra object
  Icont *xcont3 = imodContourNew();    // open   contour used in extra object
                                           // NOTE: it's rare to use all these at once
  
  imodContourSetFlag(xcont, ICONT_CURSOR_LIKE | ICONT_MMODEL_ONLY, 1);
  setOpenFlag(xcont,  0);
	setOpenFlag(xcont2, 1);
  setOpenFlag(xcont3, 1);
  
  
  //## GET Z VALUE:
  
  Imod *imod  = ivwGetModel(plug.view);
  int ix, iy,iz;
  ivwGetLocation(plug.view, &ix, &iy, &iz);
  plug.mouse.z = iz;
  
  float x = plug.mouse.x;
  float y = plug.mouse.y;
  float z = plug.mouse.z;
  
  float radius = plug.sculptRadius;
  float hRadius = radius*0.5f;
  float qRadius = radius*0.25f;
  
  float zapZoom = 1.0f;                 // gets the zoom of the top-most zap window
  int noZap = ivwGetTopZapZoom(plug.view, &zapZoom); 
  float sc = fDiv( 1.0f, zapZoom);   // tomogram distance for one screen pixel 
  
  
  //## IF REQUESTED, DRAW Z HINT:
  
  if( plug.drawZhint )
  {    
    const float xOffset = 5;
    const float xOverhang = 10;
    const float yOffset = 3;
    float xOffsetRight = plug.xsize - xOffset;
    
    for(int i=0; i<plug.zsize; i++)
    {
      float fractionUpZ = fDiv( i, plug.zsize-1 );
      float fullHeight  = (plug.drawZhint==4) ? plug.zsize : plug.ysize;
      float yTopPos    = (fullHeight - yOffset) * fractionUpZ + yOffset;
      float yBottomPos = -(fullHeight) * (1.0 - fractionUpZ);
      
      Icont *xcontZhint  = imodContourNew();    // open   contour used in extra object
      Icont *xcontZhint2 = imodContourNew();    // open   contour used in extra object
      Icont *xcontZhint3 = imodContourNew();    // open   contour used in extra object
      Icont *xcontZhint4 = imodContourNew();    // open   contour used in extra object
      
      if( plug.drawZhint == 1 )
      {
        cont_generateBox(xcontZhint,xOffset,yTopPos,-1,-(yTopPos-yOffset),i);
        imodPointAppendXYZ( xcontZhint, xOffset+xOverhang, yTopPos, i );
      }
      
      if( plug.drawZhint == 2 )
      {
        cont_generateBox(xcontZhint,xOffset,yTopPos,-1,-(yTopPos-yOffset),i);
        imodPointAppendXYZ( xcontZhint, xOffset+xOverhang, yTopPos, i );
        
        cont_generateBox(xcontZhint2,xOffset,yBottomPos,-1,-yBottomPos+0.01,i);
        imodPointAppendXYZ( xcontZhint2, xOffset+xOverhang, yBottomPos, i );
      }
      
      if( plug.drawZhint == 3 )
      {
        cont_generateBox(xcontZhint,xOffset,yTopPos,-1,-(yTopPos-yOffset),i);
        imodPointAppendXYZ( xcontZhint, xOffset+xOverhang, yTopPos, i );
        
        cont_generateBox(xcontZhint2,xOffset,yBottomPos,-1,-yBottomPos+0.01,i);
        imodPointAppendXYZ( xcontZhint2, xOffset+xOverhang, yBottomPos, i );
        
        cont_generateBox(xcontZhint3,xOffsetRight,yTopPos,-1,-(yTopPos-yOffset),i);
        imodPointAppendXYZ( xcontZhint3, xOffsetRight-xOverhang, yTopPos, i );
        
        cont_generateBox(xcontZhint4,xOffsetRight,yBottomPos,-1,-yBottomPos+0.01,i);
        imodPointAppendXYZ(xcontZhint4,xOffsetRight-xOverhang,yBottomPos, i );
      }
      
      if( plug.drawZhint == 4 )
      {
        cont_generateBox(xcontZhint2,xOffset,yBottomPos,-1,-yBottomPos+0.01,i);
        cont_generateBox(xcontZhint2,xOffsetRight,yBottomPos,1,-yBottomPos+0.01,i);
        
        cont_generateBox(xcontZhint3,xOffset,yTopPos,-1,-(yTopPos-yOffset),i);
        cont_generateBox(xcontZhint3,xOffsetRight,yTopPos,-1,-(yTopPos-yOffset),i);
        cont_generateBox(xcontZhint3,xOffsetRight,plug.ysize+yTopPos,-1,-yTopPos,i);
        cont_generateBox(xcontZhint3,xOffset,plug.ysize+yTopPos,-1,-yTopPos,i);
      }
      
      imodObjectAddContour(xobj, xcontZhint);
      imodObjectAddContour(xobj, xcontZhint2);
      imodObjectAddContour(xobj, xcontZhint3);
      imodObjectAddContour(xobj, xcontZhint4);
      free(xcontZhint);
      free(xcontZhint2);
      free(xcontZhint3);
      free(xcontZhint4);
    }
  }
  
  
  
  //## IF CHANGING Z HEIGHT: DRAW RECTANGLE REPRESENTING SLICES
  
  if( plug.but1Down && plug.shiftDown )   // draw rectangle and bar representing z slices
  {
    int currSlice;
    ivwGetTopZapZslice(plug.view, &currSlice);
    float xmin = x - 20*sc;
    float xmax = xmin + 10*sc;
    float ymin = y - (currSlice-2)*sc;
    float ymax = ymin + plug.zsize*sc;
    
    cont_generateBox( xcont, xmin, ymin, 10*sc, plug.zsize*sc, currSlice, true);
    
    imodPointAppendXYZ( xcont, xmin,     y, currSlice );
    imodPointAppendXYZ( xcont, xmax,     y, currSlice );
    imodPointAppendXYZ( xcont, xmin,     y, currSlice );
		
    imodObjectAddContour(xobj, xcont);
    free(xcont);
    imodContourDelete(xcont2);
    imodContourDelete(xcont3);
    
		if( redraw )
			ivwRedraw( plug.view );
		return true;
  }
  
  
  //## CONSTRUCT EXTRA CONTOURS:
  
  switch( plug.drawMode )
  {
  case (DM_NORMAL):            // draw a tiny verticle line:
    {
      imodPointAppendXYZ( xcont, x, y, z );
      imodPointAppendXYZ( xcont, x, y+1, z );
      break;
    }
			
	case(DM_WARP):            // draw warp area:
    {
      float warpRadius = plug.warpRadius;
      bool ptInContortRange = false;
      
      if( !plug.but2Down && !plug.but3Down )    // if mouse not down: check if cont close
      {
        if( plug.warpBehavior == WB_AUTO )
        {
          float contortDistTol = 10.0f*sc; //MAX( radius*0.2f, 5.0f );
          ptInContortRange =
            edit_selectNearPtInCurrObj( &plug.mouse, contortDistTol, 0.0f, false );
        }
        else if( plug.warpBehavior == WB_LINE )
        {
          float contortDistTol = warpRadius;
          ptInContortRange =
            edit_selectNearPtInCurrObj( &plug.mouse, contortDistTol, 0.0f, false );
        }
        
        plug.contortInProgress = false;
      }
      
      bool showContort = isCurrPtValid() && plug.warpBehavior != WB_AREA
                          && ( plug.contortInProgress || ptInContortRange );
      
      if( showContort )    // draw contort area (instead of warp circle)
      {
                              // draw thick circle around closest (current) point
        Ipoint *currPt = imodPointGet(imod);
        Icont *xcontCircle = imodContourNew();
        cont_generateCircle( xcontCircle, 4.0f*sc, 100, *currPt, true );
        cont_generateCircle( xcontCircle, 4.5f*sc, 100, *currPt, true );
        imodObjectAddContour(xobj, xcontCircle);
        
                              // draw warp area
        
        Iobj  *obj  = imodObjectGet(imod);
        Icont *cont = imodContourGet(imod);
        int objIdx, contIdx, ptIdx;
        imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
        bool closed = isContClosed(obj,cont);
        float contLength = imodContourLength( cont, closed );
        float distToWarp = MIN( warpRadius, contLength * 0.25f );
        int numPts = psize(cont);
        
        int idxStart, idxEnd;
        float distFromSelPt = 0.0f;
        for( int p=numPts+(ptIdx-1); p>=ptIdx; p-- )
        {
          distFromSelPt += line_distBetweenPts2D( getPt(cont,p), getPt(cont,p-1) );
          if( distFromSelPt >= distToWarp )
          {
            idxStart = p;
            break;
          }
        }
        
        distFromSelPt = 0.0f;
        for( int p=numPts+(ptIdx+1); p<=numPts+numPts+ptIdx; p++ )
        {
          distFromSelPt += line_distBetweenPts2D( getPt(cont,p-1), getPt(cont,p) );
          if( distFromSelPt >= distToWarp )
          {
            idxEnd = p;
            break;
          }
        }
        
        Icont *xcontS = imodContourNew();
        Icont *xcontE = imodContourNew();
        Icont *xcontL = imodContourNew();
        setOpenFlag(xcontL,1);
        for( int p=idxStart; p<=idxEnd; p++ )
          imodPointAppend( xcontL, getPt(cont,p) );
        cont_generateCircle( xcontS, 3.5f*sc, 4, *getPt(cont,idxStart), true );
        cont_generateCircle( xcontE, 3.5f*sc, 4, *getPt(cont,idxEnd), true );
        imodObjectAddContour(xobj, xcontL);
        imodObjectAddContour(xobj, xcontS);
        imodObjectAddContour(xobj, xcontE);
        
                              // draw small lines to hint warp radius
        
        float halfSmallLineLen = MAX( warpRadius*0.05f, 0.5f );
        imodPointAppendXYZ( xcont2, x-warpRadius, y+halfSmallLineLen, z );
        imodPointAppendXYZ( xcont2, x-warpRadius, y-halfSmallLineLen, z );
        imodPointAppendXYZ( xcont3, x+warpRadius, y+halfSmallLineLen, z );
        imodPointAppendXYZ( xcont3, x+warpRadius, y-halfSmallLineLen, z );
      }
      else        // draw warp circle
      {
        cont_generateCircle( xcont, warpRadius, 100, plug.mouse, true );
        setInterpolated( xcont, true );
        break;
      }
      
      break;
    }
			
  case(DM_SCULPT):            // draw sculpt circle:
    {
      cont_generateCircle( xcont, radius, 100, plug.mouse, true );
      
      if( plug.scupltBut3Warp && plug.but3Down )
        setInterpolated( xcont, 1 );
      
      if( plug.shiftDown )      // draw pinch lines:
      {
        imodPointAppendXYZ( xcont2, x+hRadius, y+qRadius, z );
        imodPointAppendXYZ( xcont2, x+qRadius, y, z );
        imodPointAppendXYZ( xcont2, x+hRadius, y-qRadius, z );
        
        imodPointAppendXYZ( xcont3, x-hRadius, y-qRadius, z );
        imodPointAppendXYZ( xcont3, x-qRadius, y, z );
        imodPointAppendXYZ( xcont3, x-hRadius, y+qRadius, z );
      }
      
      break;
    }
			
  case(DM_JOIN):              // draw sculpt circle with plus sign in middle:
    {
      if( !plug.but3Down )
      {
        cont_generateCircle( xcont, radius, 100, plug.mouse, true );
        
        imodPointAppendXYZ( xcont2, x+hRadius,   y,      z );
        imodPointAppendXYZ( xcont2, x-hRadius,   y,      z );
        imodPointAppendXYZ( xcont3, x,           y+hRadius,  z );
        imodPointAppendXYZ( xcont3, x,           y-hRadius,  z );
      }
      else                      // draw line and rectangle from where user clicked down:
      {
        float xS = plug.mouseDownPt.x;
        float yS = plug.mouseDownPt.y;
        
        imodPointAppendXYZ( xcont2, xS, yS, z );
        imodPointAppendXYZ( xcont2, x,  y,  z );
        
        float xDiff = xS - x;
        float yDiff = yS - y;
        float dist  = sqrt( (xDiff*xDiff) + (yDiff*yDiff) );
        float sideScale = fDiv( radius , dist );
        
        imodPointAppendXYZ( xcont, x +(yDiff*sideScale), y -(xDiff*sideScale), z );
        imodPointAppendXYZ( xcont, x -(yDiff*sideScale), y +(xDiff*sideScale), z );
        imodPointAppendXYZ( xcont, xS-(yDiff*sideScale), yS+(xDiff*sideScale), z );
        imodPointAppendXYZ( xcont, xS+(yDiff*sideScale), yS-(xDiff*sideScale), z );
      }
      
      break;
    }
	
	case(DM_LIVEWIRE):         // draw a livewire line from selected point
    {
			drawExtraObjectLivewire(false);
      break;
    }
	
	case(DM_WAND):         // draw a livewire line from selected point
    {
			drawExtraObjectWand(false);
      break;
    }
	
  case(DM_TRANSFORM):         // draw rectangle around current contour or next to mouse 
    {
      Icont *cont = imodContourGet(imod);
      if( isContValid(cont) )
      {
        Ipoint ll, ur;
        imodContourGetBBox( cont, &ll, &ur);
        cont_generateBox( xcont, ll, ur, false );
        
        if( (!plug.shiftDown && plug.but3Down) || (plug.shiftDown && plug.but2Down) )
        {                          // draw crosshair and vertical line from clicked:
          imodPointAppendXYZ( xcont2, plug.centerPt.x,      plug.centerPt.y, z );
          imodPointAppendXYZ( xcont2, plug.centerPt.x+4*sc, plug.centerPt.y, z );
          imodPointAppendXYZ( xcont2, plug.centerPt.x-4*sc, plug.centerPt.y, z );
          imodPointAppendXYZ( xcont2, plug.centerPt.x,      plug.centerPt.y, z );
          imodPointAppendXYZ( xcont2, plug.centerPt.x, plug.centerPt.y+4*sc, z );
          imodPointAppendXYZ( xcont2, plug.centerPt.x, plug.centerPt.y-4*sc, z );
          
          imodPointAppendXYZ( xcont3, plug.mouseDownPt.x, plug.mouseDownPt.y, z );
          imodPointAppendXYZ( xcont3, plug.mouseDownPt.x, y, z );
          if(!plug.transformBut3Unif && plug.but3Down)
            imodPointAppendXYZ( xcont3, x, y, z );
        }
        else if(plug.but2Down)      // draw line from point clicked to mouse:
        {
          imodPointAppendXYZ( xcont2, plug.mouseDownPt.x, plug.mouseDownPt.y, z );
          imodPointAppendXYZ( xcont2, x, y, z );
        }
        else if(plug.but3Down)    // draw line from center of contour to mouse:
        {
          imodPointAppendXYZ( xcont2, plug.centerPt.x, plug.centerPt.y, z );
          imodPointAppendXYZ( xcont2, x, y, z );
        }
      }
      else
      {
        float rectLen = 10.0 * sc;
        cont_generateBox( xcont, x+rectLen, y+rectLen, rectLen, 0.5*rectLen, z );
      }
      
      break;
    }
	
  case(DM_ERASER):            // draw sculpt circle with a diagonal line through it:
    {
      if( plug.but2Down || plug.but3Down )  {
        cont_generateCircle( xcont, radius*0.99f, 100, plug.mouse, true );
        cont_generateCircle( xcont, radius*0.98f, 100, plug.mouse, true );
      }
      cont_generateCircle( xcont, radius, 100, plug.mouse, true );
      imodPointAppendXYZ( xcont2, x+hRadius, y+hRadius, z );
      imodPointAppendXYZ( xcont2, x-hRadius, y-hRadius, z );
      
      if( plug.shiftDown )              // draw extra arrows on diagonal line:
      {
        imodPointAppendXYZ( xcont3, x+hRadius, y+qRadius, z );
        imodPointAppendXYZ( xcont3, x+hRadius, y+hRadius, z );
        imodPointAppendXYZ( xcont3, x-hRadius, y-hRadius, z );
        imodPointAppendXYZ( xcont3, x-hRadius, y-qRadius, z );
      }
      break;
    }
	
  case (DM_CURVE):            // draw curve
    {
      Icont *cont = imodContourGet(imod);
      int objIdx, contIdx, ptIdx;
      imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
      
      if( isContValid(cont) )
      {
        float distFromFirstPt = imodPointDistance( getPt(cont,0), &plug.mouse );
        bool readyToCloseCont = (psize(cont) > 3) && (distFromFirstPt < 10.0);
        
        cont_copyPts( cont, xcont2, false );
        if( !readyToCloseCont )
          imodPointAdd( xcont2, &plug.mouse, ptIdx+1 );
        cont_addPtsSmooth( xcont2, plug.smoothMinDist,
                           plug.smoothTensileFract, (readyToCloseCont), false, false );
        
        if( readyToCloseCont )
        {
          cont_generateCircle( xcont, 4.0f*sc, 100, *getFirstPt(cont), true );
          cont_generateCircle( xcont, 4.5f*sc, 100, *getFirstPt(cont), true );
        }
      }
      
      break;
    }
			
	case (DM_CIRCLE):            // draw circle
    {
			if( plug.but2Down && !plug.but2Released )
			{
				float distDragged = line_distBetweenPts2D( &plug.mouseDownPt, &plug.mouse );
				Ipoint centerPt   = line_getPtHalfwayBetween( &plug.mouseDownPt, &plug.mouse );
				cont_generateCircle( xcont, distDragged / 2.0f, 120, centerPt, false );
      }
			else if(plug.but3Down)    // draw line from center of contour to mouse:
			{
				imodPointAppendXYZ( xcont2, plug.centerPt.x, plug.centerPt.y, z );
				imodPointAppendXYZ( xcont2, x, y, z );
			}
      break;
    }
		
	case (DM_CORRECT):            // draw little correct circle:
    {
			Icont *cont = imodContourGet(imod);
			float correctCircleRadius = MAX(radius / 5.0f, 5.0f);
			float distMouseCurrPt = (cont==NULL) ? FLOAT_MAX
			                      : cont_minDistPtAndContourPts2D( &plug.mouse, cont, true );
			bool isMouseInRange = distMouseCurrPt < (correctCircleRadius*2.0f);
			bool drawLittleCorrectCircle = !plug.but2Down && isMouseInRange;
			
			Icont *xcont  = imodContourNew();
			cont_generateCircle( xcont, correctCircleRadius, 50,
													(plug.but2Down) ? plug.mouseDownPt : plug.mouse, true );
			imodObjectAddContour(xobj, xcont);
			free(xcont);
			
			if( drawLittleCorrectCircle )		// show correction which would be applied
			{
				 Icont *xcont2  = imodContourDup( cont );
				 int ptsAfterCorrection = edit_contCorrectCircle( xcont2, plug.mouse,
																													correctCircleRadius, 10,
				 plug.mouseDownPt );
				 if( ptsAfterCorrection > 10 )
				 imodObjectAddContour(xobj, xcont2);
				 free(xcont2);
			}
      
      break;
    }
			
	case (DM_MEASURE):            // measure distance
    {
      float fontSize = 12 * sc;
      
      float pixelSize = imodGetPixelSize(imod);
      char *unitsChs = imodUnits(imod);
      Ipoint scalePt;        setPt( &scalePt, 1,1, imodGetZScale(imod) );
      float dist3D = imodPoint3DScaleDistance( &plug.mouseDownPt, &plug.mouse, &scalePt );
      float unitDist = dist3D * pixelSize;      
      
      if( plug.but2Down )
      {
        float xS = plug.mouseDownPt.x;
        float yS = plug.mouseDownPt.y;
        
        int slicesSpanned = ABS( plug.mouseDownPt.z - z );
        
        float xDiff = xS - x;
        float yDiff = yS - y;
        float dist  = sqrt( (xDiff*xDiff) + (yDiff*yDiff) );
        float sideScale = fDiv( 1.0f , dist );
        
        imodPointAppendXYZ( xcont, x +(yDiff*sideScale), y -(xDiff*sideScale), z );
        imodPointAppendXYZ( xcont, x -(yDiff*sideScale), y +(xDiff*sideScale), z );
        imodPointAppendXYZ( xcont, xS-(yDiff*sideScale), yS+(xDiff*sideScale), z );
        imodPointAppendXYZ( xcont, xS+(yDiff*sideScale), yS-(xDiff*sideScale), z );
        
        if( slicesSpanned )
        {
          setInterpolated( xcont, 1 );
        }
        else
        {
          imodPointAppendXYZ( xcont2, xS, yS, z );
          imodPointAppendXYZ( xcont2, x,  y,  z );
          imodPointAppendXYZ( xcont2, xS, yS, z );
          imodPointAppendXYZ( xcont2, x,  y,  z );
          
          float halfSi = sideScale / 2.0;
          imodPointAppendXYZ( xcont, x +(yDiff*halfSi), y -(xDiff*halfSi), z );
          imodPointAppendXYZ( xcont, x -(yDiff*halfSi), y +(xDiff*halfSi), z );
          imodPointAppendXYZ( xcont, xS-(yDiff*halfSi), yS+(xDiff*halfSi), z );
          imodPointAppendXYZ( xcont, xS+(yDiff*halfSi), yS-(xDiff*halfSi), z );
        }
        
        Ipoint textPos = line_getPtHalfwayBetween( &plug.mouseDownPt, &plug.mouse );
        textPos.y -= fontSize / 2.0f;
        textPos.z = z;
        if( ABS(yDiff) < ABS(xDiff) )
          textPos.y += (yDiff*xDiff>0) ? -3*fontSize : 2*fontSize;
        else
          textPos.x += fontSize;
        
        //string text = toString( unitDist ) + " " + toString( unitsChs )
        //              + " (" + toString( dist3D ) + " pixels)";
        //cont_generateTextAsConts( xobj, text, textPos, fontSize, TA_LEFT, true );
        
        string text = toString( unitDist ) + " " + toString( unitsChs )
                        + "\n (=" + toString( dist3D ) + " pixels)";
        
        if( slicesSpanned )
          text += "\n spanning " + toString( slicesSpanned ) + " slices";
        
        cont_generateTextAreaAsConts( xobjT, text, textPos, fontSize, TA_LEFT, TV_CENTER,
                                      true, 4 );
      }
      else if( isCurrPtValid() && imodPointGet(imod)->z == z )
      {
        Ipoint *pt   = imodPointGet(imod);
        Icont  *cont = imodContourGet(imod);
                
        float lineLenHoriz = 15*sc;
        float lineLenVert  = 15*sc;
        
        //float distOnRightInSc = (plug.xsize - pt->x) / sc;
        int textAlign = (  pt->x < 0.7*plug.xsize ) ? TA_LEFT : TA_RIGHT;
        if( textAlign == TA_RIGHT )
          lineLenHoriz = -lineLenHoriz;
        
        Ipoint textPos; setPt(&textPos, pt->x+lineLenHoriz, pt->y+lineLenVert, z);         
        cont_addTwoPointContourToObj( xobj, pt->x+2*sc, pt->y+2*sc, 
                                      pt->x+lineLenHoriz, pt->y+lineLenVert, z );

        
        char *objName    = imodObjectGetName( imodObjectGet(imod) ); 
        string text = toString( objName );
        if( text.length() == 0 )
        {
          text = "WARNING: this object is unlabelled";
          cont_generateTextAsConts( xobjT, text, textPos, fontSize, textAlign, true );
        }
        
        cont_generateTextAsConts( xobj, text, textPos, fontSize, textAlign, true );
        
        if( imodPointDistance( pt, &plug.mouse ) < 30.0 )
        {
          textPos.y -= (fontSize + 15)*sc;
          bool closed = isContClosed( getCurrObj(), cont);
          float lengthClosed = imodContourLength( cont, closed ) * pixelSize;
          string extraText = (closed) ? "CLOSED LENGTH: " : "OPEN LENGTH: ";
          extraText += toString(lengthClosed)+ " " + toString( unitsChs );
          cont_generateTextAsConts( xobjT, extraText, textPos, fontSize, textAlign, true );
        }
        
        /*
        //## PRINT ANY LABEL ATTACHED TO THE CONTOUR
        //## WON'T YET WORK AS NO WAY TO ACCESS LABEL'S TITLE?! : 
        //## SEE: file:///Users/a.noske/Documents/MACMOD/html/libhelp/ilabel.html
        Ilabel *label = imodContourGetLabel( imodContourGet(imod) );
        if(label && imodLabelItemGet(label,0)!=NULL )
        {
          char *labelText = imodLabelItemGet( label, 0 );
          string extraText = "CONTOUR LABEL: " + toString(labelText);
          textPos.y -= (fontSize + 4 + 15)*sc;
          cont_generateTextAsConts( xobj, extraText, textPos, fontSize, textAlign, true );
        } */       
      }
      
      break;
    }
  }
  
  imodObjectAddContour(xobj, xcont);
  imodObjectAddContour(xobj, xcont2);
  imodObjectAddContour(xobj, xcont3);
  free(xcont);
  free(xcont2);
  free(xcont3);
  
  if( redraw )
    ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
  
  return true;
}





//------------------------
//-- Accesses some of the extra object and draw an active livewire line
//-- as a thick yellow contour extendging from the the mouse to the 
//-- last livewire point. Livewire points are drawing as red squares
//-- and a black box is drawn near the mouse to show progress of
//-- "plug.weights" and the active livewire thread(s) "plug.livewire"
//-- and/or "plug.livewireF".

bool DrawingTools::drawExtraObjectLivewire( bool redraw )
{
  if ( !plug.window )
    return false;
  
  //## CLEAR EXTRA OBJECT:
  
  Iobj *xobjT = ivwGetAnExtraObject(plug.view, plug.extraObjText);
	Iobj *xobjL = ivwGetAnExtraObject(plug.view, plug.extraObjLW);
	Iobj *xobjP = ivwGetAnExtraObject(plug.view, plug.extraObjLWPts);
	
  if ( !xobjT || !xobjL || !xobjP )
    return false;
	
  imodObjectSetValue(xobjT, IobjFlagExtraInModv, (plug.showMouseInModelView)?1:0);
	imodObjectSetValue(xobjL, IobjFlagExtraInModv, (plug.showMouseInModelView)?1:0);
	imodObjectSetValue(xobjP, IobjFlagExtraInModv, (plug.showMouseInModelView)?1:0);
	
	if( plug.drawMode != DM_LIVEWIRE )
		return false;
	
	
  //## GET Z VALUE:
  
  Imod *imod  = ivwGetModel(plug.view);
	Icont *cont = imodContourGet(imod);
	Ipoint *pt  = imodPointGet(imod);
  int ix, iy,iz;
  ivwGetLocation(plug.view, &ix, &iy, &iz);
  plug.mouse.z = iz;
  
  float x = plug.mouse.x;
  float y = plug.mouse.y;
  float z = plug.mouse.z;
  
  float zapZoom = 1.0f;                 // gets the zoom of the top-most zap window
  int noZap = ivwGetTopZapZoom(plug.view, &zapZoom); 
  float sc = fDiv( 1.0f, zapZoom);			// tomogram distance for one screen pixel 
	
	
	//## DRAW LIVEWIRE FROM SELECTED POINT:
	
	uint w = plug.xsize, h = plug.ysize;
	int mX = (int)plug.mouse.x;		keepWithinRange( mX, 2, (int)w-1);
	int mY = (int)plug.mouse.y;		keepWithinRange( mY, 2, (int)h-1);
	int mZ = (int)plug.mouse.z;
	
	bool addLivewireSegment = (pt != NULL) && ( (int)imodPointGet(imod)->z == mZ);
	
	if( addLivewireSegment )
	{
		//## WORK OUT WHAT LINES TO DRAW:
		
		int numLivewirePts      = plug.lwPts==NULL ? 0 : psize(plug.lwPts);
		bool finishContour      = false;
		if( numLivewirePts >= 3 )
		{
			float distFirstLWPt = line_distBetweenPts2D(getFirstPt(plug.lwPts),&plug.mouse);
			float distLastLWPt  = line_distBetweenPts2D(getLastPt(plug.lwPts), &plug.mouse);
			if( distFirstLWPt <= LW_SNAP_DIST || distLastLWPt <= LW_SNAP_DIST )
				finishContour = true;
		}
		QPoint qptEnd = QPoint( mX, mY );
		
		int objIdx, contIdx, ptIdx;
		imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
		bool lastPtSelected     = (ptIdx >= psize(cont)-1);
		
		
		//## GENERATE CONTOUR FROM LIVEWIRE POINTS:
		
		if( plug.livewire!=NULL && (numLivewirePts > 1 || !lastPtSelected) )
		{
			Icont *xcontL  = imodContourNew();
			setOpenFlag(xcontL, 1);
			edit_addLivewirePtsToCont( xcontL, -1, plug.livewire, qptEnd, mZ );
			imodObjectAddContour(xobjL, xcontL);
			free(xcontL);
		}
		
		if( plug.livewireF!=NULL && lastPtSelected )
		{
			bool showSolid = numLivewirePts==1 || finishContour;
			Icont *xcontL  = imodContourNew();
			setOpenFlag(xcontL, 1);
			imodContourSetFlag( xcontL, ICONT_STIPPLED, (showSolid) ? 0 : 1 );
			edit_addLivewirePtsToCont( xcontL, -1, plug.livewireF, qptEnd, mZ );
			imodObjectAddContour(xobjL, xcontL);
			free(xcontL);
		}
		
		if( plug.lwPts!=NULL )
		{
			Icont *xcontP  = imodContourDup( plug.lwPts );
			imodObjectAddContour(xobjP, xcontP);
			free( xcontP );
		}
		
		if( !lastPtSelected )
		{
			float dist = line_distBetweenPts2D( &plug.mouse, pt );
			if( psize(cont) > 10 && dist > 5.0f )
			{
				Ipoint closestPt;
				float closestDist;
				int closestPtIdx;
				cont_findClosestPtInContToGivenPt( &plug.mouse, cont, &closestDist,
																					 &closestPt, &closestPtIdx );
				bool removePts = (closestDist <= LW_SNAP_DIST) && abs(closestPtIdx-ptIdx) >= 3;
				
				if(removePts)
				{
					Icont *xcontP   = imodContourNew();
					imodPointAppendXYZ( xcontP, closestPt.x, closestPt.y, closestPt.z );
					imodObjectAddContour(xobjP, xcontP);
					free( xcontP );
				}
			}
		}
	}
	
	
	//## DRAW PERCENTAGE BAR:
	/*
	if( plug.lwWeightProgress > 0 )
	{
		float barH = 6*sc;				// |-- height and width of progress bar
		float barW = 102*sc;			// |
		float barX = x-(50*sc);		// |-- bottom left corner of progress bar
		float barY = y+(20*sc);		// |
		
		float progW = ((float)plug.lwWeightProgress / 100.f) * barW;
		
		Icont *xcontB  = imodContourNew();
		imodPointAppendXYZ( xcontB, barX,      barY,      z );
		imodPointAppendXYZ( xcontB, barX+barW, barY,      z );
		imodPointAppendXYZ( xcontB, barX+barW, barY+barH, z );
		imodPointAppendXYZ( xcontB, barX,      barY+barH, z );
		imodObjectAddContour(xobjT, xcontB);
		free(xcontB);
		
		Icont *xcontP  = imodContourNew();
		imodPointAppendXYZ( xcontP, barX,       barY+(0.5*barH), z );
		imodPointAppendXYZ( xcontP, barX+progW, barY+(0.5*barH), z );
		imodObjectAddContour(xobjT, xcontB);
		free(xcontP);
	}*/
	
	
	
	
	//## REDRAW (IF NEEDED) AND RETURN TRUE:
	
	if( redraw )
    ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
  
  return true;
}



//------------------------
//-- Accesses some of the extra object to draw the "wand" area, whereby
//-- an area is filled using an adaptive flood fill algorithm, and points
//-- within this area are shown as red square pixels.

bool DrawingTools::drawExtraObjectWand( bool redraw )
{
  if ( !plug.window )
    return false;
  
  //## CLEAR EXTRA OBJECT:
	
  Iobj *xobj  = ivwGetAnExtraObject(plug.view, plug.extraObjNum);
	Iobj *xobjW = ivwGetAnExtraObject(plug.view, plug.extraObjWPts);
	
  if ( !xobj && !xobjW )
    return false;
	
	imodObjectSetValue(xobj,  IobjFlagExtraInModv, 0);
	imodObjectSetValue(xobjW, IobjFlagExtraInModv, 0);
	
	if( plug.drawMode != DM_WAND )
		return false;
	
	
	
  //## GET Z VALUE:
  
  Imod *imod  = ivwGetModel(plug.view);
	Icont *cont = imodContourGet(imod);
  int ix, iy,iz;
  ivwGetLocation(plug.view, &ix, &iy, &iz);
  plug.mouse.z = iz;
  
  float x = plug.mouse.x;
  float y = plug.mouse.y;
  float z = plug.mouse.z;
  
  float zapZoom = 1.0f;                 // gets the zoom of the top-most zap window
  int noZap = ivwGetTopZapZoom(plug.view, &zapZoom); 
  float sc = fDiv( 1.0f, zapZoom);			// tomogram distance for one screen pixel 
	
	float radius = plug.sculptRadius;
	
	
	
	//## IF MOUSE IS NEAR CURRENTLY SELECTED CONTOUR,
	//## DRAW A LITTLE CORRECT CIRCLE:
	
	
	float correctCircleRadius = MAX(radius / 5.0f, 5.0f);
	float distMouseCurrPt = (cont==NULL) ? FLOAT_MAX
	                      : cont_minDistPtAndContourPts2D( &plug.mouse, cont, true );
	bool isMouseInRange = distMouseCurrPt < (correctCircleRadius*2.0f);
	bool drawLittleCorrectCircle = !plug.but2Down && isMouseInRange;
	
	if( drawLittleCorrectCircle )
	{
		Icont *xcont  = imodContourNew();
		cont_generateCircle( xcont, correctCircleRadius, 50,
												(plug.but2Down) ? plug.mouseDownPt : plug.mouse, true );
		imodObjectAddContour(xobj, xcont);
		free(xcont);
		
		
		/*Icont *xcont2  = imodContourDup( cont );
		int ptsAfterCorrection = edit_contCorrectCircle( xcont2, plug.mouse,
																				             correctCircleRadius, 10,
																										 plug.mouseDownPt );
		if( ptsAfterCorrection > 10 )
			imodObjectAddContour(xobj, xcont2);
		free(xcont2);*/
	}
	
	
	
	//## DRAW SCULPT CIRCLE OVER MOUSE TO REPRESENT
	//## AREA OF INFLUENCE / BOUNDING AREA FOR WAND:
	
	Icont *xcont  = imodContourNew();
	cont_generateCircle( xcont, radius, 100,
											(plug.but2Down) ? plug.mouseDownPt : plug.mouse, true );
	if( plug.but2Down || correctCircleRadius )
		setInterpolated( xcont, 1 );
	imodObjectAddContour(xobj, xcont);
	free(xcont);
	
	
	
	//## IF BUTTON DOWN: DRAW WAND AREA
	
	if( plug.but2Down )
	{
		float tolerance = line_distBetweenPts2D( &plug.mouseDownPt, &plug.mouse );
		
		Icont *xcontP  = imodContourNew();    // used to draw points in wand area
		Icont *xcontF  = imodContourNew();		// used to draw contour around area
		
		edit_addWandPtsToCont( xcontP, plug.mouseDownPt, (int)radius,
													 plug.wandAvgGrayVal, tolerance, plug.waDistBias, true );
		edit_scanContShrink( xcontP, (int)radius, 5, true );
		edit_scanContFill(xcontP, xcontF, plug.mouseDownPt, (int)radius, 6 );
		
		
		imodObjectAddContour(xobjW, xcontP);
		imodObjectAddContour(xobj, xcontF);
		free(xcontP);
		free(xcontF);
	}
	
	
	//## REDRAW (IF NEEDED) AND RETURN TRUE:
	
	if( redraw )
    ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
  
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
//-- Used to initialize default values into DrawingToolsData.

void DrawingTools::initValues()
{
  plug.drawMode               = DM_WARP;
  plug.reducePts              = 0;
  plug.reducePtsTol           = 0.05f;
  plug.reducePtsMinArea       = 0.5f;
  plug.reducePtsOpt           = RD_MINAREA;
  plug.smoothMinDist          = 5;
  plug.smoothTensileFract     = 0.5f;
  plug.smoothReduceFirst      = false;
  plug.smoothMoveIts          = 0;
  plug.smoothMoveFract        = 0.25f;
  plug.smoothMoveMinDist      = 0.20f;
  plug.printSmoothResults     = true;
   
  plug.sculptRadius           = 30.0f;
  plug.warpRadius             = 30.0f;
  plug.diffWarpSize           = false;
  plug.sculptResizeScheme     = SR_STAGGERED;
  plug.warpBehavior           = WB_AUTO;
  plug.scupltBut3Warp         = false;
  plug.transformBut3Unif      = true;
  plug.lineDisplayWidth       = 1;
  
  plug.wheelBehav             = WH_SCULPTCIRCLE;
  plug.dKeyBehav              = DK_TOEND;
  plug.pgUpDownInc            = 1;
  plug.useNumKeys             = false;
  plug.smartPtResizeMode      = false;
  plug.markTouchedContsAsKey  = false;
  plug.wheelResistance        = 100;
  plug.showMouseInModelView   = false;    // this one we don't store
  plug.testIntersetAllObjs    = true;
  plug.selectedAction         = 0;
  plug.sortCriteria           = SORT_NUMPTS;
  plug.findCriteria           = SORT_NUMPTS;
  plug.minObjsNameWarning     = 5;
  plug.drawZhint              = 0;
  plug.useArrowKeys           = true;
  
	plug.modeOrder[0]           = DM_NORMAL;					// 1st slot
	plug.modeOrder[1]           = DM_WARP;						// 2
	plug.modeOrder[2]           = DM_SCULPT;					// 3
	plug.modeOrder[3]           = DM_JOIN;						// 4
	plug.modeOrder[4]           = DM_LIVEWIRE;				// 5
	plug.modeOrder[5]           = DM_WAND;						// 6
	plug.modeOrder[6]           = DM_ERASER;					// 7
	plug.modeOrder[7]           = DM_MEASURE;					// 8
	plug.modeOrder[8]           = DM_NORMAL;					// 9 (defaults to empty)
	
	plug.lwOpt									= LW_DARK_MEMBRANE;
	plug.lwSmooth								= false;
	plug.lwSmoothIts						= 8;
	plug.lwUseWrap							= true;
	
	plug.lwAreaSize				      = 3;
	plug.lwBinning				      = 1;
	plug.lwNoiseRed				      = 0;
	plug.lwColor                = 0;
	plug.lwDontShowAgain				= false;
	
	plug.waSmooth								= true;
	plug.waSmoothIts						= 8;
	plug.waDistBias             = 0.2f;
	plug.waDontShowAgain				= false;
	
	plug.lwInit									= false;
	plug.lwWeightZVal						= -1;               
	
	plug.wandAvgGrayVal         = 0;
  plug.sortCriteriaOfVals     = -1;
}

//------------------------
//-- Loads most of the settings for DrawingTools from user preferences

void DrawingTools::loadSettings()
{
  
  double savedValues[NUM_SAVED_VALS];
  
  int nvals = prefGetGenericSettings("DrawingTools", savedValues, NUM_SAVED_VALS);
  
  if(nvals!=NUM_SAVED_VALS )
  {
    wprint("DrawingTools: Could not load saved values");
    QMessageBox::about( this, "-- Documentation --",
                        "If this is your first time using 'Drawing Tools' \n"
                        "we HIGHLY recommended you click 'Help' \n"
                        "(at bottom of the plugin) and watch the video tutorial! \n\n"
                        "                                   -- Andrew Noske" );
    return;
  }
  
  plug.drawMode                   = savedValues[0];
  plug.reducePts                  = savedValues[1];
  plug.reducePtsTol               = savedValues[2];
  plug.reducePtsMinArea           = savedValues[3];
  plug.reducePtsOpt               = savedValues[4];
  plug.smoothMinDist              = savedValues[5];
  plug.smoothTensileFract         = savedValues[6];
  plug.smoothReduceFirst          = savedValues[7];
  plug.smoothMoveIts              = savedValues[8];
  plug.smoothMoveFract            = savedValues[9];
  plug.smoothMoveMinDist          = savedValues[10];
  plug.printSmoothResults         = savedValues[11];
  plug.sculptRadius               = savedValues[12];
  plug.sculptResizeScheme         = savedValues[13];
  plug.diffWarpSize               = savedValues[14];
  plug.warpBehavior               = savedValues[15];
  plug.scupltBut3Warp             = savedValues[16];
  plug.transformBut3Unif          = savedValues[17];
  plug.lineDisplayWidth           = savedValues[18];
  
  plug.wheelBehav                 = savedValues[19];
  plug.dKeyBehav                  = savedValues[20];
  plug.pgUpDownInc                = savedValues[21];
  plug.useNumKeys                 = savedValues[22];
  plug.smartPtResizeMode          = savedValues[23];
  plug.markTouchedContsAsKey      = savedValues[24];
  plug.wheelResistance            = savedValues[25];
	plug.showMouseInModelView       = false;								//savedValues[26];
  plug.testIntersetAllObjs        = savedValues[27];
  plug.selectedAction             = savedValues[28];
  plug.sortCriteria               = savedValues[29];
  plug.findCriteria               = savedValues[30];
  plug.minObjsNameWarning         = savedValues[31];
  plug.drawZhint                  = savedValues[32];
  plug.useArrowKeys               = savedValues[33];
	
	plug.modeOrder[0]               = savedValues[34];
	plug.modeOrder[1]               = savedValues[35];
	plug.modeOrder[2]               = savedValues[36];
	plug.modeOrder[3]               = savedValues[37];
	plug.modeOrder[4]               = savedValues[38];
	plug.modeOrder[5]               = savedValues[39];
	plug.modeOrder[6]               = savedValues[40];
	plug.modeOrder[7]               = savedValues[41];
	plug.modeOrder[8]               = savedValues[42];
	
	plug.lwOpt                      = savedValues[43];
	plug.lwSmooth                   = savedValues[44];
	plug.lwSmoothIts                = savedValues[45];
	plug.lwUseWrap                  = savedValues[46];
	plug.lwAreaSize                 = savedValues[47];
	plug.lwBinning                  = savedValues[48];
	plug.lwNoiseRed                 = savedValues[49];
	plug.lwColor                    = savedValues[50];
	plug.lwDontShowAgain            = savedValues[51];
	
	plug.waSmooth                   = savedValues[52];
	plug.waSmoothIts                = savedValues[53];
	plug.waDistBias                 = savedValues[54];
	plug.waDontShowAgain            = savedValues[55];
}


//------------------------
//-- Saves most of the settings within DrawingToolsData in user preferences
//-- so they will load next time the plugin is started

void DrawingTools::saveSettings()
{
  double saveValues[NUM_SAVED_VALS];
  
  saveValues[0]   = plug.drawMode;
  saveValues[1]   = plug.reducePts;
  saveValues[2]   = plug.reducePtsTol;
  saveValues[3]   = plug.reducePtsMinArea;
  saveValues[4]   = plug.reducePtsOpt;
  saveValues[5]   = plug.smoothMinDist;
  saveValues[6]   = plug.smoothTensileFract;
  saveValues[7]   = plug.smoothReduceFirst;
  saveValues[8]   = plug.smoothMoveIts;
  saveValues[9]   = plug.smoothMoveFract;
  saveValues[10]  = plug.smoothMoveMinDist;
  saveValues[11]  = plug.printSmoothResults;
  saveValues[12]  = plug.sculptRadius;
  saveValues[13]  = plug.sculptResizeScheme; 
  saveValues[14]  = plug.diffWarpSize;
  saveValues[15]  = plug.warpBehavior;
  saveValues[16]  = plug.scupltBut3Warp;
  saveValues[17]  = plug.transformBut3Unif;
  saveValues[18]  = plug.lineDisplayWidth;
  
  saveValues[19]  = plug.wheelBehav;
  saveValues[20]  = plug.dKeyBehav;
  saveValues[21]  = plug.pgUpDownInc;
  saveValues[22]  = plug.useNumKeys;
  saveValues[23]  = plug.smartPtResizeMode;
  saveValues[24]  = plug.markTouchedContsAsKey;
  saveValues[25]  = plug.wheelResistance;
	saveValues[26]  = false;												// plug.showMouseInModelView;
  saveValues[27]  = plug.testIntersetAllObjs;
  saveValues[28]  = plug.selectedAction;
  saveValues[29]  = plug.sortCriteria;
  saveValues[30]  = plug.findCriteria;
  saveValues[31]  = plug.minObjsNameWarning;
  saveValues[32]  = plug.drawZhint;
  saveValues[33]  = plug.useArrowKeys;
  
	saveValues[34]  = plug.modeOrder[0];
	saveValues[35]  = plug.modeOrder[1];
	saveValues[36]  = plug.modeOrder[2];
	saveValues[37]  = plug.modeOrder[3];
	saveValues[38]  = plug.modeOrder[4];
	saveValues[39]  = plug.modeOrder[5];
	saveValues[40]  = plug.modeOrder[6];
	saveValues[41]  = plug.modeOrder[7];
	saveValues[42]  = plug.modeOrder[8];
	
	saveValues[43]  = plug.lwOpt;
	saveValues[44]  = plug.lwSmooth;
	saveValues[45]  = plug.lwSmoothIts;
	saveValues[46]  = plug.lwUseWrap;
	saveValues[47]  = plug.lwAreaSize;
	saveValues[48]  = plug.lwBinning;
	saveValues[49]  = plug.lwNoiseRed;
	saveValues[50]  = plug.lwColor;
	saveValues[51]  = plug.lwDontShowAgain;
	
	saveValues[52]  = plug.waSmooth;
	saveValues[53]  = plug.waSmoothIts;
	saveValues[54]  = plug.waDistBias;
	saveValues[55]  = plug.waDontShowAgain;
	
  prefSaveGenericSettings("DrawingTools",NUM_SAVED_VALS,saveValues);
}



//------------------------
//-- Change to flag to keep on top or run timer as for info window

void DrawingTools::keepOnTop(bool state)
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
//-- Reduces the number of points in the current contour.

void DrawingTools::reduceCurrentContour()
{  
  if( !isCurrContValid() )
    return;
  
  undoContourDataChgCC( plug.view );        // REGISTER UNDO
  
  int pointsRemoved = edit_reduceCurrContour();
  
  if(pointsRemoved)
  {
    undoFinishUnit( plug.view );            // FINISH UNDO
    ivwRedraw( plug.view );
  }
  if( plug.printSmoothResults )
    wprint("%d points deleted (contour reduction)\n", pointsRemoved);
}

//------------------------
//-- Smooths the current contour by adding extra points using a spline
//-- (existing points are not moved).

void DrawingTools::smoothCurrentContour( bool moveExistingPtsOnce )
{
  if( !isCurrContValid() )
    return;
  
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  bool closed = isContClosed(obj,cont);
  
  int ptsAdded   = 0;
  int ptsDeleted = 0;
  int ptsMoved   = 0;
  
  undoContourDataChgCC( plug.view );      // REGISTER UNDO
  
  if( plug.smoothReduceFirst )
    ptsDeleted = edit_reduceCurrContour();
  
  ptsAdded = edit_smoothCurrContour();
  
  int numMoveIts = ( moveExistingPtsOnce ) ? 1 : plug.smoothMoveIts;
  
  for(int i=0; i<numMoveIts; i++)
    ptsMoved += cont_avgPtsPos( cont, plug.smoothMoveFract,
                                plug.smoothMoveMinDist, closed, true );
  
  if( ptsAdded || ptsDeleted || ptsMoved )
  {
    undoFinishUnit( plug.view );            // FINISH UNDO
    ivwRedraw( plug.view );
  }
  
  if( plug.printSmoothResults )
    wprint("%d pts added %d moved (contour smoothing)\n", ptsAdded-ptsDeleted, ptsMoved);
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
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int includeCType = 0;
  int contMin = 1;
  int contMax = nConts;
  
  string msg = ( plug.reducePtsOpt == RD_TOL ) ?
    "-----"
    "\ntolerance  = " + toString(plug.reducePtsTol) +
    "\n-----"
    "\nWARNING: reducing contours with "
    "\n a large 'tolerance' can result "
    "\n in an undesirable loss of "
    "\n information/contour detail."
    :
    "-----"
    "\nmin area  = " + toString(plug.reducePtsMinArea) + " pix sq."
    "\n-----"
    "\nWARNING: reducing contours with "
    "\n a large 'min area' can result "
    "\n in an undesirable loss of "
    "\n information/contour detail.";
  
  QString toolStr = ( plug.reducePtsOpt == RD_TOL ) ?
    "The 'tolerance' value represents how thorough the smoothing "
    "\nalgorithm will be while still trying to preserve the overall shape. "
    "\nA value of >1.0 is not recommended for reducing large numbers of contours"
    :
    "The 'min area' value represents the minimum area which must be between  "
    "\nany 3 consequtive points, else the middle point be deleted. "
    "\nA value of >5.0 is not recommended for reducing large numbers of contours";
  
	CustomDialog ds("Reduce Contours",this);
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour (inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addComboBox( "include:",
                  "all contours|"
                  "only key contours|"
                  "only interpolated",
                  &includeCType );
  ds.addLabel   ( msg.c_str(), false, toolStr );
	ds.exec();
	if( ds.wasCancelled() )
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
    
    if( plug.reducePtsOpt == RD_TOL )
    {
      pointsDeleted = cont_reducePtsTol( cont, plug.reducePtsTol );
    }
    else
    {
      pointsDeleted = cont_reducePtsMinArea( cont, plug.reducePtsMinArea,
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
  int percentChanged   = calcPercentInt( totalContsChanged, totalContsInspected );
  int percentReduction = 100 - calcPercentInt( totalPointsAfter, totalPointsBefore );
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
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  static int  includeCType     = 0;
  static bool roundZOpenPts    = true;
  static bool addPtEveryZ      = true;
  static bool movePts          = false;
  static float moveFract       = plug.smoothMoveFract;
  static float minDistToMove   = plug.smoothMoveMinDist;
  
  string msg =
    "-----"
    "\nsmooth tensile fract  = " + toString(plug.smoothTensileFract) +
    "\nsmooth point distance = " + toString(plug.smoothMinDist) +
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
  
  CustomDialog ds("Smooth Contours",this);
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour (inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addComboBox( "include:",
                  "all contours|"
                  "only key contours|"
                  "only interpolated",
                  &includeCType );
	ds.addCheckBox( "round Z values (for open contours)", &roundZOpenPts );
  ds.addCheckBox( "add pt every Z (for open contours)", &addPtEveryZ );
  ds.addLabel   ( msg.c_str(), false, toolStr );
  ds.addLabel   ( "----" );
  ds.addCheckBox( "ALLOW POINTS TO BE MOVED.... using:", &movePts, 
                  "NOT TICKED: additional points will be added \n"
                  "   along the curve but no existing points moved \n"
                  "TICKED: points will be added and existing \n"
                  "   points moved to make a smoother contour." );
  ds.addDblSpinBoxF ( "move fraction:", 0.01, 2.0, &moveFract, 2, 0.01,
                      "Points will be moved towards this percentage distance \n"
                      "from their current location, to the position halfway \n"
                      "between the point before and after (average pos). \n" );
  ds.addDblSpinBoxF ( "min distance to move:", 0.001, 10.0, &minDistToMove, 3, 0.01,
                      "Points closer than this distance to their 'average pos' \n"
                      "will NOT be moved." );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  
  contMin  -= 1;
  contMax  -= 1;
  
  //## SMOOTH ALL CONTOURS WITHING RANGE:
  
  int totalContsInspected = 0;
  int totalContsChanged   = 0;
  int totalPointsAfter    = 0;
  int totalPointsAdded    = 0;
  int totalPointsMoved    = 0;
  
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
      cont_addPtsSmooth( cont, plug.smoothMinDist, plug.smoothTensileFract,
                         isContClosed(obj,cont), roundZOpenPts, addPtEveryZ );
    
    int pointsMoved = 0;
    if(movePts)
    {
      int closed = isContClosed( obj, cont ) ? 1 : 0;
      pointsMoved = cont_avgPtsPos( cont, moveFract, minDistToMove, closed, true );
    }
    
    totalPointsAdded  += pointsAdded;
    totalPointsAfter  += psize(cont);
    totalPointsMoved  += pointsMoved;
    
    totalContsInspected++;
    if( pointsAdded || pointsMoved )
      totalContsChanged++;
  }
  if(totalContsChanged)
    undoFinishUnit( plug.view );
  
  
  //## PRINT RESULT:
  
  int totalPointsBefore = totalPointsAfter - totalPointsAdded;
  int percentChanged   = calcPercentInt( totalContsChanged, totalContsInspected );
  int percentIncrease  = calcPercentInt( totalPointsAfter, totalPointsBefore );
  int percentMoved     = calcPercentInt( totalPointsMoved, totalPointsAfter );
  wprint("SMOOTHING OF CONTOURS:\n");
  wprint(" # contours changed = %d of %d  (%d%%)\n", totalContsChanged,
         totalContsInspected, percentChanged );
  wprint(" # points added \t= %d\n", totalPointsAdded);
  wprint("   ... %d > %d \t= %d%% increase\n",
         totalPointsBefore,totalPointsAfter,percentIncrease);
  if( movePts )
  {
    wprint(" # points moved \t= %d\n", totalPointsMoved);
    wprint("   ... %d of %d \t= %d%% moved\n",
           totalPointsMoved,totalPointsAfter,percentMoved);
  }
  
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
  Iobj *obj  = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  Icont *cont = imodContourGet(imod);
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
    
    case( DK_MOVEPT ):
    {
      movePoint();
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
  Iobj *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int  overlapAction          = 1;
  
	CustomDialog ds("Find Intersecting Contours", this);
  ds.addRadioGrp( "action:",
                  "find next intersecting edge [a]|"
                  "list all intersecting edges|"
                  "delete intersecting contours in current object",
                  &overlapAction,
                  "",
                  "Selects the first non-simple contour or \n"
                  "first contour intersecting another contour \n"
                  "beyond the currently selected contour - \n"
                  "searching all objects|"
                  "Prints out all contours in the model \n"
                  "which have edges intersecting some other contour|"
                  "Deletes any contour in the current object \n"
                  "which overlaps another contour in the \n"
                  "SAME object"
                  );
	ds.addCheckBox( "find edge intersections between different objects",
                  &plug.testIntersetAllObjs,
                  "If true: each contour will be tested for \n"
                  "crossing paths with other contours in all \n"
                  "closed objects, not just the same object. \n"
                  "WARNING: This can make it much slower" );
	ds.addCheckBox( "find nested contours\n"
                  "(from the same object)",
                  &plug.testOverlapping,
                  "If true: will find any contours contained within \n"
                  "(i.e. overlapping, but not necessarily intersecting) \n"
                  "other contours in the same object." );
  ds.addLabel   ( "-----\n"
                  "NOTE:\n"
                  " > Only closed contours are tested for intersection \n"
                  " > A contour 'intersects' another only if their edges \n"
                  "    cross - not if one is completely inside the other\n"
                  " > Testing may take several minutes for large models" );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  
  switch( overlapAction )
  {
    case(0):
    {
      edit_selectNextIntersectingCont();
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
          if( edit_findIntersectingCont( o, c, &pCross, &objCross, &contCross, &samePts,
                                        startObjIdx, endObjIdx, false ) )
          {
            wprint("  obj %d cont %d \tAND   obj %d cont %d @ pt %d%s\n",
                   o+1, c+1, objCross+1, contCross+1, pCross+1, (samePts) ? " (*)" : "");
            numOverlappingConts++;
          }
          
          if( plug.testOverlapping )
          {
            int parents = edit_countOverlappingContsCrude( o, c );
            if( parents )
              wprint("  obj %d cont %d \tFALLS INSIDE   %d other conts\n", 
                     o+1, c+1, parents );
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
        
        if( edit_findIntersectingCont( objIdx, c, &pCross, &objCross, &contCross, &samePts,
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
  Iobj *obj  = imodObjectGet(imod);
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
  
  int percentSinglePt  = calcPercentInt( singlePtConts, nonEmptyConts );
  int percentOpen      = calcPercentInt( openConts, nonEmptyConts );
  int percentClockwise = calcPercentInt( cclockwiseConts, nonEmptyConts );
  int percentStippled  = calcPercentInt( stippledConts, nonEmptyConts );
  
  float ptsPerCont = fDiv( totPts, nonEmptyConts );
  float avgDistPts = fDiv( totLen, totPts - (openConts+singlePtConts)  );
  float avgLen     = fDiv( totLen, nonEmptyConts );
  float avgArea    = fDiv( totArea, nonEmptyConts );
  float avgPtSize  = fDiv( totPtSize, nonEmptyConts );
  
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
  Iobj *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nConts = csize(obj);
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int contMin              = contIdx+1;
  int contMax              = contIdx+1;
  static int  printID      = 1;
  static bool usePixelLen  = true;
  
	CustomDialog ds("Contour Printing", this);
  ds.addLabel   ( "contours to sort (inclusive):" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours after this contour "
                  "(inclusive) will be reordered" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour "
                  "(inclusive) will be reordered" );
  ds.addRadioGrp( "print:",
                  "summary only|"
                  "points|"
                  "segment lengths",
                  &printID,
                  "",
                  "Only prints basic info (area, length etc)|"
                  "Lists the position of all points|"
                  "List the line segment distance for every point"
                  "to the next point"
                  );
	ds.addCheckBox( "show distances in pixels", &usePixelLen,
                  "If true measures all lengths etc in pixels \n"
                  "Else use the units in the model header." );
	ds.exec();
	if( ds.wasCancelled() )
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
        int   segPercent = calcPercentInt( cumulativeLen, length);
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
    
    float ptsPerCont = fDiv( totObjPts, csize(obj) );
    float avgDistPts = fDiv( totObjLen, totObjPts  );    
          // NOTE: not accurate for open contours and single points (no length)
    
    if(emptyConts)
      wprint(" # EMPTY CONTOURS = %d\n", emptyConts );
    wprint(" # conts  = %d\n", totConts );
    wprint(" # pts    = %d\n", totObjPts );
    wprint(" avg pts/cont = %g\n", ptsPerCont );
    wprint(" avg dist between pts = %g\n", avgDistPts );
  }
  
  float ptsPerContAll = fDiv( totPts, totConts );
  float avgDistPtsAll = fDiv( totLen, totPts );
  
  wprint("\n------------\n");
  wprint("OVERALL:\n");
  wprint(" # empty contours = %d\n", totEmpty );
  wprint(" # conts  = %d\n", totConts );
  wprint(" # pts    = %d\n", totPts );
  wprint(" avg pts/cont = %g\n", ptsPerContAll );
  wprint(" avg dist between pts = %g\n", avgDistPtsAll );
}


//------------------------
//-- Prints an summary of the tube length traced using open contours.
//-- If different point sizes are used for each point, information about
//-- tube thickness is printed too.
//-- 
//-- NOTE: This function is almost identical to the function:
//-- "analysis_outputTubeSizeAnalysis" in plugs/analysistools/analysistools.cpp
//-- except that the user is given options to include or omit the radius
//-- of the first and last points in calculating the "full length".

void DrawingTools::analyzeTubes()
{
  Imod *imod  = ivwGetModel(plug.view);
  int nObjs   = osize(imod);
	int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
	
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int minObjNum     = objIdx+1;
	static int maxObjNum     = objIdx+1;
  static bool incFirstRad  = true;
  static bool incLastRad   = true;
  
	static bool outputEachCont = true;
	static bool convertUnits   = true;
	
	
	CustomDialog ds("Tube Analysis", this);
  ds.addLabel   ( "object range:" );
	
	ds.addMinMaxSpinBoxPair ( " > object:", "to", 1, nObjs, &minObjNum, &maxObjNum,1,
								 "The range of objects you wish to analyze" );
	
	ds.addLabel   ( "length calculation:" );
	
	ds.addCheckBox( "include radius of first point", &incFirstRad,
								 "If true will include the radius of the first  \n"
								 "point in the total length." );
	ds.addCheckBox( "include radius of last point", &incLastRad,
								 "If true will include the radius of the last  \n"
								 "point in the total length." );
	
	ds.addLabel   ( "output options:" );
	
	ds.addCheckBox( "output each contour's length", &outputEachCont,
								 "If true, will output the length of each contour... \n"
								 "if false, outputs a summary only." );
	ds.addCheckBox( "output in units", &convertUnits,
								 "If true will output all measurements in the units specified \n"
								 "under 'Edit > Model > Header' (instead of pixels)." );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	
	//## PRINT TUBE SUMMARY
	
	int minObj = MAX( minObjNum-1, 0       );
	int maxObj = MIN( maxObjNum-1, nObjs-1 );
	
	float pixelSize = imodGetPixelSize(imod);
  float zScale    = imodGetZScale(imod);
  float scaleValues = (convertUnits) ? pixelSize : 1.0f;
	string unitsStr   = (convertUnits) ? toString( imodUnits(imod) ) : "pixels";
	QString qEndStrUnits = " " + QString(unitsStr.c_str()) + "\n";
	QString SEP = "\t";			// seperator character
	
	QString outStr = "TUBE SUMMARY\n";
  
  for( int o=minObj; o<=maxObj; o++ )
  {
		float OopenL   = 0.0f;		// tally open length
		float OfullL   = 0.0f;		// tally end-to-end length (includes first and/or last pt)
		float OtotR    = 0.0f;		// tally the total radius as multiplied by open length
    int   OemptyConts = 0;		// tally the number of empty contours
    
    Iobj *obj = getObj(imod,o);
    
		outStr += "\nOBJECT " + QStr(o+1) +"\n";
    
    if( csize(obj)==0 )
    {
      outStr += " ... empty\n";
      continue;
    }
    
		if( isObjClosed(obj) )
		{
      outStr += " WARNING: this object has closed contours\n";
			outStr += "  The analyze tube function should be used on open contours only\n";
		}
		
		if(outputEachCont)
		{
			outStr += "cont#"+SEP+"pts"+SEP+SEP+"openLength"+SEP+"fullLength"+SEP+SEP;
			outStr += "avgRadius"+SEP+"firstR"+SEP+"lastR"+SEP;
			outStr += "minR"+SEP+"maxR"+SEP+"minMidR"+"\n";
		}
		
    for( int c=0; c<csize(obj); c++ )
    {
      Icont *cont   = getCont(obj,c);
      int nPts      = psize(cont);
			
			float openL, fullL, avgR;
			float firstR, lastR, minR, maxR, minMidR, openVol, fullVol, surfaceA;
			
			cont_calcPtsizeInfo( obj, cont, zScale, scaleValues, openL,fullL,
													 avgR,firstR,lastR,minR,maxR,minMidR,
													 openVol,fullVol,surfaceA );
			
			fullL = openL;
			if( incFirstRad )	fullL += firstR;
			if( incLastRad  )	fullL += lastR;
			
			if(outputEachCont)
			{
				outStr += QStr(c+1)   + SEP + QStr(nPts)   + SEP+SEP;
				outStr += QStr(openL) + SEP + QStr(fullL)  + SEP+SEP;
				outStr += QStr(avgR)  + SEP + QStr(firstR) + SEP  + QStr(lastR) + SEP;
				outStr += QStr(minR)  + SEP + QStr(maxR)   + SEP  + QStr(minMidR) +"\n";
			}
			
			OopenL += openL;
      OfullL += fullL;
      OtotR  += (openL * avgR);
			
			if(nPts==0)
				OemptyConts++;
		}
    
		int OnumConts = csize(obj) - OemptyConts;
		
    float OavgOpenL    = fDiv(OopenL, OnumConts);
		float OavgFullL    = fDiv(OfullL, OnumConts);
		float OavgDiameter = fDiv(OtotR, OopenL) * 2.0f;
		
    outStr += "\n\n";
    if(OemptyConts > 0)
      outStr += "# EMPTY CONTOURS = " + QStr(OemptyConts) + "\n";
    outStr += "total   open length  = " + QStr(OopenL)    + qEndStrUnits;
		outStr += "average open length  = " + QStr(OavgOpenL) + qEndStrUnits;
		outStr += "   (where 'open length' does not include any radius values) \n\n";
		
		if( incFirstRad || incLastRad )
		{
			outStr += "total   full length = " + QStr(OfullL)    + qEndStrUnits;
			outStr += "average full length = " + QStr(OavgFullL) + qEndStrUnits;
			outStr += "   (where 'full length' includes the radius of each ";
			if     ( incFirstRad && incLastRad )		outStr += "first and last point)\n\n";
			else if( incFirstRad )									outStr += "first point)\n\n";
			else if( incLastRad  )									outStr += "last point)\n\n";
		}
		
		outStr += "average diameter = " + QStr(OavgDiameter) + qEndStrUnits;
  }
	
	
  //## SHOW OUTPUT IN BIG BOX:
  
	CustomDialog dsR("Tube Analysis Results", this);
  dsR.addLabel    ( "results:" );
	
	dsR.addReadOnlyTextEdit ( outStr, false, 400,
													 "Use copy [ctrl+c] and paste [ctrl]+[v] to paste \n"
													 "this into a spreadsheet or text editor."  );
	
	dsR.setMinimumWidth(500);
	dsR.exec();
	
	if( dsR.wasCancelled() )
		return;
}




//------------------------
//-- Gives a choice of several other options for the user.

void DrawingTools::moreActions()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds("Perform Action", this);
  ds.addRadioGrp( "action:",
                  "clean model and fix contours|"
                  "find intersecting edges [a]|"
                  "sort contours|"
                  "find contours [y]|"
                  "delete contours|"
                  "crop contours|"
                  "copy contours|"
                  "transform contours|"
                  "modify contours|"
                  "move point|"
                  "expand contours|"
								  "round points|"
                  "analyze tubes|"
								  "print basic model info|"
                  "print detailed object info|"
                  "print detailed contour info|"
                  "reset values",
                  &plug.selectedAction,
                  "",
                  "Contains a number of options to clean multiple \n"
                    "objects and 'fix' contours by removing bad; \n"
                    "redundant or out-of-bounds points etc|"
                  "Finds closed contours which cross their own \n"
                    "path or the path of other contours. \n"
                    "In most situations (eg: closed membranes) \n"
                    "no two lines should ever intersect; \n"
                    "especially a line crossing itself; \n"
                    "so it's a good idea to use this tool before \n"
                    "converting your contours into the final mesh model|"
                  "Physically sorts contours using the criteria you select|"
                  "Use [y] to find the contour or point with the \n"
                    "next biggest value based on the criteria you \n"
                    "select|"
                  "Allows you to delete any contours which meet \n"
                    "your specified criteria|"
                  "Will crop and 'cut open' contours in the current \n"
                    "object which go outside the tomogram boundaries \n"
                    "or rubber band area|"
                  "Use this to copy or move a range of contours from \n"
                    "the current object to another object|"
                  "Use this to precisely translate; scale and/or rotate \n"
                    "a range of contours in the current object|"
                  "Allows you to modify contour properties over \n"
                    "a range of contours in the current object|"
                  "Use this to move the current point or the current \n"
                    "contour to a precise position|"
                  "Use this to expand a ring around a range of \n"
                    "open or closed contours within the current object|"
								  "Use this to round all points to an integer and/or multiple of \n"
								    "a decimal value in x, y and/or z|"
								  "Prints an summary of the tube length traced using open \n"
								    "contours. If different point sizes are used for each point \n"
								    "information about tube thickness is printed too.|"
								  "Prints some basic information about the current \n"
                    "object including the average distance between \n"
                    "points; average points contour; and number of \n"
                    "empty contours|"
                  "Prints detailed information about the \n"
                    "current object|"
                  "Prints detailed information about the \n"
                    "current contour; or a range of them|"
                  "Resets all setting (for this plugin) to default" );
	ds.exec();
	if( ds.wasCancelled() )
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
      deleteRangeContours();
      break;
    case(5):      // trim contours
      cropRangeContours();
      break;
    case(6):      // copy or move contours
      copyOrMoveContourRange();
      break;
    case(7):      // transform contours
      tranformContourRange();
      break;
    case(8):      // modify contours
      modifyRangeContours();
      break;
    case(9):      // move point(s)
      movePoint();
      break;
    case(10):      // expand contours
      expandContourRange();
      break;
    case(11):      // round points
      roundPoints();
      break;
		case(12):      // analyze tubes
      analyzeTubes();
      break;
		case(13):      // print basic model info
      printModelPointInfo();
      break;
    case(14):      // print detailed object info
      printObjectDetailedInfo();
      break;
    case(15):      // print detailed contour info
      printContourDetailedInfo();
      break;
    case(16):      // reset values
      if( !MsgBoxYesNo(this, "Restore all default settings for this plugin?" ) )
        return;
      plug.window->initValues();
			plug.window->changeRadioOptions();
      wprint("Default values have been restored\n");
      break;
  }
  
  ivwRedraw( plug.view );
}

//------------------------
//-- Allows user to change other plugin values/settings.

void DrawingTools::keyboardSettings()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds("Mouse and Keyboard Settings", this);
  ds.addLabel   ( "--- MOUSE ---" );
  ds.addComboBox( "wheel behavior:",
                  "none|"
                  "resize sculpt circle|"
                  "scroll slices|"
                  "scroll contours|"
                  "scroll pts|"
                  "resize curr point", &plug.wheelBehav,
                  "The action performed by the mouse wheel" );
  ds.addSpinBox ( "wheel resistance:",
                  10, 1000, &plug.wheelResistance, 10,
                  "The higher the value, the slower "
                  "mouse scrolling works. \n"
                  "RECOMMENDED VALUE: 100" );
  ds.addCheckBox( "smart point resize mode", 
                  &plug.smartPtResizeMode,
                  "If this mode is on, you can use the mouse wheel to resize points \n"
                  "whenever and 'open' or 'scattered' object is selected. \n"
                  "Furthermore, if you are in 'normal' drawing mode new points, \n"
                  "created when button 2 is pressed, will have the same \n"
                  "point size as the previous point in the contour. \n"
                  "\n"
                  "NOTE: This mode can be useful when drawing spheres or tubes of \n"
                  "varying size. This mode has no effect on closed contour objects." );
  ds.addCheckBox( "use button 3 in sculpt to warp", &plug.scupltBut3Warp,
                  "When using sculpt mode, the third mouse button will warp \n"
                  "the contour instead of moving the selected point" );
  ds.addCheckBox( "button 3 scales uniformly", 
                  &plug.transformBut3Unif,
                  "When using the 'Transform' drawing mode with the third \n"
                  "mouse button, contours will scale uniformly in X and Y \n"
                  "else they will scale seperately in X and Y." );
  ds.addCheckBox( "show mouse in model view", 
                  &plug.showMouseInModelView,
                  "Will show the mouse in the Model View "
                  "windows as you move it in the ZAP window. \n"
                  "WARNING: This will reduce performance!");
 
  ds.addLabel   ( "\n--- KEYBOARD ---" );
  ds.addCheckBox( "use number keypad to change mode", 
                  &plug.useNumKeys,
                  "If on: they keypad numbers [1]-[6] are intercepted \n"
                  " and used to change the drawing mode. \n"
                  "If off: the keypad can be used to move points \n"
                  " as per normal... but mode can still be change with \n"
                  " the normal number keys");
  ds.addComboBox( "on [d] remove:",
                  "do nothing|"
                  "pts to end|"
                  "to nearest end|"
                  "current pt|"
                  "current contour|"
                  "pt size current pt|"
                  "pt sizes current cont|"
                  "move pt", &plug.dKeyBehav,
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
  ds.addCheckBox( "use up/down arrows to scroll slices", 
                  &plug.useArrowKeys,
                  "If on: the up and down keys are intercepted and used \n"
                  " to change the current slice. This is a handy option if \n"
                  " you have a laptop without PageUp/PageDown keys"
                  "\n"
                  "WARNING: Having this on may (unfortunately) have side-effects \n"
                  "whereby it does silly things while you're editing text");
  
  ds.addLabel   ( "\n--- SCULPT CIRCLE ---" );
  ds.addCheckBox( "mark contours as key after sculpt", 
                  &plug.markTouchedContsAsKey,
                  "If on: any stippled contour selected and/or "
                  "\nsculpted using the 'sculpt' or 'join' "
                  "\ntool will become unstippled." );
  ds.addCheckBox( "use a different radius for warp circle", 
                  &plug.diffWarpSize,
                  "If off: the size of the warp circle will always be equal"
                  "\nto the size of the sculpt circle." );
  ds.addLineEditF( "sculpt circle radius:",
                   0.01f, 1000.0f, &plug.sculptRadius, 3,
                   "The radius (in pixels) of the circle used "
                   "in sculpt and join drawing mode. \n"
                   "NOTE: Change this using [q] and [w] or (better yet) use the \n"
                   "   mouse wheel (so long as 'mouse behavior' is set correctly).");
  ds.addComboBox( "sculpt resize scheme:",
                  "normal|"
                  "linear|"
                  "log|"
									"fast",
                  &plug.sculptResizeScheme,
                  "The method used to resize the sculpt circle using the mouse wheel \n"
                  "or [q] and [w].\n"
                  "\n"
                  " > normal - staggered approach (default) \n"
                  " > linear - tends to be fast as circle gets small \n"
                  " > log    - tends to be fast when circle is large \n"
								  " > fast   - three times faster than linear \n"
                  "\n"
                  "TIP: Adjust 'wheel resistant' to make the circle reize "
                  "    faster or slower as you scroll the mouse" );
  ds.addComboBox( "warp tool behavior:",
                  "auto|"
                  "contort line|"
                  "warp area",
                  &plug.warpBehavior,
                  "The method used to warp the contour with the warp tool.\n"
                  "\n"
                  " > auto - the warp mode will depend on how close your mouse \n"
                  "    is to the edge of the contour \n"
                  " > contort line - moves points along an portion of the contour \n"
                  "    (as indicated with a red line). This method uses a sine curve \n"
                  "    with the selected point (i.e. nearest to mouse) at the center \n"
                  " > warp area    - moves all points within the warp circle \n"
                  "    (the stippled circle) with the most influence on points \n"
                  "    near the center" );
  
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  if( plug.sculptRadius <= 0.01 )
    plug.sculptRadius == 0.01;
  
  ivwRedraw( plug.view );  
}

//------------------------
//-- Allows user to change other plugin values/settings.

void DrawingTools::moreSettings()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds("More Settings", this);
  
  ds.addLabel   ( "--- CONTOUR REDUCTION [r] ---" );
  ds.addCheckBox( "reduce drawn contours", &plug.reducePts,
                  "Automatically applies smoothing to any contour drawn with the \n"
                  "'sculpt' and 'join' tools upon release of the mouse button" );
  ds.addComboBox( "reduction method:",
                  "segment threshold|"
                  "min area",
                  &plug.reducePtsOpt,
                  "The method used when [r] or 'Reduce Contours' "
                  "is used.\n"
                  "\n"
                  " > segment threshold - better suited "
                  "to preserve the curvature of contour segments \n"
                  " > min area - better suited "
                  "to remove sharp contours of nearby points" );
  ds.addDblSpinBoxF("segment threshold:", 0, 3, &plug.reducePtsTol, 2, 0.01,
                    "When a contour is reduced: the higher this tolerance value \n"
                    "the more points are removed. A value >1 is not advised. \n\n"
                    "RECOMMENDED VALUE: 0.05");
  ds.addDblSpinBoxF("min area:", 0, 3, &plug.reducePtsMinArea, 2, 0.05,
                    "When a contour is reduced: wherever three consecutive points \n"
                    "form a triangular area less than this many pixels squared, \n"
                    "the middle point is removed.\n\n"
                    "RECOMMENDED VALUE: 0.5");
  
  ds.addLabel   ( "\n--- CONTOUR SMOOTHING [e] ---" );
  
  ds.addCheckBox( "reduce contour before smoothing", &plug.smoothReduceFirst,
                  "Will apply 'contour reduction' settings above, before \n"
                  "smoothing the contour. With this ticked, densely spaced \n"
                  "points will be elimiated and the smoothing results more \n"
                  "dramatic." );
  
  ds.addDblSpinBoxF("smooth point dist:", 1, 50, &plug.smoothMinDist, 0, 1,
                    "When a contour is smoothed: wherever two consecutive points \n"
                    "are greater than this many pixels apart, additional point(s) \n"
                    "will be added between them. \n\n"
                    "RECOMMENDED VALUE: 5" );
  ds.addDblSpinBoxF("smooth tensile value:", 0, 2, &plug.smoothTensileFract, 1, 0.1,
                    "When a contour is smoothed: a cardinal spline agorithm is used \n"
                    "with a tensile fraction of this value. This value dictates how \n"
                    "'curvy' (sensitive to direction change) the contour will be when \n"
                    "points are added --> 0 = straight line, 2 = very curvy.\n\n"
                    "RECOMMENDED VALUE: 0.5");
  
  ds.addSpinBox ( "# times to move pts:", 0, 50,
                   &plug.smoothMoveIts, 1,
                   "The number of iterations used to move point by averaging \n"
                   "the position of consequtive points using the parameters \n"
                   "below (when 'e' is pressed). \n\n"
                   "NOTE: By default this is set to '0' - meaning points will \n"
                   "be added, but no points will be moved. \n"
                   "You can move points one iteration by pressing [Shift]+[e]" );
  
  ds.addDblSpinBoxF ( "move fraction:", 0.01, 2.0,
                      &plug.smoothMoveFract, 2, 0.01,
                      "Points will be moved towards this percentage distance \n"
                      "from their current location, to the position halfway \n"
                      "between the point before and after (average pos). \n\n"
                      "RECOMMENDED VALUE: 0.25" );
  ds.addDblSpinBoxF ( "min distance to move:", 0.001, 10.0,
                      &plug.smoothMoveMinDist, 3, 0.01,
                      "Points closer than this distance to their 'average pos' \n"
                      "will NOT be moved. \n"
                      "NOTE: Without this limit, continuous smoothing would \n"
                      "eventually result in the contour turning into a circle! \n\n"
                      "RECOMMENDED VALUE: 0.20" );
  
  ds.addLabel   ( "\n--- OTHER ---" );
  ds.addCheckBox( "print result of [e] and [r]", &plug.printSmoothResults,
                  "Will output a single-line summary of changes each time \n"
                  "[e] or [r] is pressed" );
  ds.addComboBox( "show Z hint:",
                  "off|"
                  "part line left|"
                  "line on left|"
                  "line both sides|"
                  "full box scaled",
                  &plug.drawZhint,
                  "Will display a hint on the edges of the tomogram to help show what \n"
                  "slice you are on.... which you may find useful making animations. \n"
                  "Note you can also see a nice hint and quicly scroll by holding \n"
                  "[Shift] plus the second mouse button. \n" );
  ds.addSpinBox ( "line display width:",
                  1, 50, &plug.lineDisplayWidth, 1,
                  "The thickness of lines used to display "
                  "contours as lines" );
  
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  Iobj *xobj = ivwGetAnExtraObject(plug.view, plug.extraObjNum);
  imodObjectSetValue(xobj, IobjLineWidth2, plug.lineDisplayWidth);
  
  ivwRedraw( plug.view );
}


//------------------------
//-- Allows user to change options for livewire.

void DrawingTools::showLivewireOptions()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds("Livewire Options", this);
  
	ds.addHtmlLabel( "Help livewire by selecting from these options." );
	ds.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
	
	ds.addLabel( "---" );
  
  ds.addRadioGrp( "what are you tracing around?",
							    "(a) the middle of a dark membrane|"
							    "(b) the middle of a light membrane|"
								  "(c) a color image",
							    &plug.lwOpt,
								  ".....",
								  "choose this if ...... |"
								  "or choose this if ...... |"
								  "......... " );
	
	ds.addComboBox( "livewire area:",
								 "256 x 256 px|"
								 "512 x 512 px|"
								 "1024 x 1024 px|"
								 "2048 x 2048 px|"
								 "4096 x 4096 px", &plug.lwAreaSize,
								 "Determines the maximum area over which livewire will calculate \n"
								 "paths at one time. The larger this area, the further you can place \n"
								 "points apart without extra clicks... but larger area also means \n"
								 "slower processing time to calculate paths." );
	
	ds.addComboBox( "binning:",
								 "none|"
								 "bin by 2|"
								 "bin by 3|"
								 "bin by 4|"
								 "bin by 5", &plug.lwBinning,
								 "Used to reduce the resolution of the image and output contour \n"
								 "by averaging pixels in either a 3x3 or 5x5 area. Increasing this \n"
								 "value should also speedup how fast livewire runs." );
	
	ds.addComboBox( "noise reduction:",
								 "median|"
								 "mean|"
								 "gaussian", &plug.lwNoiseRed,
								 "The method used to reduce noise. We recommend 'median' noise \n"
								 "reduction - 'mean' is a tiny bit faster, but results in blurry \n"
								 "images versus the other two options." );
	
	ds.addLabel( "---" );
	
	ds.addComboBox( "livewire color:",
								  "yellow|white|black|red|green|blue", &plug.lwColor,
		 						  "What color do you want the livewire line to be?" );
	
	ds.addSpinBox ( "smoothing iterations:",
								 1, 50, &plug.lwSmoothIts, 1,
								 "The number of times to apply reduce and smooth upon "
								 "finishing a contour" );
		
	ds.addCheckPrev("apply smoothing iterations: ", &plug.lwSmooth, CB_NONE, true,
									"Will automatically apply 'contour smoothing' \n"
									"to any lines you draw." );
	
	ds.addHtmlLabel( "<font size='2.5'>This algorithm was developed by "
									 "<b>Jeffrey Bush</b>.<br>"
									 "<a href='http://www.coderforlife.com/'>Click here</a> "
									 "for video demo & source code.</fontsize>","" );
	ds.setStylePrev( "background-color: rgb(100, 255, 100);");			// light green
	
	ds.addCheckBox( "do not show again (I know what I'm doing)", &plug.lwDontShowAgain,
								  "NOTE: You can still access this by clicking the numbered button." );
	ds.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	//## CHANGE LIVEWIRE SETTINGS OF WEIGHT CALCULATOR:
	
	initLivewire( plug.xsize, plug.ysize );		// if not already, make sure livewire
																						//  variables are initialized
	setupLivewireOptions();										// change livewire settings
}

//------------------------
//-- Sets up the livewire settings ("plug.lwSettings") used by the weights
//-- calculator ("plug.weights") based on the values of "plug.lwOpt",
//-- "plug.lwAreaSize", "plug.lwBinning" and "plug.lwNoiseRed".

void DrawingTools::setupLivewireOptions()
{
	if( plug.weights == NULL )
		return;
	
	//## CHANGE LIVEWIRE SETTINGS OF WEIGHT CALCULATOR:
	
	Livewire::WeightCalculator::CoalescingMethod     setMethod;
	Livewire::WeightCalculator::PixelReductionMethod setPixelRed;
	Livewire::WeightCalculator::NoiseReductionMethod setNoiseRed;
	Livewire::WeightCalculator::EdgeDetectionMethod  setEdgeDetect;
	Livewire::WeightCalculator::AccentuationMethod   setAccentuation;
	
	//** COALESCING METHOD:
	
	setMethod = Livewire::WeightCalculator::BlueChannel;
	if(plug.lwOpt == 3)
		setMethod = Livewire::WeightCalculator::WeightedHSV;
	
	//** NOISE-REDUCTION / BINNING METHOD:
	
	setPixelRed = Livewire::WeightCalculator::NoPixelReduction;
	setNoiseRed = Livewire::WeightCalculator::NoNoiseReduction;
	
	if( plug.lwBinning==0 )				// "none":
	{
		if     ( plug.lwNoiseRed==0 )
			setNoiseRed = Livewire::WeightCalculator::MedianFilter3pxWindow;
		else if( plug.lwNoiseRed==1 )
			setNoiseRed = Livewire::WeightCalculator::MeanFilter3pxWindow;
		else if( plug.lwNoiseRed==2 )
			setNoiseRed = Livewire::WeightCalculator::GaussianFilter3pxWindow;
	}
	if( plug.lwBinning==1 )				// "bin by 2":
	{
		if     ( plug.lwNoiseRed==0 )
			setPixelRed = Livewire::WeightCalculator::Median2pxWindow;
		else if( plug.lwNoiseRed==1 )
			setPixelRed = Livewire::WeightCalculator::Mean2pxWindow;
		else if( plug.lwNoiseRed==2 )
			setPixelRed = Livewire::WeightCalculator::Mean2pxWindow;
	}
	if( plug.lwBinning==2 )				// "bin by 3":
	{
		if     ( plug.lwNoiseRed==0 )
			setPixelRed = Livewire::WeightCalculator::Median3pxWindow;
		else if( plug.lwNoiseRed==1 )
			setPixelRed = Livewire::WeightCalculator::Mean3pxWindow;
		else if( plug.lwNoiseRed==2 )
			setPixelRed = Livewire::WeightCalculator::Gaussian3pxWindow;
	}
	if( plug.lwBinning==3 )				// "bin by 4":
	{
		if     ( plug.lwNoiseRed==0 )
			setPixelRed = Livewire::WeightCalculator::Median4pxWindow;
		else if( plug.lwNoiseRed==1 )
			setPixelRed = Livewire::WeightCalculator::Mean4pxWindow;
		else if( plug.lwNoiseRed==2 )
			setPixelRed = Livewire::WeightCalculator::Gaussian4pxWindow;
	}
	if( plug.lwBinning==4 )				// "bin by 5":
	{
		if     ( plug.lwNoiseRed==0 )
			setPixelRed = Livewire::WeightCalculator::Median5pxWindow;
		else if( plug.lwNoiseRed==1 )
			setPixelRed = Livewire::WeightCalculator::Mean5pxWindow;
		else if( plug.lwNoiseRed==2 )
			setPixelRed = Livewire::WeightCalculator::Gaussian5pxWindow;
	}
	
	
	//** EDGE DETECT AND ACCENTUATION SETTING:
	
	setEdgeDetect   = Livewire::WeightCalculator::NoEdgeDetection;
	setAccentuation = Livewire::WeightCalculator::Sigmoid;
	
	//** INVERTED SETTING:
	
	bool isImageInverted = false;
			//TODO: ivwGetContrastReversed(plug.view);		// true if user toggle F11
	bool setInvert = (plug.lwOpt == 2) ? !isImageInverted : isImageInverted;
	
	
	//** UPDATE THE SETTINGS OF "plug.weights" WITH NEW VALUES:
	
	bool wasChanges =   plug.lwSettings.Method          != setMethod
	                 || plug.lwSettings.PixelReduction  != setPixelRed
	                 || plug.lwSettings.NoiseReduction  != setNoiseRed
									 || plug.lwSettings.EdgeDetection   != setEdgeDetect
									 || plug.lwSettings.EdgeDetection   != setAccentuation
	                 || plug.lwSettings.Invert          != setInvert;
	
	if( wasChanges )
	{
		plug.lwSettings = Livewire::WeightCalculator::Settings(
												setMethod, setPixelRed, setNoiseRed,
												setEdgeDetect, setAccentuation, setInvert);
		plug.weights->ChangeSettings(plug.lwSettings);
	}
	
	//## UPDATE LIVEWIRE LINE COLOR
	
	Iobj *xobjL = ivwGetAnExtraObject(plug.view, plug.extraObjLW);
	if(xobjL)
	{
		switch (plug.lwColor)
		{
			case (0):	imodObjectSetColor(xobjL, 1, 1, 0);	break;	// yellow
			case (1):	imodObjectSetColor(xobjL, 1, 1, 1);	break;	// white
			case (2):	imodObjectSetColor(xobjL, 0, 0, 0);	break;	// black
			case (3):	imodObjectSetColor(xobjL, 1, 0, 0);	break;	// red
			case (4):	imodObjectSetColor(xobjL, 0, 1, 0);	break;	// green
			case (5):	imodObjectSetColor(xobjL, 0, 0, 1);	break;	// blue
		}
	}
	
}

//------------------------
//-- Allows user to change options for the wand.

void DrawingTools::showWandOptions()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds("The Wand Options", this);
  
	ds.addHtmlLabel( "Help the wand by selecting from these options." );
	ds.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
	
	ds.addLabel( "---" );
	
	ds.addSpinBox ( "smoothing iterations:",
								 1, 50, &plug.waSmoothIts, 1,
								 "The number of times to apply reduce and smooth upon "
								 "finishing a contour" );
	
	ds.addCheckPrev("apply smoothing iterations: ", &plug.waSmooth, CB_NONE, true,
									"Will automatically apply 'contour smoothing' \n"
									"to any lines you draw." );
	
	ds.addHtmlLabel( "<font size='2.5'>This algorithm was based on code by <br>"
									"<b>Andrew Huynh</b> and <b>Jeffrey Bush</b>.<br>"
									"<a href='http://www.slashsegmentation.com/'>Click here</a> "
									"for video demo & source code.</fontsize>","" );
	ds.setStylePrev( "background-color: rgb(100, 255, 100);");			// light green
	
	ds.addDblSpinBoxF( "distance bias:", 0, 2.0f, &plug.waDistBias, 2, 0.05,
										 "A coefficient representing a bias for points further away \n"
										 "from where you clicked (but inside the circle) to be less \n"
										 "likely to get selected. \n"
										 "RECOMMENDED VALUE: 0.2" );
	
	ds.addCheckBox( "do not show again (I know what I'm doing)", &plug.waDontShowAgain,
								  "NOTE: You can still access this by clicking the numbered button." );
	ds.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
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
  if( nConts > 100 )
    printVals = false;
  
	CustomDialog ds("Sorting Options", this);
  ds.addLabel   ( "contours to sort (inclusive):" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours after this contour "
                  "(inclusive) will be reordered" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour "
                  "(inclusive) will be reordered" );
  ds.addRadioGrp( "sort contours by:      (sort criteria)",
                  "surface number|"
                  "number points|"
                  "contour length|"
                  "area|"
                  "clockwise area|"
                  "avg segment length|"
                  "max segment length|"
                  "avg point size|"
                  "avg gray value|"
                  "interpolated|"
                  "random|"
                  "mean x|"
                  "mean y|"
                  "mean z|"
                  "min x|"
                  "min y|"
                  "min z",
                  &plug.sortCriteria,
                  "",
                  "Sorts contours by their surface number... "
                    "(helps to run imodmesh first!)|"
                  "Sorts by the number of points (empty first)|"
                  "Length of the contours (open or closed - depending on object/contour)|"
                  "Area of the contour (smallest first)|"
                  "From largest anti-clockwise to no area to largest clockwise area|"
                  "From contour with least (average) distance between points to largest|"
                  "From contour with smallest max distance between points to largest|"
                  "Average point size over all points in the contour "
                    "(using object default if not set)|"
                  "Uses the average gray value of the pixel closest to each point|"
                  "Stippled contours first|"
                  "Uses a random number for each contour|"
                  "Contour's center of mass in X|"
                  "Contour's center of mass in Y|"
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
  
	ds.exec();
	if( ds.wasCancelled() )
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
  
  CustomDialog ds("Find Options", this);
  ds.addComboBox( "Find property:",
                  "contour surface number|"
                  "contour number of points|"
                  "contour length|"
                  "contour area|"
                  "contour clockwise area|"
                  "contour avg segment length|"
                  "contour max segment length|"
                  "contour avg point size|"
                  "contour avg gray value|"
                  "contour interpolated|"
                  "contour random|"
                  "contour mean x value|"
                  "contour mean y value|"
                  "contour mean z value|"
                  "contour min x value|"
                  "contour min y value|"
                  "contour min z value|"
                  "point x value|"
                  "point y value|"
                  "point z value|"
                  "point size|"
                  "point gray value",
                  &plug.findCriteria,
                  "The criteria used to advance through contours"
                  "and/or points using [y]\n" );
  ds.addCheckBox( "start search using value below",
                  &useStartVal,
                  "If not ticked will use the (find value) "
                  "of the selected contour/point instead." );
  ds.addLineEditF( "Value to find:               ",
                  INT_MIN, INT_MAX, &startVal, 5,
                  "The value to start searching for" );
  ds.addLabel   ( "-----\n"
                  "NOTE: Use [y] and [Y] to advance through\n"
                  "   points/contours with increasing\n"
                  "   value - according to the specified\n"
                  "   'find property' - compared to the\n"
                  "   currently selected point/contour." );
  
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  edit_goToContNextBiggestFindVal( false, true, !useStartVal, startVal );
}


//------------------------
//-- Allows the user to change several contour properties over a range of contours
//-- in the selected object

void DrawingTools::modifyRangeContours()
{
  if( !isCurrObjValidAndShown() )  {
    wprint("\aMust select valid object\n");
    return;
  }
  
  
  Imod  *imod  = ivwGetModel(plug.view);
  Iobj  *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int nConts = csize(obj);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int         contMin         = 1;
  int         contMax         = nConts;
  static bool makeOpen        = false;
  static bool makeClosed      = false;
  static bool removePtSize    = false;
  static bool removeFineGrain = false;
  static bool makeStippled    = false;
  static bool makeUnstippled  = false;
  static bool roundToInc      = false;
  static float roundIncrement = 1.0f;
  
	CustomDialog ds("Modify Contour Range",this);
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour (inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addLabel   ( "-----\n" );
  ds.addCheckBox( "remove all point sizes", &removePtSize,
                  "Any contour with a non-default point size (for the object) \n"
                  "will have all it's points deleted and added again" );
  ds.addCheckBox( "remove all fine grain info", &removeFineGrain,
                  "Resets all fine grain info and the point size array, but \n"
                  "keeps contour 'open' and 'stippled' flags" );
  ds.addCheckBox( "make open", &makeOpen );
  ds.addCheckBox( "make closed", &makeClosed );
  ds.addCheckBox( "make stippled", &makeStippled );
  ds.addCheckBox( "make unstippled", &makeUnstippled );
  ds.addCheckBox( "round all pts to ", &roundToInc );
  ds.addDblSpinBoxF( "   nearest increment:", 0.01, 999, &roundIncrement, 3, 0.5 );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  contMin  -= 1;
  contMax  -= 1;
  
  if(roundIncrement<=0)
    return;
  
  //## EXECUTE CHANGE CONTOURS:
  
  int contsProcessed   = 0;
  int totContsMadeOpen = 0;
  int totContsMadeClosed = 0;
  int totContsPtSizeRemoved = 0;
  int totContsMadeStippled = 0;
  int totContsMadeUnstippled = 0;
  int totContsRounded = 0;
  long totPtsRounded = 0;
  
  for(int c=contMin; c<=contMax && c<csize(obj); c++)
  {
    Iobj *cont  = getCont( obj, c );
    bool closed = isContClosed(obj,cont);
    bool stippled = isInterpolated(cont);
    contsProcessed++;
    
    if( isEmpty(cont) )
      continue;
    
    //## CHANGE PROPERTIES:
    
    if( removePtSize )
    {
      bool nonDefaultSize = false;
      
      for(int p=0; p<psize(cont); p++)
        if( !isDefaultSize(obj,cont,p))
          nonDefaultSize = true;
      if(nonDefaultSize)
      {
        undoContourPropChg( plug.view, objIdx, c );     // REGISTER UNDO
        removePtsSize( cont );
        totContsPtSizeRemoved++;
      }
    }
    
    if( removeFineGrain )
    {
      undoContourPropChg( plug.view, objIdx, c );     // REGISTER UNDO
      removeExtraInfo( cont );
    }
    
    if( makeOpen && closed )
    {
      undoContourPropChg( plug.view, objIdx, c );     // REGISTER UNDO
      imodContourSetFlag( cont, ICONT_OPEN, 1 );
      totContsMadeOpen++;
    }
    
    if( makeClosed && !closed )
    {
      undoContourPropChg( plug.view, objIdx, c );     // REGISTER UNDO
      imodContourSetFlag( cont, ICONT_OPEN, 0 );
      totContsMadeClosed++;
    }
    
    
    if( makeStippled && !stippled )
    {
      undoContourPropChg( plug.view, objIdx, c );     // REGISTER UNDO
      setInterpolated( cont, 1 );
      totContsMadeStippled++;
    }
    if( makeUnstippled && stippled )
    {
      undoContourPropChg( plug.view, objIdx, c );     // REGISTER UNDO
      setInterpolated( cont, 0 );
      totContsMadeStippled++;
    }
    
    if( roundToInc )
    {
      int numPtsNeedRounding = 0;
      for(int p=0; p<psize(cont); p++)
      {
        Ipoint *pt = getPt(cont,p);
        if( fMod(pt->x,roundIncrement) != 0 || fMod(pt->y,roundIncrement) != 0 )
          numPtsNeedRounding++;
      }
      if( numPtsNeedRounding )
      {
        undoContourPropChg( plug.view, objIdx, c );     // REGISTER UNDO
        for(int p=0; p<psize(cont); p++)
        {
          Ipoint *pt = getPt(cont,p);
          pt->x = roundPrec( pt->x, roundIncrement );
          pt->y = roundPrec( pt->y, roundIncrement );
        }
        setInterpolated( cont, 0 );
        totContsRounded++;
        totPtsRounded += numPtsNeedRounding;
      }
    }
  }
  
  undoFinishUnit( plug.view );                      // FINISH UNDO
  
  
  //## PRINT SUMMARY:
  
  wprint("\nCONTOUR CHANGE SUMMARY:\n" );
  wprint("conts processed: %d\n", contsProcessed );
  if(makeOpen)        wprint("conts made open: %d\n", totContsMadeOpen );
  if(makeClosed)      wprint("conts made closed: %d\n", totContsMadeClosed );
  if(removePtSize)    wprint("conts removed pt size: %d\n", totContsPtSizeRemoved );
  if(makeStippled)    wprint("conts made stippled: %d\n", totContsMadeStippled );
  if(makeUnstippled)  wprint("conts made unstippled: %d\n", totContsMadeUnstippled );
  if(roundToInc)      wprint("conts rounded in X/Y: %d\n", totContsRounded );
  if(roundToInc)      wprint("point rounded in X/Y: %d\n", (int)totPtsRounded );
}

//------------------------
//-- Allows the user to delete contours which on certain slices,
//-- and/or meet the specified criteria

void DrawingTools::deleteRangeContours()
{
  if( !isCurrObjValidAndShown() )  {
    wprint("\aMust select valid object\n");
    return;
  }
  
  Imod  *imod  = ivwGetModel(plug.view);
  Iobj  *obj   = imodObjectGet(imod);
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
  
	CustomDialog ds("Contours to Delete",this);
  ds.addLabel   ( "-----\n"
                  "contours to consider:" );
  ds.addComboBox( "include:",
                  "all contours (in range)|"
                  "only key contours|"
                  "only interpolated",
                  &includeCType );
  ds.addRadioGrp( "include:",
                  "(1) object range;  all contours|"
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
  
	ds.exec();
	if( ds.wasCancelled() )
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
    
    CustomDialog ds1("Slices to Delete Contours",this);
    ds1.addLabel    ( "delete contours on these slices:" );
    ds1.addSpinBox ( "min:", -10, plug.zsize, &sliceMin, 1 );
    ds1.addSpinBox ( "max:", 1, plug.zsize+10, &sliceMax, 1 );
    ds1.addCheckBox( "ignore top and bottom slice", &sliceIgnoreEnds );
    ds1.addCheckBox( "skip every Nth slice", &sliceSkipN,
										 "Example: if you enter 10 then 9 in 10 slices will \n"
										 "get cleared, and 1 in 10 will be left alone" );
    ds1.addSpinBox ( "  where N = ", 2, 100, &sliceN, 1 );
    ds1.exec();
    if( ds1.wasCancelled() )
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
    CustomDialog ds2("Contours to Delete - Number Points",this);
    ds2.addLabel   ( "delete contours with" );
    ds2.addSpinBox ( "between:", 0, 999999, &pointsMin, 1 );
    ds2.addSpinBox ( "and:",     0, 999999, &pointsMax, 1 );
    ds2.addLabel   ( "points (inclusive)" );
    ds2.exec();
    if( ds2.wasCancelled() )
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
      
      // DNM 12/4/12 changed && to ||
      if( criteriaPoints )
      {
        if( psize(cont) > pointsMax || psize(cont) < pointsMin )
          continue;
      }
      
			undoContourRemoval( plug.view, o, c );              // REGISTER UNDO
			imodObjectRemoveContour( objO, c );
      numContsDeleted++;
    }
  }
  
  undoFinishUnit( plug.view );                      // FINISH UNDO
  
  int percentDeleted = calcPercentInt(numContsDeleted, numCandidateConts);
  wprint("%d contours deleted (%d%%)\n", numContsDeleted, percentDeleted );
}



//------------------------
//-- Provides options to crop a range of contours by the boundaries of the tomogram,
//-- and/or other x,y and z boundary window provided by the user. Cropped contours
//-- are cut open and, if specified, the regions outside are deleted.

void DrawingTools::cropRangeContours()
{
  if( !isCurrObjValidAndShown() )  {
    wprint("\aMust select valid object\n");
    return;
  }
  
  Imod  *imod  = ivwGetModel(plug.view);
  Iobj  *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int oObjs  = osize(imod);
  int nConts = csize(obj);
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int         contMin         = 1;
  int         contMax         = nConts;
  static int  includeCType    = 0;
  static int  minX            = 0;
  static int  maxX            = plug.xsize;
  static int  minY            = 0;
  static int  maxY            = plug.ysize;
  static bool delOutside      = true;
  static bool useObjToTrim    = false;
	
	CustomDialog ds("Contours to Trim",this);
  ds.addLabel   ( "-----\n"
                  "contours to consider:" );
  ds.addComboBox( "include:",
                  "all contours (in range)|"
                  "only key contours|"
                  "only interpolated",
                  &includeCType );
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour (inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addLabel   ( "-----\n"
                  "bounding box (crop area):" );
  ds.addSpinBox ( "min x:", -999999, 999999, &minX, 1 );
  ds.addSpinBox ( "max x:", -999999, 999999, &maxX, 1 );
  ds.addSpinBox ( "min y:", -999999, 999999, &minY, 1 );
  ds.addSpinBox ( "max y:", -999999, 999999, &maxY, 1 );
  ds.addCheckBox( "delete segments outside box", &delOutside,
                  "If true: will break contours intersecting the crop area then \n"
                  "          delete contours and contour segments outside this area."
                  "If false: as above, but without deleting segments outside the area" );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  contMin -= 1;
  contMax -= 1;
								 
								 
  //## SET UP BOUNDING BOX (CROP AREA):
  
  if( minX > maxX || minY > maxY )
  {
    MsgBox("Bad bounding box values entered");
    return;
  }
  
  Icont *trimCont =  imodContourNew();
  imodPointAppendXYZ( trimCont, minX, minY, 0 );
  imodPointAppendXYZ( trimCont, maxX, minY, 0 );
  imodPointAppendXYZ( trimCont, maxX+0.001, maxY, 0 );
  imodPointAppendXYZ( trimCont, minX+0.001, maxY, 0 );
            // NOTE: I'm still having problems with "line_doLinesCrossAndWhere"
            //       dealing with vertical lines - hence I add 0.001
  
  
  //## TRIM CONTOURS OUTSIDE SPECIFIED BOUNDING BOX:
  
  int numContsDeleted = 0;
  int numContsBroken = 0;
  
  for( int c=MIN(contMax,csize(obj)-1); c>=contMin; c-- )
  {
    Icont *cont = getCont(obj, c);
    bool closed = isContClosed(obj,cont);
    
    if(    isEmpty(cont)
        || ( includeCType == 1 &&  isInterpolated( cont ) )
        || ( includeCType == 2 && !isInterpolated( cont ) ) )
      continue;
    
    bool cross  = cont_doCountoursCross( trimCont, cont, true, closed );
    bool inside = imodPointInsideCont( trimCont, getPt(cont,0) );
    
    if( cross )
    {
      vector<IcontPtr> contSeg;
      vector<IcontPtr> trimSeg;
      int numIntersectPts =
        cont_getIntersectingSegments( cont, trimCont, contSeg, trimSeg  );
      
      if( contSeg.size() == 1 )
      {
        undoContourDataChgCC(plug.view);                  // REGISTER UNDO
        cont_copyPts( contSeg[0].cont, cont, true );
      }
      
      for (int i=(int)contSeg.size()-1; i>=0; i--)   // make any new contours open
      {
        Icont *seg = contSeg[i].cont;
        if( delOutside &&
            ( psize(seg) < 2 || !imodPointInsideCont( trimCont, getPt(seg,1) ) ) )
          eraseContour(contSeg, i);
        else
          setOpenFlag( seg, 1 );
      }
      
      if( contSeg.size() == 1 )
      {
        undoContourDataChg(plug.view, objIdx, c);                  // REGISTER UNDO
        cont_copyPts( contSeg[0].cont, cont, true );
        undoContourPropChg(plug.view, objIdx, c);                  // REGISTER UNDO
        setOpenFlag( cont, 1 );
      }
      else if( contSeg.size() > 1 )
      {
        undoContourDataChg(plug.view, objIdx, c);                  // REGISTER UNDO
        cont_copyPts( contSeg[0].cont, cont, true );
        undoContourPropChg(plug.view, objIdx, c);                  // REGISTER UNDO
        setOpenFlag( cont, 1 );
        for( int i=1; i<contSeg.size(); i++ )
          edit_addContourToObj( obj, contSeg[i].cont, true );
      }
      
      numContsBroken++;
      
      deleteContours(contSeg);
      deleteContours(trimSeg);
    }
    else if( delOutside && !inside )
    {
      undoContourRemovalCO( plug.view, c );              // REGISTER UNDO
      imodObjectRemoveContour( obj, c );
      numContsDeleted++;
    }
  }
  
  imodContourDelete( trimCont );
  
  if( numContsDeleted || numContsBroken )
    undoFinishUnit( plug.view );                      // FINISH UNDO
  
  
  if(numContsDeleted)
    wprint("%d contours deleted\n", numContsDeleted );
  wprint("%d contours broken by boundary box\n", numContsBroken );
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
  Iobj  *obj   = imodObjectGet(imod);
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
  
	CustomDialog ds("Copy or Move Contour Range",this);
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour (inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addComboBox( "include:",
                  "all contours|"
                  "only key contours|"
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
	
	ds.exec();
	if( ds.wasCancelled() )
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
    //undoObjectAddition( plug.view, osize(imod) );                 // REGISTER UNDO
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
  Iobj  *obj   = imodObjectGet(imod);
  Icont *cont  = imodContourGet(imod);
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
  
	CustomDialog ds("Copy or Move Contour Range",this);
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour \n"
                  "(inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour \n"
                  "(inclusive) will be changed" );
  ds.addLabel   ( "-----\n"
                  "translate:" );
  ds.addLineEditF( "x:", INT_MIN, INT_MAX, &translateX, 10 );
  ds.addLineEditF( "y:", INT_MIN, INT_MAX, &translateY, 10  );
  ds.addLineEditF( "z:", INT_MIN, INT_MAX, &translateZ, 10  );
  ds.addLabel   ( "-----\n"
                  "scale:" );
  ds.addLineEditF( "x:", INT_MIN, INT_MAX, &scaleX, 10  );
  ds.addLineEditF( "y:", INT_MIN, INT_MAX, &scaleY, 10  );
  ds.addLabel   ( "-----\n"
                  "rotate:" );
  ds.addLineEditF( "degrees:", INT_MIN, INT_MAX, &rotateDegrees, 10  );
  ds.addLabel   ( "-----\n"
                  "other options:" );
  ds.addCheckBox( "copy contours",  &copy );
  ds.addCheckBox( "scale and translate around MBR center", &useMBRcenter);
	
	ds.exec();
	if( ds.wasCancelled() )
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
  Iobj  *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  Ipoint *pt  = imodPointGet(imod);
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
  
  
	CustomDialog ds("Move Point(s)",this);
  ds.addLabel    ( "point position:" );
  ds.addLineEditF( "x:", INT_MIN, INT_MAX, &posX, 5 );
  ds.addLineEditF( "y:", INT_MIN, INT_MAX, &posY, 5 );
  ds.addLineEditF( "z:", INT_MIN, INT_MAX, &posZ, 5 );
  ds.addLabel    ( "-----\n"
                   "other options:" );
  ds.addCheckBox ( "move relative to"
                   "\ncurrent position",  &moveRelative );
  ds.addCheckBox ( "move whole contour",  &moveWholeCont );
  ds.addCheckBox ( "move contour's MBR to here", &moveMBRcenter );
  ds.addLabel    ( msg.c_str() );
	
	ds.exec();
	if( ds.wasCancelled() )
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
  Iobj *obj   = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int nConts = csize(obj);
  
  int           contMin          = contIdx+1;
  int           contMax          = contIdx+1;
  int           objToIdx         =  osize(imod);
  static float  radius           = 10;
  static int    minAngleChamfers = 20;
  static bool   open = true;
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
	CustomDialog ds("Expand Contours",this);
  ds.addLabel   ( "contour range:" );
  ds.addSpinBox ( "min:", 1, nConts, &contMin, 1,
                  "Only contours AFTER this contour (inclusive) will be changed" );
  ds.addSpinBox ( "max:", 1, nConts, &contMax, 1,
                  "Only contours BEFORE this contour (inclusive) will be changed" );
  ds.addLabel   ( "-----\n"
                  "options:" );
  ds.addLineEditF( "radius to expand by:", 0, INT_MAX, &radius, 5 );
  ds.addSpinBox  ( "min angle for chamfers:",1,360,&minAngleChamfers,1 );
  ds.addCheckBox ( "treat contours as open", &open );
  ds.addSpinBox  ( "object for new contours", 1, osize(imod),&objToIdx, 1 );
  
  ds.exec();
  if( ds.wasCancelled() )
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
//-- Allows the user to specify a range of objects and a value to round
//-- points to in x, y and or z. All points in the object range that
//-- are not evenly divisible by the specified values will be shifted to the
//-- nearest value.

void DrawingTools::roundPoints()
{
	Imod *imod  = ivwGetModel(plug.view);
	int nObjects = osize(imod);
	int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
	//## GET USER INPUT FROM CUSTOM DIALOG:
	
	int         objMin         = contIdx+1;
	int         objMax         = contIdx+1;
	
	static bool roundX				 = true;
	static bool roundY				 = true;
	static bool roundZ				 = true;
	static float xDivisor      = 1.0f;
	static float yDivisor      = 1.0f;
	static float zDivisor      = 1.0f;
	
	//static bool showSummaryFirst = true;
	
	
	CustomDialog ds("Clean Model Options",this);
	ds.addLabel    ("object range:" );
	ds.addSpinBox  ("min object:", 1, nObjects, &objMin, 1 );
	ds.addSpinBox  ("max object:", 1, nObjects, &objMax, 1 );
	ds.addLabel    ("-----" );
	
	ds.addLineEditF("", 0.000001f, 10000.0f, &xDivisor, 6, "Divisor for x." );
	ds.addCheckPrev("round x to nearest: ", &roundX, CB_NONE, true,
									"If true: each point will be rounded to this value in x.\n" );	
	ds.addLineEditF("", 0.000001f, 10000.0f, &yDivisor, 6, "Divisor for y." );
	ds.addCheckPrev("round y to nearest: ", &roundY, CB_NONE, true,
									"If true: each point will be rounded to this value in y.\n" );
	ds.addLineEditF("", 0.000001f, 10000.0f, &zDivisor, 6, "Divisor for z." );
	ds.addCheckPrev("round z to nearest: ", &roundX, CB_NONE, true,
									"If true: each point will be rounded to this value in z.\n" );
	
	ds.addLabel    ("WARNING: Please save first, as you \n"
									"         cannot undo this operation." );
	ds.setStylePrev("background-color: rgb(255, 40, 40);");			// red
	
	ds.exec();
  if( ds.wasCancelled() )
    return;
  
	
	//## VALIDATE RESULTS:
	
	int objMinIdx = MAX( 0,          objMin-1);
	int objMaxIdx = MIN( nObjects-1, objMax-1);
	
	if( xDivisor <=0 || xDivisor <=0 || xDivisor <=0 )
	{
		MsgBox("Bad divisor values");
		return;
	}
	
	float halfXDiv = 0.5f * xDivisor;
	float halfYDiv = 0.5f * yDivisor;
	float halfZDiv = 0.5f * zDivisor;
	
	
	//## PERFORM ROUNDING OF POINTS:
	
	long numPtsMoved = 0;
	
	for (int o=objMinIdx; o<=objMaxIdx; o++)
	{
		Iobj *obj = getObj(imod, o);
		for( int c=0; c<csize(obj); c++ )
		{
			Icont *cont = getCont(obj,c);
			for( long p=0; p<psize(cont); p++)
			{
				Ipoint *pt  = getPt( cont, p );
				float x = ( floor((pt->x+halfXDiv)/xDivisor) )*xDivisor;
				float y = ( floor((pt->y+halfYDiv)/yDivisor) )*yDivisor;
				float z = ( floor((pt->z+halfZDiv)/zDivisor) )*zDivisor;
				
				bool pointChanged = false;
				
				if( roundX && pt->x != x )	{		pt->x = x;	pointChanged = true;	}
				if( roundY && pt->y != y )	{		pt->y = y;	pointChanged = true;	}
				if( roundZ && pt->z != z )	{		pt->z = z;	pointChanged = true;	}
				
				if(pointChanged)
					numPtsMoved++;
			}
		}
	}
	
	QMessageBox::information( this, "Results", QStr(numPtsMoved) + " points were moved");
	
	wprint("%d contours were movoed\n", numPtsMoved );
	
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
  static bool checkWild      = true;
  static bool deleteDupPts   = true;
  static bool deleteRedPts   = true;
  static bool deleteOutPts   = false;
  static bool roundPts       = false;
  static bool makeCW         = true;
  static bool antiCW         = false;
  static bool makeSimple     = false;
  static bool killVertSegs   = false;
  
	CustomDialog ds("Clean Model Options",this);
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
                  "tomogram's boundaries in X, Y or Z - \n"
                  "see: 'crop contours' also" );
  ds.addCheckBox( "round points to nearest slice",
                  &roundPts,
                  "rounds the Z value to the nearest slice "
                  "for any point not quite on a slice. \n"
                  "USE CAREFULLY on open contours" );
  ds.addLabel    ( "-----\n"
                   "contour options:" );
  ds.addCheckBox( "remove empty contours", &cleanConts,
                  "same effect as Edt >> Object >> Clean");
  ds.addCheckBox( "check wild flag", &checkWild,
                  "sets a wild flag to on for any contour spanning multiple slices "
                  "or off if on a single slice (used for optimization and ghosting)");
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
  ds.addCheckBox( "eliminate vertical contour segments *", &killVertSegs,
                  "for any CLOSED contour path with two consequetive points with the \n"
                  "same x or y value, will very slightly shift the second so as to \n"
                  "eliminate any perfectly horizontal or vertical line segments.\n"
                  "USE CAREFULLY\n" );
  
	ds.exec();
	if( ds.wasCancelled() )
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
  
  long contsProcessed   = 0;
  long totDupPtsRemoved = 0;
  long totRedPtsRemoved = 0;
  long totOutPtsRemoved = 0;
  long totPtsRounded    = 0;
  long totContsDeleted  = 0;
  long totContsWildTog  = 0;
  long totContsReversed = 0;
  long totContsMadeSimp = 0;
  long totPtsShiftedXY  = 0;
  
  for( int o=objMin; o<=objMax && o<osize(imod); o++ )
  {
    Iobj *obj = getObj(imod,o);
    
    for( int c=csize(obj)-1; c>=0; c-- )
    {
      Icont *cont = getCont(obj, c);
      bool closed = isContClosed(obj,cont);
      
      contsProcessed++;
      
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
      
      if( killVertSegs && closed )
      {
        totPtsShiftedXY += cont_killVertAndHorzSegments(cont);
      }
        
      if( checkWild )
      {
        int wildBefore = imodContourGetFlag( cont, ICONT_WILD );
        imodel_contour_check_wild(cont);  
        if ( (int)wildBefore != (int)imodContourGetFlag( cont, ICONT_WILD ) )
          totContsWildTog++;
      }
    }
    
  }
  
  undoFinishUnit( plug.view );                      // FINISH UNDO
  
  wprint("\nCLEAN UP SUMMARY:\n" );
  if(true)         wprint("  %d conts processed\n", contsProcessed );
  if(deleteDupPts) wprint("  %d duplicate points removed\n", totDupPtsRemoved );
  if(deleteRedPts) wprint("  %d redundant points removed\n", totRedPtsRemoved );
  if(deleteOutPts) wprint("  %d point outside tomogram removed\n", totOutPtsRemoved );
  if(roundPts)     wprint("  %d point rounded in Z\n", totPtsRounded );
  if(cleanConts)   wprint("  %d empty contours removed\n", totContsDeleted );
  if(checkWild)    wprint("  %d contour wild flags changed\n", totContsWildTog );
  if(makeCW)       wprint("  %d contours made %s\n", totContsReversed,
                          (antiCW)? "anti-clockwise" : "clockwise" );
  if(makeSimple)   wprint("  %d non-simple contours fixed\n", totContsMadeSimp );
  if(killVertSegs) wprint("  %d points nudged to prevent vert or horz segments\n",
                          totPtsShiftedXY );
  
  checkForNamelessObjects(true);
}



//------------------------
//-- Checks for nameless objects and outputs warning text if there are more than
//-- two objects in the model and any of them are missing names.
//-- If there are more than "minObjsNameWarning" objects in the model or if
//-- "forceMessageBox" is true a message box is generated, otherwise the message
//-- just appears in the IMOD window.

void DrawingTools::checkForNamelessObjects( bool forceMessageBox )
{    
  Imod *imod  = ivwGetModel(plug.view);
  
  if( osize(imod) <= 2 )          // if there are less than 2 objects don't
    return;                       // don't bother enforcing names / generating warning
  
  //## COUNT NUMBER OF OBJECTS WITH NO NAME
  
  int numObjsNoName = 0;
  string listObjsNoName = "";
  
  for( int o=0; o<osize(imod); o++ )
  {
    Iobj *obj = getObj(imod,o);
    string objName = toString( imodObjectGetName(obj) );
    if( objName.length() == 0 )
    {
      listObjsNoName += (numObjsNoName) ? "," + toString(o+1) : toString(o+1);
      numObjsNoName++;
    }
  }
  if( numObjsNoName == 0 )    // if no objects with empty names were found: do nothing
    return;
  
  //## IF OBJECTS WITH NO NAME WERE FOUND: GENERATE WARNING
  
  QString warningStr = "WARNING: " + QStr(numObjsNoName) + " objects have no name! ";
  if(numObjsNoName==1)
    warningStr = "WARNING: one of your objects has no name! ";
  
  wprint("");
  wprint("\a" + warningStr.toLatin1() );
  
  if( osize(imod) <= plug.minObjsNameWarning && !forceMessageBox )
    return;     // if there are less than the required number of objects, return early
  
  
  //## IF WE GET HERE WE WANT TO GENERATE A "NAG SCREEN":
  
  int action = 0;
  CustomDialog ds("Missing Labels",this);
  ds.addLabel   ( "" );
  ds.addLabel   ( warningStr, true );
  ds.setStylePrev( "color: rgb(255, 0, 0); background-color: rgba(255, 255, 255);", true );
  ds.addLabel   ( "" );
  ds.addLabel   ( "It's important to label objects with PROPER names\n"
                  "(e.g. 'Microtubules', 'Nucleus', etc) so others\n"
                  "can analyze, understand and reuse your models." );
  ds.addLabel   ( "-----" );
  ds.addRadioGrp( "action:",
                  "let me fix this now|"
                  "learn more and get proper names",
                  &action,
                  "",
                  "Takes you through all un-named objects and give\n"
                  "them names, one at a time.|"
                  "Takes you to the short webpage with information about good \n"
                  "naming protocol and a list of common organelles to help you \n"
                  "correctly identify and name of subcellular compoments." );
  ds.addSpinBox ( "do not check for missing names unless there are\n"
                  "at least this many objects in the model:", 3, 500,
                  &plug.minObjsNameWarning, 1,
                  "When you open Drawing Tools, this window will only appear if \n"
                  "you have more than this many objects in your model and one or \n"
                  "of those objects have empty names.\n\n"
                  "NOTE: You can still access this window and change this value via \n"
                  "'More Actions > clean model and fix contours'" );
	ds.setStylePrev("background-color: rgb(200, 200, 200);");			// grey
	
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  if( action == 0 )
  {
    for( int o=0; o<osize(imod); o++ )
    {
      Iobj *obj = getObj(imod,o);
      string objName = toString( imodObjectGetName(obj) );
      if( objName.length() == 0 )
      {
        int renamed = promptRenameObject( o );
        if(!renamed)
          return;
      }
    }
  }
  else if( action == 1 )
  {
    imodShowHelpPage("../plughelp/naming_help.html#TOP");
  }
}


//------------------------
//-- Presents a dialog which allows the user to rename an object using an autocomplete
//-- textbox which is populated with the name of common cellular and 
//-- subcellular compartments. Returns 1 if the user clicks "Okay" (to make changes
//-- or 0 if the user hits "Cancel".

int DrawingTools::promptRenameObject( int objIdx )
{  
  //## IF FIRST TIME OPENING DIALOG GENERATE WORDS FOR AUTOCOMPLETE:
  
  static QStringList wordList;
  if( wordList.size() == 0 )
  {
    wordList
    
          //## ADD WORDS FROM http://neurolex.org/wiki/Subcellular_Parts_Table
    
                        // A:
    << "Actin Filament"
    << "Active Zone Cytomatrix" 
    << "Active Zone Dense Projection" 
    << "Active Zone Plasma Membrane" 
    << "Age Associated"     // ??
    << "Amorphous Vesicle" 
    << "Autolysosome" 
    << "Autophagosome" 
    << "Axolemma" 
    
                        // B:
    << "Barr Body" 
    << "Basal Body" 
    << "Bunina Body" 
    
                        // C:
    << "Cajal Body" 
    << "Cellular Inclusion" 
    << "Cellular Membrane" 
    << "Cellular Subcomponent" 
    << "Centriole" 
    << "Chromatin" 
    << "Cilium" 
    << "Classical Lewy Body" 
    << "Clathrin Coat" 
    << "Clathrin Coated Endocytic Vesicle" 
    << "Coated Pit" 
    << "Coated Tip" 
    << "Condensed Chromatin" 
    << "Contractile vacuole" 
    << "Cortical Lewy Body" 
    << "Cytoplasmic Vesicle" 
    << "Cytoskeletal Element" 
    << "Cytosol" 
    
                        // D:
    << "Dendritic Microtubule" 
    << "Dense Body" 
    << "Dense Core Vesicle" 
    << "Docked Vesicle" 
    
                        // E:
    << "Early Endosome" 
    << "Endocytic Vesicle" 
    << "Endoplasmic Reticulum" 
    << "Endosomal Membrane" 
    << "Endosomal Subcomponent" 
    << "Endosome" 
    << "Extended Chromatin" 
    
                        // F:
    << "Fibrillary Inclusion" 
    << "Flame-shaped Neurofibrillary Tangle" 
    << "Free Ribosome" 
    
                        // G:
    << "Glial Cytoplasmic Inclusion" 
    << "Glial Filament" 
    << "Glial Inclusion" 
    << "Glycogen Granule" 
    << "Golgi Apparatus" 
    << "Golgi Lamellae" 
    << "Golgi Subcomponent" 
    << "Golgi-associated Vesicle" 
    << "Granular Vesicle" 
    
                        // H:
    << "Hyaline Inclusion" 
    
                        // I:
    << "Inter-Golgi Transport Vesicle" 
    << "Interchromatin Granule" 
    << "Intermediate Filament" 
    << "Intermediate Filament"        // sao952483289
    << "Intracellular Membrane" 
    
                        // L:
    << "Lamellar Body" 
    << "Laminated Body" 
    << "Large Vesicle" 
    << "Late Endosome" 
    << "Lewy Body" 
    << "Lewy Body-like Hyaline Inclusion" 
    << "Lipofuscin" 
    << "Lumen Cargo" 
    << "Lysosome" 
    << "Lytic vacuole" 
    
                        // M:
    << "Membrane Bound Organelle" 
    << "Membrane Bound Ribosome" 
    << "Membrane Cargo" 
    << "Microfilament"                // sao2006047981
    << "Microtubule"                  // sao1846835077
    << "Mitochondrial Adhaerens Complex" 
    << "Mitochondrial Chromosome" 
    << "Mitochondrial Matrix" 
    << "Mitochondrial Membrane" 
    << "Mitochondrial Membrane Inner" 
    << "Mitochondrial Membrane Outer" 
    << "Mitochondrial Subcomponent" 
    << "Mitochondrion" 
    << "Multivesicular Body" 
    
                        // N:
    << "Nematosome" 
    << "Neurofibrillary Tangle" 
    << "Neurofilament"                // sao1316272517
    << "Neuromelanin" 
    << "Neuronal Cytoplasmic Inclusion" 
    << "Neurosecretory Vesicle" 
    << "Neurotubule" 
    << "Non Membrane Bound Organelle" 
    << "Nuclear Body" 
    << "Nuclear Inner Membrane" 
    << "Nuclear Lamina" 
    << "Nuclear Membrane" 
    << "Nuclear Outer Membrane" 
    << "Nuclear Pore" 
    << "Nuclear Subcomponent" 
    << "Nucleolus" 
    << "Nucleolus-associated Heterochromatin" 
    << "Nucleoplasm" 
    << "Nucleus" 
    
                        // O:
    << "Organelle" 
    
                        // P:
    << "Peroxisome" 
    << "Pick Body" 
    << "Pigment" 
    << "Pinocytic Vesicle" 
    << "Plasma Membrane" 
    << "Plasmalemmal precursor vesicle" 
    << "Post-lysosomal Vacuole" 
    << "Post-synaptic Component" 
    << "Post-synaptic Density" 
    << "Pre-synaptic Active Zone Component" 
    << "Pre-synaptic Component" 
    << "Pre-synaptic Dense Body" 
    << "Pre-synaptic Grid" 
    << "Pre-synaptic Ribbon" 
    << "Primary Lysosome" 
    
                        // R:
    << "RER Membrane"
    << "Ribosome"
    << "Rough Endoplasmic Reticulum"
    
                        // S:
    << "SER Membrane"
    << "SER Subcomponent"
    << "Secondary Lysosome"
    << "Skein-like Inclusion"
    << "Skein-like inclusion"
    << "Smooth Endoplasmic Reticulum" 
    << "Smooth Membrane" 
    << "Sorting Endosome" 
    << "Spine Apparatus" 
    << "Star-shaped Neurofibrillary Tangle" 
    << "Storage vacuole" 
    << "Subplasmalemmal Coating" 
    << "Synaptic Component" 
    << "Synaptic Vesicle" 
    
                        // T:
    << "Taxi body" 
    << "Transport Vesicle" 
    << "Tubular Endosome" 
    
                        // V:
    << "Vacuole" 
    << "Vesicle" 
    << "Vesicle Cargo" 
    << "Vesicle Coat" 
    << "Vesicle Membrane" 
    << "Vesicle Other" 
    << "Vesicle Subcomponent"
    
    //## EXTRA LABELS:
    << "Axon"
    << "Crinophagic Body"
    << "Dendrite"
    << "Golgi C1"
    << "Golgi C2"
    << "Golgi C3"
    << "Golgi C4"
    << "Golgi C5"
    << "Golgi C6"
    << "Golgi C7"
    << "Golgi C8"
    << "Immature Insulin Granule"
    << "Mature Insulin Granule"
    << "Golgi Trans-most Cisternae"
    
		<< "UNKNOWN"
		<< "GOLD FIDUCIALS"
		<< "POINT OF INTEREST"
		<< "RULER"
		<< "TOMOGRAM BOUNDARIES"
		
		//## STEREOLOGY-SPECIFIC LABELS:
		<< "STEREOLOGY"
		<< "STEREOLOGY_SETTINGS"
		<< "STEREOLOGY_GRID"
		<< "NO_CATEGORY"
		<< "NOTHING"
		<< "0_INTERSECTIONS"
		<< "1_INTERSECTIONS"
		<< "2_INTERSECTIONS"
		<< "3_INTERSECTIONS"
		<< "4_INTERSECTIONS"
		<< "5_INTERSECTIONS"
		<< "6_INTERSECTIONS"
		<< "7_INTERSECTIONS"
		<< "8_INTERSECTIONS"
		<< "9_INTERSECTIONS"
		<< "10_INTERSECTIONS";
  }
  
  //## CHECK OBJECT EXISTS:
  
  Imod *imod  = ivwGetModel(plug.view);
  if( objIdx >= osize(imod) || objIdx < 0 )
    return 0;
  Iobj *obj   = getObj(imod,objIdx);
  
  
  //## CREATE A DIALOG FOR OBJECT RENAMING:
  
  float red, green, blue;
  imodObjectGetColor( obj, &red, &green, &blue );
  string colorStr = "background-color: rgb(" + toString(red*255) + ","
    + toString(green*255) + "," + toString(blue*255) + ");";
  
  string objName = toString( imodObjectGetName(obj) );
  QColor color(red*255,green*255,blue*255);
  QString objectType = isObjClosed(obj) ? " closed contours " : " open contours ";
  
  CustomDialog ds1("Fix Object Names",this);
  ds1.addLabel   ( "Object " + QStr(objIdx+1) + ":", true );
  ds1.addLineEdit( " name: ", &objName );
  ds1.addAutoCompletePrev(wordList, false);
  
  ds1.addColorSel( " contains: " + QStr( csize(obj) ) + objectType +
                   "                  color: ", &color );
  ds1.addLabel   ( "-----", false );
  ds1.addHtmlLabel   ( "<i>TIP: To find proper names & examples visit:<br>"
                       " <a href='http://neurolex.org/wiki/Subcellular_Parts_Table'>www.neurolex.org</a>"
                       " or use <b>'Special > Name Wizard'</b></i>" );
  ds1.exec();
  
  if( ds1.wasCancelled() )
  {
    return 0;
  }
  else
  {
    setObjColor( obj,color.red(),color.green(),color.blue() );
    imodObjectSetName( obj, (char *)objName.c_str() );
    return 1;
  }
}


//------------------------
//-- Method used for testing new routines.

void DrawingTools::test()
{  
  Icont *cont = getCurrCont();
  Iobj *obj = getCurrObj();
  
  if( !isContValid(cont) )
  {
    wprint("Have not selected valid contour\n");
    return;
  }
  
  ivwRedraw( plug.view );
}


//------------------------
//-- Cut currently selected contour (ready to paste elsewhere).

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
//-- Copy currently selected contour into "plug.copiedCont" (ready to paste).

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
//-- Paste contour from "plug.copiedCont" to current slice.

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


//------------------------
//-- Copies the currently selected contour to the current slice.
//-- If "allContsOnSlice" is true it will take this further and also
//-- copy up all other contours in this object which are on the same
//-- slice as the selected contour.
//-- Return 1 if handled, 0 if NOT handled.

int  DrawingTools::copyCurrContToView(bool allContsOnSlice)
{
  if( !isCurrContValid() )  {
    wprint("\aMust select contour first\n");
    return 0;
  }
  
  Imod *imod  = ivwGetModel(plug.view);
  Icont *cont = imodContourGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  int contZ = getZ(cont);
  int currZ = edit_getZOfTopZap();
  
  if( contZ == currZ )
  {
    wprint("Cannot copy contour to same slice\n");
    return 0;
  }
	
	if(allContsOnSlice)								// BRAND NEW
	{
		Iobj *obj = getCurrObj();
		for(int c=0; c<csize(obj); c++ )
		{
			if(c==contIdx)
				continue;
			Icont *fromCont = getCont( obj, c );
			if(fromCont && getZInt(fromCont)==contZ )
			{
				Icont *contNew = imodContourDup( fromCont );
				setZValue( contNew, currZ );
				edit_addContourToObj( getCurrObj(), contNew, true );
			}
		}
	}
	
  Icont *contNew = imodContourDup( cont );
  setZValue( contNew, currZ );
  int newContPos = edit_addContourToObj( getCurrObj(), contNew, true );
	
	/*
	if(makeSmaller)
  {
    Ipoint centerMBR;
    cont_getCenterOfMBR( cont, &centerMBR );
    float diffZ = ABS(contZ - currZ);
    float scaleXY = pow( 0.95f, diffZ );
    //cout << scaleXY << endl;
    cont_scaleAboutPtXY( contNew, &centerMBR, scaleXY, scaleXY );
  }
	*/
	
  
  
  undoFinishUnit( plug.view );        // FINISH UNDO
  imodSetIndex(imod, objIdx, newContPos, 0);
  ivwRedraw( plug.view );
  return 1;
}




//############################################################
//## FUNCTIONS FOR LIVEWIRE EVENTS:



//------------------------
//-- Instantiates the livewire objects "plug.weights" and "plug.livewire"
//-- for the image size specified by "w" and "h".

void DrawingTools::initLivewire( int w, int h )
{
	if( plug.lwInit == true )
		return;
	
	//## IF NOT ALREADY: SETUP THE LIVEWIRE OBJECTS:
	
	if( plug.weights == NULL )
	{
		plug.weights = new Livewire::WeightCalculator(w, h, plug.lwSettings );
	}
	if( plug.livewire == NULL )
	{
		plug.livewire = new Livewire::LivewireCalculator(plug.weights);
		plug.window->connect(plug.livewire, SIGNAL(finished()),
												 plug.window, SLOT(livewireFinished()));
	}
	if( plug.livewireF == NULL )
	{
		plug.livewireF = new Livewire::LivewireCalculator(plug.weights);
		plug.window->connect(plug.livewireF, SIGNAL(finished()),
												 plug.window, SLOT(livewireFinished()));
	}
	if( plug.lwPts == NULL )
	{
		plug.lwPts = imodContourNew();
	}
	
	plug.lwInit = true;
	setupLivewireOptions();			// change livewire settings if appropriate
}

//------------------------
//-- Call back function for livewire finishing:

void DrawingTools::livewireFinished()
{
	drawExtraObjectLivewire( true );
}



//############################################################
//## BASIC METHODS TO CHANGE PLUG DATA:

//------------------------
//-- Brings up a custom dialog allowing the user to change what
//-- tools apppear and the order of these tools shown in the main
//-- plugin window.

void DrawingTools::customizeToolOrder()
{
	int preset = 0;
	QString modeList = "--none-- (hide)|Warp|Sculpt|Join|Livewire|Wand|"
	                   "Eraser|Measure|Transform|Curve|Circle|Correct";
	
	//## GET USER INPUT FROM CUSTOM DIALOG:
		
	CustomDialog ds("Customize Drawing Modes", this);
	
	ds.addHtmlLabel( "Change which drawing modes you<br>"
									 "want to appear and in what order.<br>"
									 "Note that the numbers correspond<br>"
									 "to shortcut keys." );
	ds.setStylePrev( "background-color: rgb(150, 150, 150);" );			// grey
	
	ds.addLabel( "---" );
	
	ds.addLabel( "          [1]:  Normal  (fixed)" );
	
	for(int i=1; i<NUM_TOOLS_SHOWN;i++)
		ds.addComboBox( "          ["+QStr(i+1)+"]:  ", modeList, &plug.modeOrder[i] );
	
	ds.addLabel( "---" );
	
	ds.addComboBox( "preset:  ",
								 "custom (values above)|"
								 "default: W,S,J,L,E..|"
								 "auto:    L,TW,W,S..|"
								 "old:    S,J,T,E,W..", &preset,
								 "Lets you choose from a bunch of easy presets" );
	ds.setStylePrev("background-color: rgb(150, 150, 150);");			// grey
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
	
	//## CHANGE ORDER OF TOOLS:
	
	if( preset == 1)				//	"default"
	{
		plug.modeOrder[0] = DM_NORMAL;
		plug.modeOrder[1] = DM_WARP;
		plug.modeOrder[2] = DM_SCULPT;
		plug.modeOrder[3] = DM_JOIN;
		plug.modeOrder[4] = DM_LIVEWIRE;
		plug.modeOrder[5] = DM_WAND;
		plug.modeOrder[6] = DM_ERASER;
		plug.modeOrder[7] = DM_MEASURE;
		plug.modeOrder[8] = DM_NORMAL;
	}
	else if( preset == 2)		//	"auto"
	{
		plug.modeOrder[0] = DM_NORMAL;
		plug.modeOrder[1] = DM_LIVEWIRE;
		plug.modeOrder[2] = DM_WAND;
		plug.modeOrder[3] = DM_WARP;
		plug.modeOrder[4] = DM_SCULPT;
		plug.modeOrder[5] = DM_CIRCLE;
		plug.modeOrder[6] = DM_ERASER;
		plug.modeOrder[7] = DM_MEASURE;
		plug.modeOrder[8] = DM_NORMAL;
	}
	else if( preset == 3)		//	"old"
	{
		plug.modeOrder[0] = DM_NORMAL;
		plug.modeOrder[1] = DM_SCULPT;
		plug.modeOrder[2] = DM_JOIN;
		plug.modeOrder[3] = DM_TRANSFORM;
		plug.modeOrder[4] = DM_ERASER;
		plug.modeOrder[5] = DM_WARP;
		plug.modeOrder[6] = DM_CURVE;
		plug.modeOrder[7] = DM_MEASURE;
		plug.modeOrder[8] = DM_NORMAL;
	}
	
	changeRadioOptions();
}


//------------------------
//-- Change "plug.drawMode" to the tool maching the "modeIdx"
//-- provided:

void DrawingTools::changeMode( int modeIdx )
{
	if(modeIdx < 0 || modeIdx   >= NUM_TOOLS_SHOWN)	modeIdx   = 0;
	int newMode = plug.modeOrder[ modeIdx ];
	
	if( plug.drawMode!=newMode && newMode==DM_LIVEWIRE && !plug.lwDontShowAgain )
		showLivewireOptions();
	
	if( plug.drawMode!=newMode && newMode==DM_WAND && !plug.waDontShowAgain )
		showWandOptions();
	
	plug.drawMode = newMode;
}

//------------------------
//-- Change "plug.drawMode" and set appropriate radio button:

void DrawingTools::changeModeSelected( int modeIdx )
{
	if(modeIdx < 0 || modeIdx   >= NUM_TOOLS_SHOWN)	modeIdx   = 0;
	int newMode = plug.modeOrder[ modeIdx ];
	
  plug.drawMode = newMode;
  diaSetGroup(typeButtonGroup, modeIdx);
  plug.window->drawExtraObject(true);
}

//------------------------
//-- Tries to select the mode radio button matching "desiredDrawMode"
//-- but if this mode is not on the list then it defaults to 0
//-- and "plug.drawMode" changed to the corresponding value:

bool DrawingTools::changeRadioToMatchMode( int desiredDrawMode )
{
	for( int i=0; i<NUM_TOOLS_SHOWN; i++ )
	{
		if( plug.modeOrder[i] == desiredDrawMode)
		{
			diaSetGroup(typeButtonGroup, i);
			plug.drawMode = desiredDrawMode;
			return (true);
		}
	}
	
	//## IF MODE NOT FOUND, DEFAULT TO NORMAL MODE:
	
	diaSetGroup(typeButtonGroup, 0);
	plug.drawMode = DM_NORMAL;
	return (false);
}

//------------------------
//-- Change "plug.warpRadius" by "value" using the selected
//-- "plug.sculptResizeScheme". NOTE that if "slowDown" is true
//-- value is reduced to 1/5 it's value.

void DrawingTools::changeSculptCircleRadius( float value, bool slowDown )
{
  if(slowDown)
    value *= 0.2;
  
  if( plug.drawMode == DM_WARP && plug.diffWarpSize )
  {
    plug.warpRadius += value;            // linear
  }
  else
  {
    switch( plug.sculptResizeScheme )
    {
      case(SR_STAGGERED):                     // staggered
      {
        if     ( plug.sculptRadius < 2.0f   )     value *= 0.2f;
        else if( plug.sculptRadius < 5.0f   )     value *= 0.5f;
				else if( plug.sculptRadius < 10.0f  )     value *= 0.75f;
				else if( plug.sculptRadius < 20.0f  )     value *= 1.0f;
				else if( plug.sculptRadius < 50.0f  )     value *= 2.0f;
        else if( plug.sculptRadius < 100.0f )     value *= 3.0f;
				else if( plug.sculptRadius < 200.0f )     value *= 5.0f;
				else																			value *= 10.0f;
				plug.sculptRadius += value;
        break;
      }
      case(SR_LINEAR):                        // linear
      {
        plug.sculptRadius += value;
        break;
      }
      case(SR_LOG):                           // log
      {
        plug.sculptRadius *= (1.0f + (value*0.02f) ); 
        break;
      }
			case(SR_FAST):                          // fast
      {
        plug.sculptRadius += 3.0f*value;
        break;
      }
    }
    
    if( !plug.diffWarpSize )
      plug.warpRadius = plug.sculptRadius;
  }
  
  keepWithinRange( plug.warpRadius,   1.0f, 1000.0f );
  keepWithinRange( plug.sculptRadius, 1.0f, 1000.0f );
}


//############################################################
//## PROTECTED SLOTS:


//------------------------
//-- Displays a (html) help page with information about the plugin

void DrawingTools::helpPluginHelp()
{
  imodShowHelpPage("../plughelp/drawingtools.html#TOP");
}


//------------------------
//-- Callback for the buttons at the bottom

void DrawingTools::buttonPressed(int which)
{
  if      (which==0)
    close();
  else if (which==1)
		openUrl( "http://www.slashsegmentation.com/tools/imod/drawing-tools-plugin" );
	else if (which==2)
    helpPluginHelp();
}


//------------------------
//-- Window closing event handler - removes this plugin from the imod dialog manager

void DrawingTools::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)plug.window);
  clearExtraObj();
  ivwFreeExtraObject(plug.view, plug.extraObjNum);
  ivwFreeExtraObject(plug.view, plug.extraObjText);
	ivwFreeExtraObject(plug.view, plug.extraObjLW);
	ivwFreeExtraObject(plug.view, plug.extraObjLWPts);
	ivwFreeExtraObject(plug.view, plug.extraObjWPts);
	
  ivwTrackMouseForPlugs(plug.view, 0);

  // DNM 1/27/13: add this call to free allocated arrays when closing if necessary
  ivwFreeTileCachedSection(plug.view);

	if(plug.copiedCont != NULL)
	{
		imodContourDelete( plug.copiedCont );   // caused a crash on second close of plugin
		plug.copiedCont = NULL;
	}
	
	
	//for some reason none of this code below does not work  :-/ 
	//
	//plug.lwInit = false;
	//if( plug.weights!=NULL )		{ delete plug.weights;   plug.weights = NULL;   }
	//if( plug.livewire!=NULL )		{ delete plug.livewire;  plug.livewire = NULL;  }
	//if( plug.livewireF!=NULL )	{ delete plug.livewireF; plug.livewireF = NULL; }
	//if( plug.lwPts!=NULL )			{ imodContourDelete(plug.lwPts); plug.livewireF = NULL; }
	
  plug.window->saveSettings();
  
  ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
  
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
    ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
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
  edit_setZapLocation( plug.mouse.x, plug.mouse.y, newZ, redraw );
      // before hand I used ix and iy, but gave jumping problems.
  return newZ;
}


//------------------------
//-- Changes the Z slice of the top Zap window by "changeZ", without going
//-- beyond z values and returns new z value.

int edit_changeZTopZap( int changeZ )
{
  int iz;
  ivwGetTopZapZslice( plug.view, &iz );
  int newZ = iz+changeZ;
	if( newZ < 0           )	newZ = 0;
	if( newZ >= plug.zsize )	newZ = plug.zsize-1;
  ivwSetTopZapZslice( plug.view, newZ );
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
//-- Searches all contours in the current object and select the
//-- closest point on the given slice within "distTol" of "centerPt".
//-- Note that the current contour is searched first, and the closest points here
//-- will be selected if "centerPt" is within "distTolCurrCont", or if
//-- "countInsideCurrCont" is set and "centerPt" is inside the contour.
//-- Returns true if a point is selected.
//-- 
//-- NOTE: This function is called by edit_executeSculptStart.

bool edit_selectNearPtInCurrObj( Ipoint *centerPt, float distTol,
                                 float distTolCurrCont, bool countInsideCurrCont )
{
  int   z = (int)centerPt->z;
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *currCont = imodContourGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  
  //## IF THERE IS A POINT SUFFICIENTLY CLOSE IN THE CURRENT CONTOUR: SELECT IT
  
  
  if( isContValid(currCont) && imodContourZValue(currCont)==z && !isEmpty(currCont) )
  {
    float minDist = cont_minDistPtAndContourPts2D(centerPt,currCont,countInsideCurrCont);
    if( minDist <= distTolCurrCont )
    {
      edit_selectClosestPtInCurrCont( centerPt );
      return true;
    }
  }
  
  //## FOR EACH CONTOUR ON THE SAME Z SLICE: FIND THE CLOSEST POINT IN RANGE
  
  bool  ptInRange = false;
  float sqMinDistFound = distTol*distTol;
  
  int closestContIdx = -1;
  int closestPtIdx = -1;
  
  for(int c=csize(obj)-1; c>=0; c--)    // for each contour:
  {
    Icont *cont = getCont(obj, c);
    
    if( imodContourZValue(cont)==z && !isEmpty(cont) )  // if on desired slice:
    {
      for( int p=0; p<psize(cont); p++ )
      {
        float sqDistToPt = line_sqDistBetweenPts2D( getPt(cont,p), centerPt );
        
        if( sqDistToPt <= sqMinDistFound )
        {
          ptInRange = true;
          closestContIdx = c;
          closestPtIdx   = p;
          sqMinDistFound = sqDistToPt;
          
          //if (earlyExit)
          //{
          //  imodSetIndex(imod, objIdx, c, p);
          //  return true;
          //}
        }
      }
    }
  }
  
  if( ptInRange )
  {
    imodSetIndex(imod, objIdx, closestContIdx, closestPtIdx);
    return true;
  }
  
  return false;          // (if no points found close enough were found) return false
}



//------------------------
//-- Searches all contours in the current object and selects the first
//-- point (and contour) with the given z value and within "distTolerance" of 
//-- the given x coordinate (along the x axis) and y coordinate (along the y axis).
//-- Returns true if a point is found, false if no point was within this "search box".
//--
//-- NOTE: "distTolerance" represents the maximum distance (in tomogram pixels)
//--       the x, y coordinates must be from our point.
//-- NOTE: This function is called by edit_executeSculptStart.

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

bool edit_selectVisiblePtNearCoords( Ipoint *mouse, float distScreenPix )
{
  Imod *imod = ivwGetModel(plug.view);
  
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  if( objIdx < 0 )
    objIdx = 0;
  
  float zoom = 1.0f;
  ivwGetTopZapZoom(plug.view, &zoom);
  float dist = fDiv( distScreenPix, zoom );
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
    if (!isObjectValidAndShown(obj))		// if object no visible: skip
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
    int newContPos = edit_addContourToObj( imodObjectGet(imod), contNew, true );
    imodSetIndex(imod, objIdx, newContPos, 0);
    imodContourDelete(contNew);
    
    wprint("Contour has been copied\n");
    return true;
  }
  
  return false;
}



//------------------------
//-- Commences a sculpt operation.
//-- If the currently selected contour is too far from the mouse click,
//-- the algorithm will determine if the user is trying to edit/sculpt
//-- a different contour - in which case it will set this as the
//-- current contour - if not then it will create a NEW contour
//-- around the sculpt circle.

void edit_executeSculptStart()
{
	plug.newContStarted = false;
    
//## DETERMINE IF USER IS TRYING TO EDIT THE CURRENT CONTOUR,
//## A DIFFERENT CONTOUR, OR START A NEW CONTOUR:
  
  float radius = plug.sculptRadius;
  float distTolCurrCont = MAX( radius*3.0f, 10.0f );
  float distTol         = MIN( radius*2.0f, radius+10.0f );
  
  bool suitableContourSelected =
    edit_selectNearPtInCurrObj( &plug.mouse, distTol, distTolCurrCont, true );
  
  Imod *imod = ivwGetModel(plug.view);
  Iobj *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  
//## (IF SUITABLE CONTOUR WAS FOUND) SCULPT CONTOUR BY PUSHING POINTS TO
//## EDGE OF SCULPT CIRCLE:
  
  if( suitableContourSelected && isContValid(cont) )
  {
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    edit_executeSculpt();
  }

//## (IF NO SUITABLE CONTOUR WAS FOUND) ADD THE SCULPT CIRCLE AS A NEW CONTOUR
  
  else
  {
		int segments  = (radius <= 32) ? 16 : radius/2.0f;
    Icont *newCont = imodContourNew();
    cont_generateCircle(newCont, radius, segments, plug.mouse, false);
    int newContPos = edit_addContourToObj(obj, newCont, true);
    int objIdx, contIdx, ptIdx;
    imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
    imodSetIndex(imod, objIdx, newContPos, 0);
		plug.newContStarted = true;
  }
  
}


//------------------------
//-- Executes a sculpt operation by calling the appropriate funtion depending
//-- on what mouse buttons are down.

void edit_executeSculpt()
{
  float radius = plug.sculptRadius;
  
  if(plug.shiftDown)      // pinch
  {
    edit_executeSculptPinch( plug.mouse, radius );
    return;
  }
  else										// push
  {
    float mouseMoveDist = imodPointDistance( &plug.mousePrev, &plug.mouse );       
            // the distance the mouse moved from its previously recorded position 
    
    int numIntermediates = fDiv(mouseMoveDist+2.0f, radius);
            // the number of extra circles we will have to add between the last
            // and current mouse position to sculpt smoothly.
    
    if( numIntermediates > 10 ) {
      wprint("\aYou are moving the mouse to fast!\n");
      return;
    }
    for( int i=1; i<=numIntermediates; i++ )
    {
      float fractAlong = float(i) / float(numIntermediates+1);
      Ipoint intermediateMousePos =
        line_findPtFractBetweenPts2D( &plug.mousePrev, &plug.mouse, fractAlong );
      edit_executeSculptPush( intermediateMousePos, radius );
    }
    edit_executeSculptPush( plug.mouse, radius );
    return;
  }
}


//------------------------
//-- Executes an "push sculpt operation" for a "sculpt circle" (with
//-- radius "pda.sculptRadius") at the position of the mouse. 
//-- This tool is used to draw and modify contours more quickly
//-- than is possible with the a normal draw operation.
//-- 
//-- In this function, all points within the currently selected contour 
//-- which are ALSO inside the sculpt circle are pushed away to the edge
//-- of the sculpt circle. Points are also removed/added if too
//-- close together/far apart.

void  edit_executeSculptPush( Ipoint center, float radius )
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
    
    //## FOR EACH POINT: IF IT'S IN THE SCULPT CIRCLE SHIFT IT TO 
    //## EDGE OF CIRCLE AND MARK IT AS "POINT_SHIFTED"
    
    for (int i=0; i<psize(cont); i++)
    {
      float distFromCenterSq = line_sqDistBetweenPts2D( &center, getPt(cont,i) );
      if ( distFromCenterSq < radiusSq )    // if current pt is within sculpt circle:
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
    
    float maxDistAllowedBetweenPts = MIN( plug.smoothMinDist, radius*0.25f );
    
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

            // DNM 10/21/09: clear the flag that was just set because of
            // the non-matching Z values
            imodContourSetFlag(cont, ICONT_WILD, 0);
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
    
    setZValue( cont, (int)center.z );    
  }
}


//------------------------
//-- Executes an "pinch sculpt operation" for a "sculpt circle" (with
//-- radius "pda.sculptRadius") at the position of the mouse. 
//-- This function is almost identical in structure to edit_executeSculpt(), 
//-- execept that points within the sculpt circle are pulled TOWARDS
//-- the center of the circle, rather than being pushed to the edge.

void edit_executeSculptPinch( Ipoint center, float radius )
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
    
    //## FOR EACH POINT: IF IT'S IN THE SCULPT CIRCLE SHIFT IT TOWARDS THE MIDDLE
    //## OF CIRCLE AND MARK IT AS "POINT_SHIFTED"
    
    for (int i=0; i<psize(cont); i++)
    {
      float distFromCenterSq = line_sqDistBetweenPts2D( &center, getPt(cont,i) );
      if ( distFromCenterSq < radiusSq )    // if current pt is within sculpt circle:
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
//-- Completes a sculpt action by making the contour simple
//-- (so it doesn't cross itself) and reducing points if specified

void edit_executeSculptEnd()
{
	if( !isCurrContValid() )
		return;
	
  if (plug.reducePts)
    edit_reduceCurrContour();
	
	bool makeContSimple = psize(getCurrCont()) < 500
		|| !edit_isSimpleWithinCircle( getCurrCont(), &plug.mouse, plug.sculptRadius*2.0f );
	
	if( makeContSimple )		//  or new contour started "edit_executeSculptStart()"
	{
		edit_makeCurrContSimple();			// makes contour simple (this step is slow!)
  }
	
	edit_deleteCurrContIfTooSmall();
  
	if (plug.reducePts)
    edit_reduceCurrContour();
	
  if( plug.markTouchedContsAsKey && isCurrContValid() && isInterpolated(getCurrCont()) )
  {
    undoContourPropChgCC( plug.view );        // REGISTER UNDO
    setInterpolated( getCurrCont(), 0 );
  }
	
  undoFinishUnit( plug.view );        // FINISH UNDO
  ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
}


//------------------------
//-- 

void edit_executeMinorCorrectEnd( float radius )
{
	if( !isCurrContValid() )
		return;
	
  if (plug.reducePts)
    edit_reduceCurrContour();
	
	bool makeContSimple = psize(getCurrCont()) < 500
	  || !edit_isSimpleWithinCircle( getCurrCont(), &plug.mouse, radius*2.0f );
	
	if( makeContSimple )		//  or new contour started "edit_executeSculptStart()"
		edit_makeCurrContSimple();			// makes contour simple (this step is slow!)
	
	if (plug.reducePts)
    edit_reduceCurrContour();
	  
  undoFinishUnit( plug.view );        // FINISH UNDO
  ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
}

//------------------------
//-- Completes a join action by merging the contour
//-- with any other contours it touches.... and breaking it into
//-- multiple contours if a contour was split apart.

void edit_executeJoinEnd()
{
  if (plug.reducePts)
    edit_reduceCurrContour();
  
  edit_joinCurrContWithAnyTouching();
  edit_breakCurrContIntoSimpleContsAndDeleteSmallest();
  edit_makeCurrContSimple();
  edit_deleteCurrContIfTooSmall();
  
  if (plug.reducePts)
    edit_reduceCurrContour();
  
  if( plug.markTouchedContsAsKey && isCurrContValid() && isInterpolated( getCurrCont() ) )
  {
    undoContourPropChgCC( plug.view );        // REGISTER UNDO
    setInterpolated( getCurrCont(), 0 );
  }
  
  undoFinishUnit( plug.view );        // FINISH UNDO
  ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
}


//------------------------
//-- Completes a "join rectangle" action by working out where the rectange was drawn.
//-- If the start and end point are in two different contours, all points in the
//-- rectangle will be deleted, and the contours joined.
//-- If the start and end point are outside all contours, but the rectangle spans
//-- across a contour, all points inside the rectangle will be deleted, and the
//-- contour split apart either side.

void edit_executeJoinRectEnd()
{   
  //## DETERMINE NEAREST CONTOUR TO START AND END POINT:
  
  bool changeMade = false;
  
  int z = (int)plug.mouse.z;
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  
  if( !edit_selectNearPtInCurrObj(&plug.mouseDownPt, 200, 1, true) )
    return;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int startContIdx = contIdx;
  Icont *startCont = imodContourGet(imod);
  bool isStartPtInsideCont = imodPointInsideCont(getCont(obj,contIdx),&plug.mouseDownPt );
  
  if( !edit_selectNearPtInCurrObj(&plug.mouse, 200, 1, true) )
    return;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int endContIdx = contIdx;
  Icont *endCont = imodContourGet(imod);
  bool isEndPtInsideCont = imodPointInsideCont( getCont(obj,contIdx), &plug.mouse );
  
  //## CONSTRUCT JOIN RECTANGLE:
  
  float x  = plug.mouse.x;
  float y  = plug.mouse.y;
  float xS = plug.mouseDownPt.x;
  float yS = plug.mouseDownPt.y;
  float radius = plug.sculptRadius;
  
  float xDiff = xS - x;
  float yDiff = yS - y;
  float dist  = sqrt( (xDiff*xDiff) + (yDiff*yDiff) );
  float sideScale = fDiv( radius , dist );
  
  Icont *contR = imodContourNew();
  imodPointAppendXYZ( contR, x +(yDiff*sideScale), y -(xDiff*sideScale), z );
  imodPointAppendXYZ( contR, x -(yDiff*sideScale), y +(xDiff*sideScale), z );
  imodPointAppendXYZ( contR, xS-(yDiff*sideScale), yS+(xDiff*sideScale), z );
  imodPointAppendXYZ( contR, xS+(yDiff*sideScale), yS-(xDiff*sideScale), z );
  imodContourMakeDirection( contR, IMOD_CONTOUR_CLOCKWISE );
  
  //## IF RECTANGLE SPANS BETWEEN TWO CONTOURS: JOIN THEM TO FORM ONE CONTOUR
  
  if( isStartPtInsideCont && isEndPtInsideCont )
  {
    if( startContIdx == endContIdx )
    {
      wprint("\aYou must span between the inside of two seperate contours\n");
      return;
    }
		
    vector<IcontPtr> cont1Segs;
    int n1Segs = cont_breakContourByContour( cont1Segs, startCont, contR, radius*0.5f );
   
    vector<IcontPtr> cont2Segs;
    int n2Segs = cont_breakContourByContour( cont2Segs, endCont, contR, radius*0.5f );
    
    if( n1Segs != 1 || n2Segs != 1 )
    {
      wprint("\aRectangle must not intersect multiple regions of contour\n");
    }
    else
    {
      undoContourDataChg( plug.view, objIdx, startContIdx );  // REGISTER UNDO
      imodContourDefault( startCont );
      
      undoContourPropChgCC( plug.view );                  // REGISTER UNDO
      setInterpolated( endCont, false );
      undoContourDataChgCC( plug.view );                  // REGISTER UNDO
      //cont_concat( endCont, cont1Segs[0].cont, cont2Segs[0].cont, true );
      cont_copyPts( cont1Segs[0].cont, endCont, true );
      cont_copyPts( cont2Segs[0].cont, endCont, false );
      cont_addPtsSmooth( endCont, plug.smoothMinDist, plug.smoothTensileFract,
                         isContClosed(obj,startCont) );
      setZValue( endCont, z );
      
      changeMade = true;
    }
    
    deleteContours(cont1Segs);
    deleteContours(cont2Segs);
  }
  
  //## IF RECTANGLE SPANS OVER A SINGLE CONTOUR: SPLIT IT INTO TWO CONTOURS
  
  else if( !isStartPtInsideCont && !isEndPtInsideCont )
  {
    if( startContIdx != endContIdx )
    {
      wprint("\aYou must span either side of the same contour\n");
      return;
    }
    //wprint(" contIdx: %d \n", startContIdx);
    
    vector<IcontPtr> contSegs;
    int nSegs = cont_breakContourByContour( contSegs, getCont(obj,startContIdx), contR, 10.0f );
    
    if( nSegs == 1 )
    {
      undoContourDataChgCC( plug.view );      // REGISTER UNDO
      cont_copyPts( contSegs[0].cont, endCont, true );
      cont_addPtsSmooth( endCont, plug.smoothMinDist, plug.smoothTensileFract,
                         isContClosed(obj,startCont) );
      setZValue( endCont, z );
    }
    else if( nSegs == 2 )
    {
      Icont *cont1 = imodContourDup(contSegs[0].cont);
      cont_addPtsSmooth( cont1, plug.smoothMinDist, plug.smoothTensileFract,
                         isContClosed(obj,startCont) );
      setZValue( cont1, z );
      
      Icont *cont2 = imodContourDup(contSegs[1].cont);
      cont_addPtsSmooth( cont2, plug.smoothMinDist, plug.smoothTensileFract,
                         isContClosed(obj,startCont) );
      setZValue( cont2, z );
      
      undoContourDataChgCC( plug.view );      // REGISTER UNDO
      cont_copyPts( cont1, endCont, true );
      
      edit_addContourToObj(obj, cont2, true);
      
      imodContourDelete(cont1);
      imodContourDelete(cont2);
      
      changeMade = true;
    }
    else
    {
      wprint("\aRectangle must only interset two regions contour\n");
    }
    deleteContours(contSegs);
  }
  
  else
  {
    wprint("You have not used this tool correctly\n");
  }
  
  imodContourDelete( contR );
  
  if( changeMade )
  {
    undoFinishUnit( plug.view );        // FINISH UNDO
    ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
  }
}


//------------------------
//-- Commences a warp operation.
//-- This routine selects the closest point to the mouse.
//-- If the currently selected contour is too far from the mouse click,
//-- the algorithm will determine if the user is trying to edit/sculpt
//-- a different contour/point - in which case it will set this as the
//-- current contour - if not, it will apply normal drawing mode.

void edit_executeWarpStart()
{
  //## DETERMINE IF USER IS TRYING TO EDIT THE CURRENT CONTOUR,
  //## A DIFFERENT CONTOUR, OR START A NEW CONTOUR:
  
  plug.contortInProgress = false;
  
  float warpRadius = plug.warpRadius;
  
  float zapZoom = 1.0f;                 // gets the zoom of the top-most zap window
  int noZap = ivwGetTopZapZoom(plug.view, &zapZoom); 
  float sc = fDiv( 1.0f, zapZoom);   // tomogram distance for one screen pixel 
  float contortDistTol = 10.0f*sc;
  if( plug.warpBehavior == WB_LINE )
    contortDistTol = warpRadius;
  if( plug.warpBehavior == WB_AREA )
    contortDistTol = 0;
  
  bool suitableContourSelected =
    edit_selectNearPtInCurrObj( &plug.mouse, warpRadius*2.0f, 0.0f, false );
  
  Imod *imod  = ivwGetModel(plug.view);
  Iobj *obj   = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  Ipoint *pt  = imodPointGet(imod);
  
  
  //## IF NO SUITABLE CONTOUR WAS SELECTED: CREATE AND SELECT NEW CONTOUR
  
  if( !suitableContourSelected || !isContValid(cont) || !isCurrPtValid() )
  {
    Icont *newCont = imodContourNew();
    cont_generateCircle(newCont, warpRadius, 16, plug.mouse, false);
    int newContPos = edit_addContourToObj(obj, newCont, true);
    int objIdx, contIdx, ptIdx;
    imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
    imodSetIndex(imod, objIdx, newContPos, 0);
    plug.contortInProgress = (plug.warpBehavior == WB_LINE);
    return;
  }
  
  //## IF SUITABLE CONTOUR WAS SELECTED: DETERMINE IF WE WANT TO WARP OR CONTORT IT
  
  float distToCurrPt = line_distBetweenPts2D( &plug.mouse, pt );
  
  if( distToCurrPt <= contortDistTol && plug.warpBehavior != WB_AREA )
     plug.contortInProgress = true;
  
  undoContourDataChgCC( plug.view );      // REGISTER UNDO
  edit_addPtsCurrContInRadius( &plug.mouse, warpRadius,
                               warpRadius*0.2f );
}




//------------------------
//-- Executes a warp operation by determining the movement of the mouse and
//-- moving points near the point clicked by and appropriate distance.

void edit_executeContort()
{
  float warpRadius = plug.warpRadius;
  
  Imod   *imod  = ivwGetModel(plug.view);
  Iobj   *obj   = imodObjectGet(imod);
  Icont  *cont  = imodContourGet(imod);
  Ipoint *selPt = imodPointGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  if( !isContValid(cont) || !isCurrPtValid() || psize(cont) <= 1 )
    return;
  
  bool  closed     = isContClosed( obj, cont );
  float contLength = imodContourLength(cont, closed);
  
  float distToWarp = MIN( warpRadius, contLength * 0.25f );
  
  if(distToWarp == 0)
    return;
  
  float changeX =  plug.mouse.x - plug.mousePrev.x;
  float changeY =  plug.mouse.y - plug.mousePrev.y;
  
  int maxPtsToCheckEitherSide = floor( psize(cont) * 0.4);
  int minPtToCheck = ptIdx - maxPtsToCheckEitherSide;
  int maxPtToCheck = ptIdx + maxPtsToCheckEitherSide;
  
  if( !closed )
  {
    minPtToCheck = MAX( minPtToCheck, 0 );
    maxPtToCheck = MIN( maxPtToCheck, psize(cont)-1 );
  }
  
  //## PROGRESS FORWARDS FROM SELECTED POINT AND SHIFT POINTS BY DECREASING AMOUNTS:
  
  float distFromSelPt = 0.0;
  Ipoint prevPt = *selPt;
  
  for( int p=ptIdx+1; p<=maxPtToCheck; p++ )
  {
    Ipoint *currPt = getPt(cont,p);
    distFromSelPt += line_distBetweenPts2D( &prevPt, currPt );
    if( distFromSelPt >= distToWarp )
      break;
    
    float fractAlong = distFromSelPt / distToWarp;
    float distToShift = sin( (fractAlong*PI) +0.5*PI) * 0.5 + 0.5;
    prevPt = *currPt;
    
    currPt->x += changeX * distToShift;
    currPt->y += changeY * distToShift;
  }
  
  //## PROGRESS BACKWARDS FROM SELECTED POINT AND SHIFT POINTS BY DECREASING AMOUNTS:
  
  distFromSelPt = 0.0;
  prevPt = *selPt;
  
  for( int p=ptIdx-1; p>=minPtToCheck; p-- )
  {
    Ipoint *currPt = getPt(cont,p);
    distFromSelPt += line_distBetweenPts2D( &prevPt, currPt );
    if( distFromSelPt >= distToWarp )
      break;
    
    float fractAlong = distFromSelPt / distToWarp;
    float distToShift = sin( (fractAlong*PI) +0.5*PI) * 0.5 + 0.5;
    prevPt = *currPt;
    
    currPt->x += changeX * distToShift;
    currPt->y += changeY * distToShift;
  }
  
  //## MOVE CURRENT POINT:
  
  selPt->x += changeX;
  selPt->y += changeY;
  
  
  //## IF THIRD MOUSE BUTTON IS DOWN: MOVE POINTS SO THEY LINE UP MORE STRAIGHT:
  
  if(plug.but3Down)
  {
    float distFromSelPt = 0.0;
    Ipoint prevPt = *selPt;
    
    for( int p=ptIdx+1; p<=maxPtToCheck-1; p++ )
    {
      Ipoint *currPt =  getPt(cont,p);
      float distFromMouse = line_distBetweenPts2D( currPt, &plug.mouse );
      if( distFromMouse > warpRadius )
        break;
      
      Ipoint estPos = line_getPtHalfwayBetween( getPt(cont,p-1), getPt(cont,p+1) );
      *currPt = line_findPtFractBetweenPts2D( currPt, &estPos, 0.4 );
    }
    
    for( int p=ptIdx-1; p>=minPtToCheck; p-- )
    {
      Ipoint *currPt =  getPt(cont,p);
      float distFromMouse = line_distBetweenPts2D( currPt, &plug.mouse );
      if( distFromMouse > warpRadius )
        break;
      
      Ipoint estPos = line_getPtHalfwayBetween( getPt(cont,p-1), getPt(cont,p+1) );
      *currPt = line_findPtFractBetweenPts2D( currPt, &estPos, 0.4 );
    }
  }
}


//------------------------
//-- Executes a warp operation by determining the movement of the mouse and
//-- moving points near the point clicked by and appropriate distance.

void edit_executeWarp()
{
  undoContourDataChgCC( plug.view );      // REGISTER UNDO
  
  if( plug.contortInProgress )
  {
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    edit_executeContort();
    return;
  }
  
  Imod   *imod  = ivwGetModel(plug.view);
  Iobj   *obj   = imodObjectGet(imod);
  Icont  *cont  = imodContourGet(imod);
  Ipoint *selPt = imodPointGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  if( !isContValid(cont) || !isCurrPtValid() || psize(cont) <= 1 )
    return;
  
  bool  closed     = isContClosed( obj, cont );
  float contLength = imodContourLength(cont, closed);
  
  float warpRadius = plug.warpRadius;
  
  if(warpRadius == 0)
    return;
  
  float changeX =  plug.mouse.x - plug.mousePrev.x;
  float changeY =  plug.mouse.y - plug.mousePrev.y;
  
  cont_addPtsCrude( cont, warpRadius*0.25, closed );
  
  for( int p=0; p<psize(cont); p++ )
  {
    Ipoint *pt = getPt(cont,p);
    float distToCenter  = line_distBetweenPts2D( &plug.mouse, pt );
    if( distToCenter > warpRadius )
      continue;
    
    float fractToCenter = distToCenter / warpRadius;
    float moveWeight    = (1.0f - fractToCenter);
    
    pt->x += moveWeight * changeX;
    pt->y += moveWeight * changeY;
  }
  
  
  if(plug.but3Down)
  {
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    
    for( int p=1; p<psize(cont)-1; p++ )
    {
      Ipoint *pt = getPt(cont,p);
      float distToCenter  = line_distBetweenPts2D( &plug.mouse, pt );
      if( distToCenter > warpRadius )
        continue;
      
      Ipoint estPos = line_getPtHalfwayBetween( getPt(cont,p-1), getPt(cont,p+1) );
      *pt = line_findPtFractBetweenPts2D( pt, &estPos, 0.1 );
    }
  }
}

//------------------------
//-- Completes a warp action by reducing points.

void edit_executeWarpEnd()
{
  plug.contortInProgress = false;
  
  if( !isCurrContValid() )
    return;
  
  if (plug.reducePts)
  {
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    edit_reduceCurrContour();
  }
  
  undoFinishUnit( plug.view );        // FINISH UNDO
}


//------------------------
//-- Executes a curbe action by creating a new contour (if none selected)
//-- or, if the user is completing the contour it generates the contour
//-- into a smoother "curve" using cardinal spline algorightm.

void edit_executeCurve( bool forceEndContour )
{
  Imod  *imod  = ivwGetModel(plug.view);
  Iobj  *obj   = imodObjectGet(imod);
  Icont *cont  = imodContourGet(imod);
  Ipoint *pt   = imodPointGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  if( !obj )
    return;
  
  //## IF NO CONTOUR IS SELECTED: CREATE NEW ONE
  
  if( !cont )
  {
    if( !plug.but2Pressed )   // prevent mouse down creating new contour on top old one
      return;
    
    Icont *newCont = imodContourNew();
    setInterpolated( newCont, 1 );
    imodPointAppendXYZ( newCont, plug.mouse.x, plug.mouse.y, plug.mouse.z );
    edit_addContourToObj( obj, newCont, true );
    imodSetIndex(imod, objIdx, csize(obj)-1, 0);
    undoFinishUnit( plug.view );            // FINISH UNDO
    return;
  }
  
  //## DETERMINE WETHER TO ADD NEW POINT OR COMPLETE AND SMOOTH CONTOUR:
  
  if( plug.mouse.z != getZ(cont) && isContClosed(obj,cont) )
  {
    wprint("\aWrong slice\n");
    return;
  }
  
  float distFromCurrPt  = imodPointDistance( pt,            &plug.mouse );
  float distFromFirstPt = imodPointDistance( getPt(cont,0), &plug.mouse );
  
  bool readyToCloseCont = (psize(cont) > 3) && (distFromFirstPt < 10.0);
  bool readyToAddNewPt  = (distFromCurrPt  >= 20.0) || (plug.but2Pressed);
  
  if( readyToCloseCont || forceEndContour )
  {
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    cont_addPtsSmooth( cont, plug.smoothMinDist,
                       plug.smoothTensileFract, isContClosed(obj,cont), false, false );
    imodContourUnique( cont );
    undoContourPropChgCC( plug.view );      // REGISTER UNDO
    setInterpolated( cont, 0 );
    imodSetIndex(imod, objIdx, -1, -1);
    undoFinishUnit( plug.view );            // FINISH UNDO
  }
  else if(readyToAddNewPt)
  {
    undoContourDataChgCC( plug.view );      // REGISTER UNDO
    imodPointAdd( cont, &plug.mouse, ptIdx+1 );
    imodSetIndex(imod, objIdx, contIdx, ptIdx+1);
    undoFinishUnit( plug.view );            // FINISH UNDO
  }  
  
  ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
}




//------------------------
//-- Executes a circle action by adding a new contour in the shape
//-- of the circle which spans between the mouse coordinates
//-- and the last point clicked.

void edit_executeCircleEnd()
{
  Imod  *imod  = ivwGetModel(plug.view);
  Iobj  *obj   = imodObjectGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  if( !obj )
    return;
  
  //## IF USER HAS DRAGGED SUFFICENT DISTANCE, GENERATE CIRCLE:
  
	float distDragged = line_distBetweenPts2D( &plug.mouseDownPt, &plug.mouse );
	
	if( distDragged >= 3.0f )
	{
		Icont *newCont = imodContourNew();
		Ipoint centerPt = line_getPtHalfwayBetween( &plug.mouseDownPt, &plug.mouse );
		cont_generateCircle( newCont, distDragged / 2.0f, 120, centerPt, false );
		
		edit_addContourToObj( obj, newCont, true );
		imodSetIndex(imod, objIdx, csize(obj)-1, 0);
		undoFinishUnit( plug.view );            // FINISH UNDO
	}
	
	plug.window->drawExtraObject(true);
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
    if( plug.reducePtsOpt == RD_TOL )
    {
      return cont_reducePtsTol( cont, plug.reducePtsTol );
    }
    else
    {
      return cont_reducePtsMinArea( cont, plug.reducePtsMinArea,
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
    
    return cont_addPtsSmooth( cont, plug.smoothMinDist, plug.smoothTensileFract,
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
//-- Erases all visible contours in the given radius and returns the
//-- number of contours removed, and creates undo if contours
//-- were removed.

int edit_eraseContsInCircle( Ipoint center, float radius )
{
  Imod *imod = ivwGetModel(plug.view);
  
  float radiusSq = (radius*radius);
  
  int objIdx, contIdx, ptIdx;
  imodGetIndex( imod, &objIdx, &contIdx, &ptIdx );
  int numRemoved = 0;
  
  for (int o=0; o<osize(imod); o++ )
  {
    imodSetIndex( imod, o, 1, 1);
    Iobj *obj  = imodObjectGet(imod);
    
    if (!isObjectValidAndShown(obj))		// if object no visible: skip
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
            imodSetIndex( imod, objIdx, c, p );
            undoContourRemovalCC( plug.view );        // REGISTER UNDO
            imodObjectRemoveContour( obj, c );        // delete the contour
            numRemoved++;
            if( o == objIdx && c < contIdx )
              contIdx--;
            break;
          }
        }
      }
    }
  }
  
  if(numRemoved)
    undoFinishUnit( plug.view );          // FINISH UNDO
  
  //imodSetIndex( imod, objIdx, contIdx, ptIdx );
  imodSetIndex( imod, objIdx, -1, -1 );
  
  return numRemoved;
}


//------------------------
//-- Erases all visible points in the given radius, returns the number of
//-- points removed and creates undo if points were removed.

int edit_erasePointsInCircle( Ipoint center, float radius )
{
  Imod *imod = ivwGetModel(plug.view);
  
  float radiusSq = (radius*radius);
  int objIdx, contIdx, ptIdx;
  imodGetIndex( imod, &objIdx, &contIdx, &ptIdx );
  
  int numPtsRemoved = 0;
  
  for (int o=0; o<osize(imod); o++ )
  {
    imodSetIndex( imod, o, 1, 1);
    Iobj *obj  = imodObjectGet(imod);
    
    if (!isObjectValidAndShown(obj))		// if object no visible: skip
      continue;
		
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
  
  imodSetIndex( imod, objIdx, -1, -1 );    // ensures Zap doesn't jump to new slice
  
  return numPtsRemoved;
}




//------------------------
//-- Finds the first visible contour within the circle, removes any of it's
//-- points within the circle and breaks the contour apart either side,
//-- and if any changes was made it creates an undo and returs true.

bool edit_breakPointsInCircle( Ipoint center, float radius )
{
  Imod *imod = ivwGetModel(plug.view);
  
  float radiusSq = (radius*radius);
  int numPtsRemoved = 0;
  
  int objIdx, contIdx, ptIdx;
  imodGetIndex( imod, &objIdx, &contIdx, &ptIdx );
  
  for (int o=0; o<osize(imod); o++ )
  {
    imodSetIndex( imod, o, 1, 1);
    Iobj *obj  = imodObjectGet(imod);
    
    if (!isObjectValidAndShown(obj))		// if object no visible: skip
      continue;
		
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
                                                  plug.sculptRadius);
          
          for (int i=0; i<contSegments.size(); i++)   // make any new contours open
            setOpenFlag( contSegments[i].cont, 1 );
          
          if( contSegments.size() == 1 )
          {
            undoContourDataChgCC(plug.view);                  // REGISTER UNDO
            cont_copyPts( contSegments[0].cont, cont, true );
          }
          else if ( contSegments.size() > 1 )
          {
            undoContourDataChgCC(plug.view);                  // REGISTER UNDO
            cont_copyPts( contSegments[0].cont, cont, true );
            for( int i=1; i<contSegments.size(); i++ )
              edit_addContourToObj( obj, contSegments[i].cont, true );
          }
          
          undoContourPropChgCC(plug.view);                    // REGISTER UNDO
          setOpenFlag( cont, 1 );
          
          undoFinishUnit( plug.view );                        // FINISH UNDO
          imodSetIndex( imod, objIdx, -1, -1 );
          deleteContours(contSegments);
          return true;
        }
      }
    }
  }
  
  imodSetIndex( imod, objIdx, contIdx, ptIdx );
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
      cont_reducePtsTol( conts[i].cont, MAX(plug.reducePtsTol, 0.2f) );
    }
    
    //## DELETE ANY REALLY SMALL CONTOURS:
    
    for( int i=0; i<(int)conts.size(); i++ )    // for each contour: 
      if(   ( psize( conts[i].cont ) < 5 )      // if too few points
         || ( imodContourArea(conts[i].cont) < 4.0f ) )  // or very small: delete it
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
  if( isCurrContValid() ) {
    cont_makeSimple( getCurrCont() );
  }
}

//------------------------
//-- Deletes the current contour if it "too small"

void edit_deleteCurrContIfTooSmall()
{
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  
  bool isTooSmall = imodContourArea(cont) < MAX( plug.reducePtsMinArea*3.0, 10.0 );
  
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
    
    if( getZ(contCompare) != zSlice || i==contIdx || isEmpty(contCompare) )
      continue;
    
    if( cont_doCountoursCross( cont, contCompare, true, true ) )
    {
      undoContourDataChgCC( plug.view );          // REGISTER UNDO
      bool unionMade = cont_getOuterUnionPolygon( cont, cont, contCompare );
      
      if(unionMade)
      {
        imodSetIndex(imod, objIdx, i, 0);
        undoContourDataChgCC( plug.view );          // REGISTER UNDO
        imodContourDefault( contCompare );
        imodSetIndex(imod, objIdx, contIdx, ptIdx);
        
        break;      // we only want to do one at a time for now.
      }
    }
  }
}


//------------------------
//-- Takes current contour and adds extra points so that no two consequtive
//-- points are greater than "maxDistPts" apart.
//-- It also makes sure that the currently selected point is not changed by
//-- the addition of points, and returns the number of points added.

int edit_addPtsCurrCont( float maxDistBetweenPts )
{
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  if( !isContValid(cont) || psize(cont) <= 1 )
    return 0;
  
  int pointsBefore = psize(cont);
  int pointsAddedBeforeCurrPt = 0;
  
  bool closed = isContClosed( obj, cont );
  int extra = (closed) ? 0 : -1;
  
  for(int i=0; i<psize(cont)+extra; i++ )
  {
    Ipoint *currPt = getPt( cont, i );
    Ipoint *nextPt = getPt( cont, i+1 );
    
    float distToNextPt = line_distBetweenPts2D( currPt, nextPt );
    if( distToNextPt > maxDistBetweenPts )
    {
      Ipoint newPt = line_getPtHalfwayBetween( currPt, nextPt );
      imodPointAdd( cont, &newPt, i+1 );
      if( i < ptIdx )
        ptIdx++;
      i--;
    }
  }
  
  imodSetIndex(imod, objIdx, contIdx, ptIdx);
  return ( psize(cont) - pointsBefore );
}


//------------------------
//-- Selects the closest point in the current contour to the given point
//-- and returns the distance between the two points.

float edit_selectClosestPtInCurrCont( Ipoint *givenPt )
{ 
  Imod  *imod = ivwGetModel(plug.view);
  Icont *cont = imodContourGet(imod);
  if( cont == NULL || psize(cont) <= 0 )
    return 0;
  
  float closestDist;
  Ipoint closestPt;
  int closestPtIdx;
  cont_findClosestPtInContToGivenPt( &plug.mouse, cont, &closestDist, &closestPt,
                                     &closestPtIdx  );
  
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  imodSetIndex(imod, objIdx, contIdx, closestPtIdx );
  
  return (closestDist);
}

//------------------------
//-- Takes current contour and adds extra points so that no two consequtive
//-- points within the sculpt circle (near the mouse) are within
//-- "maxDistPtsInScultCircle" of each other.
//-- It also makes sure that the currently selected point is not changed by
//-- the addition of points, and returns the number of points added.

int edit_addPtsCurrContInRadius( Ipoint *centerPt, float radius, float maxDistPts )
{
  Imod  *imod = ivwGetModel(plug.view);
  Iobj  *obj  = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  
  if( cont == NULL || psize(cont) <= 1 )
    return 0;
  
  int pointsBefore = psize(cont);
  float sqRadius = SQ( radius );
  
  bool closed = isContClosed( obj, cont );
  int extra = (closed) ? 0 : -1;
  
  for(int i=0; i<psize(cont)+extra; i++ )
  {
    Ipoint *currPt = getPt( cont, i );
    Ipoint *nextPt = getPt( cont, i+1 );
    
    bool currPtInCircle = line_sqDistBetweenPts2D(currPt, centerPt) < sqRadius;
    bool nextPtInCircle = line_sqDistBetweenPts2D(nextPt, centerPt) < sqRadius;
    
    if( currPtInCircle || nextPtInCircle ) 
    {
      float distToNextPt = line_distBetweenPts2D( currPt, nextPt );
      if( distToNextPt > maxDistPts )
      {
        Ipoint newPt = line_getPtHalfwayBetween( currPt, nextPt );
        imodPointAdd( cont, &newPt, i+1 );
        if( i < ptIdx )
          ptIdx++;
        i--;
      }
    }
  }
  
  imodSetIndex(imod, objIdx, contIdx, ptIdx);
  return ( psize(cont) - pointsBefore );
}


//------------------------
//-- Finds and selects the next contour past the selected contour which
//-- overlaps (contours cross) another contour in the model

bool edit_selectNextIntersectingCont()
{
  Imod *imod = ivwGetModel(plug.view);
  
  int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
  int nObjs = osize(imod);
  
  //## FIND PROBLEM CONTOURS:
  
  for (int o=objIdx; o<(nObjs+objIdx+1); o++)         // for each object:
  {
    if( o == nObjs )
      wprint("Starting from beginning (first object)...\n");
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
      
      if( edit_findIntersectingCont( oNum, i, &pCross, &objCross, &contCross, &samePts,
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
      
      if( plug.testOverlapping && edit_countOverlappingContsCrude( oNum, i ) )
      {
        imodSetIndex( imod, o, i, 0 );
        wprint("Nested contour found\n");
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
//-- the specified range of objects which overlaps it.
//-- It also retunrs the object index (objCross) and contour index (objCross)
//-- of the overlapping contour, plus the point index (pCross) of the first
//-- line segment in the origional contour which overlaps it.
//-- If (skipPrevContsInObj) is true, then it will skip all contours in the
//-- current object (objIdx) beyond the current contour (contIdx).

bool edit_findIntersectingCont( int objIdx, int contIdx,
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
            && !(o==objIdx && c==contIdx) )
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
//-- Takes a contour, and checks it's centroid point falls inside any other contours
//-- in the same object.
//-- Returns the number of contours the contour is in.

int edit_countOverlappingContsCrude( int objIdx, int contIdx )
{
  Imod *imod  = ivwGetModel(plug.view);
  Imod *obj   = getObj(imod,objIdx);
  Imod *contI  = getCont(obj,contIdx);
  
  if( isEmpty(contI) )
    return (false);
  
  int numOverlappingConts = 0;
  int slice = getZInt(contI);
  
  Ipoint* testPt = getPt(contI,0);
  
  for(int c=0; c<csize(obj); c++)     // for each contour ahead:
  {
    Icont *cont = getCont( obj, c );
    if( isEmpty(cont) || c==contIdx )
      continue;
    
    if( ( getZInt(cont) == slice )  )     // if on same slice:
      if( imodPointInsideCont( cont, testPt ) )
        numOverlappingConts++;
  }
  
  return (numOverlappingConts);
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
    case(SORT_AVGSEGLEN):       returnStr = "CONTOUR'S MEAN SEGMENT LENGTH";  break;
    case(SORT_MAXSEGLEN):       returnStr = "CONTOUR'S MAX SEGMENT LENGTH";  break;
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
//-- NOTE: To get single grey values (but not RBG quickly) you can use:
//--     ivwGetFileValue( plug.view, pt.x,pt.y,pt.z );
//--     and see: MACMOD/imod/imodview.cpp for actual functions.
//--
//-- To efficiently get large area of slice will probably need to use
//-- "Islice" (aka "MRCSlice") structure which lives in: MACMOD/include/mrcslice.h
//-- The plugin interface has some functions for working with Islice structures here:
//-- file:///Users/andrew_noske/Documents/MACMOD/html/libhelp/mrcslice.html

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
    wprint("\aNo more 'next %s' value found - press '%s' to reset.\n",
           (findNextSmallest)?"smallest":"biggest", (findNextSmallest)?"L":"l" );
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
    case(SORT_SURFACENUM):
      returnValue = imodContourGetSurface(cont);
      break;
      
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
      
    case(SORT_AVGSEGLEN):
    {
      int    closed = isContClosed( obj, cont ) ? 1 : 0;
      float  length = imodContourLength(cont, closed); 
      returnValue = fDiv( length , psize(cont) );
    }
      break;
      
    case(SORT_MAXSEGLEN):
    {
      float maxSegLen = 0;
      for( int i=0; i<psize(cont); i++)
        updateMax( maxSegLen, imodPointDistance( getPt(cont,i), getPt(cont,i+1) ) );
      returnValue = maxSegLen;
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
      returnValue = imodPointGetSize(obj,cont,ptIdx);
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




//------------------------
//-- Returns true if, within the specified circle, the contour
//-- has no line segmentes which overlap other line segments.
//-- This function is used to avoid the more expensive "cont_isSimple()"
//-- which runs a check of wether a contour is simple over all points O(n^2).

bool edit_isSimpleWithinCircle( Icont *cont, Ipoint *center, float radius )
{
	float radSq = (radius*radius);
	
	vector<bool> ptInside;							// tracks if each point is inside the circle
	ptInside.resize( psize(cont)+1 );
	
	int minIdx = psize(cont);						// index of the first point found inside the circle
	int maxIdx = 0;											// index of the last  point found inside the circle
	int upperBound = MIN( psize(cont)+1, (int)ptInside.size() );
	
	//## FOR EACH POINT IN CONTOUR: RECORD IF INSIDE OR OUTSIDE CIRCLE:
	
	for(int p=0; p<upperBound; p++ )
	{
		float dist1Sq = line_sqDistBetweenPts2D( center, getPt(cont,p) );
		ptInside[p]   = (dist1Sq <= radSq);
		if( ptInside[p] )
		{
			updateMin( minIdx, p );
			updateMax( maxIdx, p );
		}
	}
	
	minIdx = MAX( minIdx-1, 0 );
	maxIdx = MIN( maxIdx+1, psize(cont) );
	
	//## BETWEEN FIRST AND LAST POINT INSIDE CIRCLE: CHECK IF ANY PROCEEDING LINE SEGMENT
	//## INSIDE THE CIRCLE OVERLAPS WITH THIS LINE SEGMENT:
	
  for(int i=minIdx; i<maxIdx; i++ )
	{
		if( !ptInside[i] && !ptInside[i+1] )
			continue;
		
    for(int j=i+2; j<maxIdx; j++ )
		{
			if( !ptInside[j] && !ptInside[j+1] )
				continue;
						
      if(imodPointIntersect(getPt(cont,i),getPt(cont,i+1),getPt(cont,j),getPt(cont,j+1))
         && !( i == 0 && j == psize(cont)-1 ) )
        return false;
		}
	}
	
  return true;
}









//------------------------
//-- Executes a livewire operation (DM_LIVEWIRE) by first making sure livewire 
//-- objects are setup, then checking if the user is has selected a valid 
//-- point and is adding a livewire segment - in which case the segment
//-- is first added and possibly the contour finished/deselected.
//-- 
//-- If no segments were added, the agorithm checks if a new slice/area
//-- was clicked - and if the weighting over the currenly slice is calculated
//-- using the "plugs.weight" thread.
//-- 
//-- Regardless of wether segments were added, the last step is to execute
//-- livewire is over the weighted image, starting at the mouse position
//-- clicked, and using the "plug.livewire" thread and/or "plug.livewireF"
//-- thread, depending on wether "lwUseWrap" is on, and what livewire point
//-- the user may have just added (a first, last or intermediate livewire).

void edit_executeLivewireClick()
{
	//## IF NOT ALREADY, MAKE SURE LIVEWIRE IS SETUP:
	
	uint w = plug.xsize, h = plug.ysize;
	plug.window->initLivewire( plug.xsize, plug.ysize );
	
	
	
	//## DETERMINE LOCATION OF MOUSE CLICK INSIDE THE TOMOGRAM:
	
	Imod *imod  = ivwGetModel(plug.view);
	Icont *cont = imodContourGet(imod);
	Ipoint *pt  = imodPointGet(imod);
	int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
	int mX = (int)plug.mouse.x;		keepWithinRange( mX, 0, (int)w-1);
	int mY = (int)plug.mouse.y;		keepWithinRange( mY, 0, (int)h-1);
	int mZ = (int)plug.mouse.z;
	
	
	//## IF NO POINT SELECTED OR IS ON A DIFFERENT SLICE,
	//## ADD A NEW CONTOUR POINT AND START LIVEWIRE
	//## AT THIS NEW POSITION:
	
	bool startNew = (pt==NULL) || ( (int)imodPointGet(imod)->z != mZ);
		
  if( startNew )
	{
		imodContourClearPoints( plug.lwPts );
		imodPointAppendXYZ( plug.lwPts, (float)mX, (float)mY, (float)mZ );
		
		Icont *newCont = imodContourNew();
    imodPointAppendXYZ( newCont, (float)mX, (float)mY, (float)mZ );
    int newContPos = edit_addContourToObj( imodObjectGet(imod), newCont, true );
    imodSetIndex(imod, objIdx, newContPos, 0);
		
		undoFinishUnit( plug.view );          // FINISH UNDO
		
		edit_startLivewireFromPt( mX, mY, mZ, true );
		return;
	}
	
	
	//## DETERMINE WHERE SEGMENTS SHOULD BE ADDED TO
	//## CURRENTLY SELECTED CONTOUR:
	
	int numLivewirePts      = psize(plug.lwPts);
	bool firstSegment       = (numLivewirePts == 1);
	bool finishContour      = false;
	bool lastPtSelected     = (ptIdx >= psize(cont)-1);
	int ptsAdded = 0;
	QPoint qptEnd = QPoint( mX, mY );
	
	if( numLivewirePts >= 3 && lastPtSelected )
	{
		float distFirstLWPt = line_distBetweenPts2D( getFirstPt(plug.lwPts), &plug.mouse);
		float distLastLWPt  = line_distBetweenPts2D( getLastPt(plug.lwPts),  &plug.mouse);
		if( distFirstLWPt <= LW_SNAP_DIST || distLastLWPt <= LW_SNAP_DIST )
			finishContour = true;
	}
	
	imodPointAppendXYZ( plug.lwPts, (float)mX, (float)mY, (float)mZ );
	
	
	//## IF THE LAST CONTOUR POINT SELECTED ADD POINTS TO
	//## THE END FROM THE APPROPRIATE LIVEWIRE THREAD(S):
	
	if(lastPtSelected)
	{
		if( !firstSegment )
		{
			ptsAdded += edit_addLivewirePtsToCont( cont, -1, plug.livewire, qptEnd, mZ,
																						true, true );
		}
		if( firstSegment )
		{
			ptsAdded += edit_addLivewirePtsToCont( cont, -1, plug.livewireF, qptEnd, mZ,
																						true, true );
		}
		if( finishContour )
		{
			ptsAdded += edit_addLivewirePtsToCont( cont, -1, plug.livewireF, qptEnd, mZ,
																						true, false );
		}
	}
	
	//## ELSE ADD POINTS TO APPROPRIATE POSITION:
	
	else
	{
		int insertPt = ptIdx+1;
		bool reverse = true;
		
		//## IF NEW POINT IS CLOSE TO ORIGIONAL CONTOUR THEN
		//## DELETE POINTS IN BETWEEN:
		
		float dist = line_distBetweenPts2D( &plug.mouse, pt );
		if( psize(cont) > 10 && dist > 5.0f )
		{
			Ipoint closestPt;
			float closestDist;
			int closestPtIdx;
			cont_findClosestPtInContToGivenPt( &plug.mouse, cont, &closestDist,
																				 &closestPt, &closestPtIdx );
			
			int  ptDiff    = closestPtIdx-ptIdx;
			bool removePts = (closestDist <= LW_SNAP_DIST) && abs(ptDiff) >= 3;
			
			if(removePts)
			{
				int minIdx = MIN(ptIdx,closestPtIdx);
				int maxIdx = MAX(ptIdx,closestPtIdx);
				
				//cout << "DELETING BETWEEN " << minIdx << " and " << maxIdx << endl;
				reverse = (ptDiff > 0);
				insertPt = minIdx+1;
				
				undoContourDataChgCC( plug.view );            // REGISTER UNDO
				for(int i=(maxIdx-1); i>minIdx; i--)
					imodPointDelete(cont, i);
			}
		}
		
		ptsAdded += edit_addLivewirePtsToCont( cont, insertPt, plug.livewire, qptEnd, mZ,
																					 true, reverse );
	}
	
	//## MAKE SURE THE LAST POINT FROM NEWLY ADDED 
	//## POINTS IS SELECTED:
	
	imodSetIndex(imod, objIdx, contIdx, ptIdx+ptsAdded);
	undoFinishUnit( plug.view );          // FINISH UNDO
	
	
	//## IF CONTOUR NOW FINISHED, APPLY SMOOTING 
	//## (IF NECESSARY) AND DESELECT ELSE
	//## START LIVEWIRE FROM NEW POINT:
	
	if(finishContour)
		edit_finishLivewireOnCurrCont( true );
	else 
		edit_startLivewireFromPt( mX, mY, mZ, false );
}


//------------------------
//-- Finishes livewire on the current contour by applying
//-- smoothing (if specified) and deselecting the
//-- contour if "deselect" is true.


bool edit_finishLivewireOnCurrCont(bool deselect)
{
	Imod *imod  = ivwGetModel(plug.view);
	Iobj  *obj  = imodObjectGet(imod);
	Icont *cont = imodContourGet(imod);
	
	if(cont==NULL)
		return false;
	
	//## IF SPECIFIED, SMOOTH THE CONTOUR:
	
	if (plug.lwSmooth)
	{
		undoContourDataChgCC( plug.view );        // REGISTER UNDO
		bool contClosed = isContClosed(obj,cont);
		
		for(int i=0; i<plug.lwSmoothIts; i++)
		{
			cont_reducePtsMinArea( cont, 1.0f, contClosed );
			cont_avgPtsPos( cont, plug.smoothMoveFract,
										 plug.smoothMoveMinDist, contClosed, true );
		}
		
		undoFinishUnit( plug.view );          // FINISH UNDO
	}
	
	//## IF SPECIFIED, DESELECT THE CURRENT CONTOUR:
	
	if(deselect)
	{
		int objIdx, contIdx, ptIdx;
		imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
		imodSetIndex(imod, objIdx, -1, -1);
	}
	
	return true;
}





//------------------------
//-- This function is used to insert or append livewire points to
//-- a given contour ("cont"). The livewire points start at the origion
//-- of the given livewire thread ("livewire") and extends to the designated
//-- end point ("qptEnd") via the lowest weight path. If the livewire thread
//-- has not calculated this far yet, no points are added. If a path
//-- is found, points are inserted to the contour starting at the given
//-- point index ("startIdx").
//-- 
//-- If you wish to add points to the end (which is usually the case)
//-- you can set "startIdx" to -1. If "startIdx" is -1 (or any other invalid
//-- index for that matter), points are appended rather than inserted. The
//-- input value "z" specifies what slice to add point, "reverse" can be used
//-- to reverse the order which points are added, and "addUndo" should be
//-- called if making a change to a model contour (as opposed to an extra
//-- contour) that the user may wish to undo.

int edit_addLivewirePtsToCont( Icont *cont, int startIdx,
															 Livewire::LivewireCalculator *livewire,
															 QPoint qptEnd, int z, bool addUndo, bool reverse )
{
	int ptsAdded = 0;		// tracks how many new points we add
	Ipoint newPt;				// used to store each new point we add
	
	bool append = (startIdx < 0) || (startIdx >= psize(cont));
	
	
	//## IF WEIGHTS HAS FINISHED, GET LIVEWIRE POINTS FROM THE INPUT
	//## LIVEWIRECALCULATOR TO THE DEFINED END POINT:
	
	
	if( plug.weights!=NULL && livewire!=NULL )
	{
		QVector<QPoint> qpts = livewire->GetTrace(qptEnd.x(),qptEnd.y());	
																														// get livewire line
		//cout<< "qpts.size = " << qpts.size()  << endl;
		
		//## FOR EACH LIVEWIRE POINT, ADD IT TO THE SPECIFIED LOCATION IN THE CONTOUR:
		
		int nLWPts = (int)qpts.size();
		
		if( nLWPts >= 2 )
		{
			for( int p=(nLWPts-1); p>0; p-- )
			{
				int idx = (reverse) ? p : nLWPts-1-p;
				setPt( &newPt, (float)qpts[idx].x(), (float)qpts[idx].y(), z );
				
				if( append )
				{
					if(addUndo)
						undoPointAdditionCC( plug.view, psize(cont) );			// REGISTER UNDO
					imodPointAppend( cont, &newPt );
				}
				else
				{
					if(addUndo)
						undoPointAdditionCC( plug.view, startIdx+ptsAdded );        // REGISTER UNDO
					imodPointAdd( cont, &newPt, startIdx+ptsAdded );
				}
				ptsAdded++;
			}
		}
	}
	
	//## IF NO POINTS WERE ADDED, OR THE LAST POINT ADDED WAS NOT THE 
	//## DEFINED END POINT, ADD THE END POINT:
	
	if( ptsAdded==0 )
	{
		if( append )
		{
			if(addUndo)
				undoPointAdditionCC( plug.view, psize(cont) );			// REGISTER UNDO
			imodPointAppend( cont, &newPt );
			ptsAdded++;
		}
		else
		{
			if(addUndo)
				undoPointAdditionCC( plug.view, startIdx+ptsAdded );        // REGISTER UNDO
			imodPointAdd( cont, &newPt, startIdx+ptsAdded );
			ptsAdded++;
		}
	}
	
	return ptsAdded;
}


//------------------------
//-- Starts a new livewire thread starting from the given coordinates (x,y,z)
//-- using either "plug.livewire" or "plug.livewireF", depending on the
//-- value of "useLivewireF". Importantly, before the livewire thread is
//-- started, "plug.weights" is first checked to see if it has calculated
//-- weights over this XY area and on the same slice "plug.lwWeightZVal". 
//-- If the point is in a new area or slice, then "plug.weights" is 
//-- started over this area and the livewire thread waits until this
//-- finishes to commence. Note that both "plug.livewire" and "plug.livewireF"
//-- rely on "plug.weights" to finish before they can begin.

bool edit_startLivewireFromPt( int x, int y, int z, bool useLivewireF )
{
	//## MAKE SURE POINT IS WITHIN IMAGE BOUNDARIES:
	
	if( x < 0 ) x = 0;		if( x >= plug.xsize ) x = plug.xsize-1;
	if( y < 0 ) y = 0;		if( y >= plug.ysize ) y = plug.ysize-1;
	if( z < 0 ) z = 0;		if( z >= plug.zsize ) z = plug.zsize-1;
	
	//## IF THE SLICE CLICKED IS NOT THE SLICE IN "plug.weight",
	//## USE THIS THREAD TO CALCULATE THE WEIGHT OF THIS SLICE:
	
	if( plug.lwWeightZVal != z )
	{
		edit_setLivewireImage( z );
	}
	
	
	int area = 2048;
	
	switch(plug.lwAreaSize)
	{
		// half of displayed size because that is what LivewireCalculator needs
		case 0:				area = 256;  break;
		case 1:				area = 512;  break;
		case 2:			  area = 1024; break;
		case 3:				area = 2048; break;
		case 4:				area = 4096; break;
	}
	
	//## RESTART LIVEWIRE THREAD FROM POINT CLICKED USING
	//## EITHER "plug.livewireF" OR "plug.livewire": 
	
	if( useLivewireF )
	{
		plug.livewireF->Start(x,y, area);
	}
	else
	{
		plug.livewire->Start(x,y, area);
	}
	
	return true;
}


//------------------------
//-- Sets or resets the image used by "plug.weights" to the specified
//-- slice. This function should be called whenever a new image/slice
//-- is clicked, or when the weight settings are changed.

bool edit_setLivewireImage( int z )
{	
	if( z < 0 )
		z = 0;
	plug.lwWeightZVal = z;
	unsigned char **data = ivwGetCurrentZSection( plug.view );
	int mode = ivwGetImageStoreMode( plug.view );
	
	Livewire::WeightCalculator::DataFormat format;
	if     (mode==0)								format = Livewire::WeightCalculator::GrayscaleByte;
	else if(mode==MRC_MODE_USHORT)	format = Livewire::WeightCalculator::GrayscaleUShort;
	else														format = Livewire::WeightCalculator::RGB;
	
	uint stride = data[1] - data[0];
	plug.weights->SetImage(data[0], format, stride);
}


//------------------------
//-- Executes a "livewire select" whereby the user pushes but2
//-- (the middle mouse for me) to select a point, and we want 
//-- livewire generated between the closest livewire points.
//-- To determine if there are livewire point, this function
//-- calls "edit_getNextAndPrevLivewirePts()" and if no livewire
//-- points are found, livewire just begins on the selected point.
//-- Returns true if livewire is started, or false if no point
//-- was/is selected and thus livewire is not restarted.

bool edit_executeLivewireSelectPt()
{
	//## SELECT ANY VISIBLE POINT NEAR WHERE THE USER CLICKED
	//## OR RETURN FALSE IF NO NEARBY POINT FOUND
	
	bool newPtFound = edit_selectVisiblePtNearCoords( &plug.mouse, 20.0f );
	//cout << "newPtFound = " << newPtFound << endl;
	if( !newPtFound )
		return false;
	
	
	//## IF NOT ALREADY, MAKE SURE LIVEWIRE IS SETUP:
	
	uint w = plug.xsize, h = plug.ysize;
	plug.window->initLivewire( plug.xsize, plug.ysize );
	
	
	//## IF ALLOW LIVEWIRE MODIFY IF OFF START LIVEWIRE FROM
	//## NEW POINT:
	
	
	Imod   *imod   = ivwGetModel(plug.view);
	Ipoint *currPt = imodPointGet(imod);
	if( currPt!=NULL )
	{
		edit_startLivewireFromPt( (int)currPt->x, (int)currPt->y, (int)currPt->z, false );
		return true;
	}
	
	
	//## DETERMINE IF SELECTED POINT BELONGS TO A CONTOUR WITH ACTIVE 
	//## LIVEWIRE POINTS AND, IF SO, THEN BEGIN LIVEWIRE FROM THE 
	//## CLOSEST LIVEWIRE POINTS BEFORE AND/OR AFTER THAT POINT:
	//
	//int prevLWPt, nextLWPt;
	//bool isPtOnLivewireCont = edit_getNextAndPrevLivewirePts( prevLWPt, nextLWPt );
	//
	//Imod   *imod   = ivwGetModel(plug.view);
	//Icont  *cont   = imodContourGet(imod);
	//Ipoint *currPt = imodPointGet(imod);
	//
	//if( cont == NULL || currPt == NULL )
	//	return (false);
	//
	//if( isPtOnLivewireCont )
	//{
	//	if( prevLWPt != -1 )
	//	{
	//		Ipoint *prevPt = getPt( cont, prevLWPt );
	//		edit_startLivewireFromPt( (int)prevPt->x, (int)prevPt->y, (int)prevPt->z, false );
	//	}
	//	if( nextLWPt != -1 )
	//	{
	//		Ipoint *nextPt = getPt( cont, nextLWPt );
	//		edit_startLivewireFromPt( (int)nextPt->x, (int)nextPt->y, (int)nextPt->z, true );
	//	}
	//}
	//else {
	//	edit_startLivewireFromPt( (int)currPt->x, (int)currPt->y, (int)currPt->z, false );
	//}
	//
	//return true;
}



//------------------------
//-- Determines if the contours belonging to the currently selected
//-- contour has livewire points. If so, it will return true and the
//-- value of "prevLWPt" and "nextLWPt" set the the point index
//-- in the current contour of the closest livewire point before and
//-- after the currently selected point.
//-- If there is no livewire point before or after, these values
//-- get set to -1, and if there are no livewire points at all
//-- (or no valid point selected) then false is returned.
/*
bool edit_getNextAndPrevLivewirePts( int &prevLWPt, int &nextLWPt )
{
	//## IF NOT ALREADY, MAKE SURE LIVEWIRE IS SETUP:
	
	uint w = plug.xsize, h = plug.ysize;
	plug.window->initLivewire( plug.xsize, plug.ysize );
	
	
	//## IF NO POINT SELECTED, EXIT EARLY:
	
	prevLWPt = -1;
	nextLWPt = -1;
	
	Imod *imod     = ivwGetModel(plug.view);
	Icont *cont    = imodContourGet(imod);
	Ipoint *currPt = imodPointGet(imod);
	
	if( currPt == NULL )
		return (false);
	
	//## IF SELECTED POINT ON DIFFERENT SLICE THAN LIVEWIRE POINTS 
	//## (plug.lwPts), START LIVEWIRE FROM SELECTED POINT:
	
	Ipoint currPtRounded;
	setPt( &currPtRounded, (int)currPt->x, (int)currPt->y, (int)currPt->z );
	
	if( plug.lwPts == NULL || getZInt(plug.lwPts) != (int)currPt->z
		  || psize(plug.lwPts) <= 0 || psize(cont) <= 1 )
	{
		return false;
	}
	
	
	//## CHECK IF THE CURRENTLY SELECTED PT IS ON LIVEWIRE CONTOUR:
	
	int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
	bool currPtOnLivewirePt   = false;
	bool currPtOnLivewireCont = false;
	
	
	for( int p=ptIdx; p>=0; p-- )								// for each point before current point:
	{
		if( cont_doesPtExistInCont( plug.lwPts, getPt(cont,p) ) )		// if livewire pt:
		{
			prevLWPt = p;																										// record position
			break;
		}
	}
	
	for( int p=ptIdx+1; p<psize(cont); p++ )		// for each point after current point:
	{
		if( cont_doesPtExistInCont( plug.lwPts, getPt(cont,p) ) )		// if livewire pt:
		{
			nextLWPt = p;																										// record position
			break;
		}
	}
	
	return ( prevLWPt!=-1 || nextLWPt!=0 );
}*/












//------------------------
//-- Executes a wand operation (DM_WAND) by adding a new contour
//-- around the area the user has just dragged out.

void edit_executeWandAdd()
{
	//## DETERMINE CONTOUR AROUND WAND AREA:
	
	int radiusInt   = (int)plug.sculptRadius;
	float tolerance = line_distBetweenPts2D( &plug.mouseDownPt, &plug.mouse );
	
	Icont *xcontP   = imodContourNew();    // used to draw points in wand area
	Icont *newCont  = imodContourNew();		 // used to draw contour around area
	
	edit_addWandPtsToCont( xcontP, plug.mouseDownPt, radiusInt,
												plug.wandAvgGrayVal, tolerance, plug.waDistBias, true );
	edit_scanContShrink( xcontP, radiusInt, 5, true );
	edit_scanContFill( xcontP, newCont, plug.mouseDownPt, radiusInt, 6 );
		
	free(xcontP);
	
	//## IF CONTOUR HAS NO POINT: DO NOTHING
	
	if( psize(newCont)==0 )
	{
		free(newCont);
		return;
	}
	
	
	//## ADD THE NEW CONTOUR:
	
	Imod *imod  = ivwGetModel(plug.view);
	int objIdx, contIdx, ptIdx;
  imodGetIndex(imod, &objIdx, &contIdx, &ptIdx);
	
	int newContPos = edit_addContourToObj( imodObjectGet(imod), newCont, true );
	imodSetIndex(imod, objIdx, newContPos, 0);
	
	undoFinishUnit( plug.view );          // FINISH UNDO
	
	
	//## IF SPECIFIED, SMOOTH THE CONTOUR:
	
	if (plug.waSmooth)
	{
		undoContourDataChgCC( plug.view );        // REGISTER UNDO
		
		Icont *cont = getCurrCont();
		Iobj  *obj  = getCurrObj();
		bool contClosed = isContClosed(obj,cont);
		
		for(int i=0; i<plug.waSmoothIts; i++)
		{
			cont_reducePtsMinArea( cont, 1.0f, contClosed );
			cont_avgPtsPos( cont, plug.smoothMoveFract,
											plug.smoothMoveMinDist, contClosed, true );
		}
		undoFinishUnit( plug.view );        // FINISH UNDO
	}
}


//------------------------
//-- This function represents a crude "adaptive flood fill algorithm"
//-- used to determine which image pixels are within "radius" distance
//-- of the given center point ("centerPt") and with have a gray value
//-- within the specified "threshold" of the given target value "targetGray".
//-- The function returns the number of points which meet this criteria
//-- and appends these points to the given contour "cont" in a
//-- "scan line" order (left to right, top to bottom).
//-- 
//-- The "distBiasCoefficient" is a value which can be increased from 
//-- zero in order to make points less likely to be selected the
//-- further they are from the center point. Using a value of 0.5,
//-- a pixel which is half the radius away but exactly the same gray
//-- value as "targetGray" still needs the tolerance to be > 64 to
//-- be turned on (0.5*0.5*255=64).
//-- 
//-- If "allPts" is true, points fail the test are also appended to the 
//-- contour but are given a z value of "PIX_OFF" (-1). Since pixels are 
//-- assessed in a minimum square around the boundary, if "allPts" is true 
//-- the total points in the contour (assuming it started empty) will be
//-- equal to ((2*radius+1)^2), and a point at x,y relative to the bottom
//-- of the bounding square will be at "getPt(cont, y*(2*radius+1)+x )" -
//-- which is useful in other functions.

int edit_addWandPtsToCont( Icont *cont, Ipoint centerPt, int radius,
													 float targetGray, float tolerance,
													 float distBiasCoefficient, bool allPts )
{
	//## DETEMINE A BOUNDING SQUARE AROUND THE SPECIFIED WAND CIRCLE:
	
	if( cont==NULL || radius<=0 )
	{
		cerr << "edit_addWandPtsToCont() - invalid contour or radius" << endl;
		return 0;
	}
	setPt(&centerPt, floor(centerPt.x), floor(centerPt.y), floor(centerPt.z) );
	
	int ptsOn = 0;			// tracks how many new points/pixels are "on"
	Ipoint pt;					// used to add new points
	
	int minX = (int)centerPt.x - radius;		// |-- represents a bounding square around the
	int maxX = (int)centerPt.x + radius;		// |   bounding circle - all pixels in this
	int minY = (int)centerPt.y - radius;		// |   square are tested if they are inside
	int maxY = (int)centerPt.y + radius;		// |   the circle and grayscale threshold
	
	unsigned char **data = ivwGetCurrentZSection( plug.view );
	
	float distBias = distBiasCoefficient * 255.0f;		// used to bias points furt
	
	
	//## WITHIN THE DESIGNATED CIRCULAR REGION, CHECK ALL PIXELS:
	
	for(int y=minY; y<=maxY; y++)
	for(int x=minX; x<=maxX; x++)
	{
		//## CHECK IF POINT IS OUTSIDE TOMOGRAM:
		
		if( y<0 || y >= plug.ysize || x<0 || x >= plug.xsize )
		{
			if(allPts)
				imodPointAppendXYZ( cont, (float)x,(float)y, PIX_OFF );
			continue;
		}
		
		
		//## DETERMING IF POINT IS OUTSIDE WAND CIRCLE:
		
		setPt( &pt, x, y, centerPt.z );
		bool pointAdded = false;
		
		float dist = line_distBetweenPts2D( &centerPt, &pt );
		
		if( dist < radius )			// if point is inside wand circle:
		{
			int grayValue    = data[y][x];									// determine it's gray value
			float distOffset = distBias*(dist / radius);		// an offset bias bad on distance 
			
			int grayValueDiff = ABS( targetGray - grayValue ) + distOffset;
			
			if( grayValueDiff <= tolerance )		// if within tolerance value:
			{
				imodPointAppendXYZ( cont, (float)x,(float)y,centerPt.z );		// add point
				pointAdded = true;
				ptsOn++;
			}
		}
		
		//## IF POINT DIDN'T MEET CRITERIA, APPEND POINT AT "PIX_OFF" 
		//## Z VALUE (IF THIS OPTION IS TURNED ON)
		
		if( !pointAdded && allPts )
			imodPointAppendXYZ( cont, (float)x,(float)y, PIX_OFF );
	}
	
	return ptsOn;
}



//------------------------
//-- Treats the contour like an image and uses "erosion" to help remove isolated
//-- points. This function is similar to "autox_shrink()" in "3dmod/auotx.cpp",
//-- however the intput/output in the form of a single "scan line" contour in a
//-- perfect square with side length "side" equal to "2*radius+1". Each point
//-- should be locatable by "getPt( cont, y*side+x)", and points with a z value
//-- equal to "PIX_OFF" (-1) are considered off - all other contours are considered
//-- on. This function goes through the specified number of "iterations" and
//-- points which don't have the required number of pixels on within their
//-- 4 AND 8 pixel neighborhood are eroded by changing their z value to PIX_OFF.
//-- In addition to changing the input contour ("cont") the function returns
//-- the number of pixels changed/eroded.
//-- 
//-- To reduce processing time, points on the edge are not processed, and if
//-- "erodeAllEdgePts" is true then all edge points are turned off, but not
//-- included in the final returned value.

int edit_scanContShrink( Icont *cont, int radius, int iterations, bool erodeAllEdgePts )
{
	//## CHECK CONTOUR IS CORRECT SIZE TO MATCH BOUNDING SQUARE:
	
	int ptsEroded = 0;								// tracks the number of points changed / eroded
	int nPts      = psize(cont);
	int side = (radius * 2) + 1;			// the width and height of the bounding square
	
	if( nPts != (side*side)  )
	{
		cerr << "edit_scanContShrink() - wrong size" << endl;
		return 0;
	}
	
	
	//## IF SPECIFIED: TURN OFF ALL POINTS AROUND THE EDGES OF THE BOUNDING SQUARE
	
	if(erodeAllEdgePts)
	{	
		int ptIdxTopRow = (side-1) * side;
		for(int i=0; i<side; i++)
		{
			getPt( cont, 0 +      i             )->z = -1;		// bottom-most row
			getPt( cont, 0 +      i+ptIdxTopRow )->z = -1;		// top-most    row
			getPt( cont, i*side + 0             )->z = -1;		// left-most   column
			getPt( cont, i*side + side-1        )->z = -1;		// right-most  column
		}
	}
	
	
	//## RUN ITERATIONS OF ERODE, WHERE EACH PIXELS WHICH IS ON
	//## (EXLUDING PIXELS ON THE EDGES) IS ERODED IF TOO FEW
	//## OF IT'S NEIGHBORS ARE ON:
	
	int pixN[8];						// used to store on/off values for the 8 pixel neighborhood
	
	for( int i=0; i<iterations; i++ )
	{
		for(int y=1; y<side-1; y++)
		for(int x=1; x<side-1; x++)
		{
			int ptIdx = y*side + x;
			
			Ipoint *pt = getPt( cont, ptIdx );
			if( pt->z == PIX_OFF)								// if pixel already off:
				continue;														// skip it
			
			edit_getPixNeigh( cont, pixN, x, y, side, side );		// outputs which pixels in the
																													// 8 pixel neighborhood are on
																													// into the "pixN" array
			
			int totalOnIn4Neigh = pixN[PX_N ] + pixN[PX_S ] + pixN[PX_E ] + pixN[PX_W ];
			int totalOnIn8Neigh = pixN[PX_NE] + pixN[PX_NW] + pixN[PX_SE] + pixN[PX_SW]
														+ totalOnIn4Neigh;
									// tally the number of pixels on in the 4 and 8 pixel neighborhood
			
			if( totalOnIn8Neigh <= 3 || totalOnIn4Neigh <=1 )		// if too few neighbors on:
				pt->z = -2;																					// flag pt for erosion
		}
		
		for(int i=0; i<nPts; i++)		// for each point in contour:
		{
			if( getPt(cont,i)->z == -2 )		// if flagged for erosion
			{
				getPt(cont,i)->z = PIX_OFF;			// mark pixel as off
				ptsEroded++;
			}
		}
	}
	
	//cout << "ptEroded=" << ptEroded << endl;
	return (ptsEroded);
}



//------------------------
//-- Inputs a contour ("cont") which is in the form of a square scanline,
//-- and uses a marching square like algorithm to generate and output
//-- a single contour ("contOut") representing the path around the middle-most
//-- area of the scanline contour with points that are "on".
//-- 
//-- This function is similar to "autoxFill()" in "3dmod/auotx.cpp", except
//-- the intput/output in the form of a single "scan line" contour in a
//-- perfect square with side length "side" equal to "2*radius+1". Each point
//-- should be locatable by "getPt( cont, y*side+x)", and points with a z value
//-- equal to "PIX_OFF" (-1) are considered off - all other contours are considered
//-- on.
//--
//-- In addition to the output contour, the function also returns the number
//-- of points added to this contour. If however, the resulting contour has
//-- less than a specified minimum ("minPts") number of points, the output
//-- contour is made empty and the function returns 0.
//-- 
//-- This function could use improvements. For example, when trying to decide 
//-- a starting points for the output contour it tries to find the first
//-- point right of "centerPt" which is off, and thus sometimes finds a hole.


int edit_scanContFill( Icont *cont, Icont *contOut,
											Ipoint centerPt, int radius, int minPts )
{
	//## CHECK CONTOUR IS CORRECT SIZE TO MATCH BOUNDING SQUARE:
	
	int ptRemoved = 0;
	int nPts      = psize(cont);
	int side = (radius * 2) + 1;
	
	if( nPts != (side*side)  )
	{
		cerr << "edit_scanContFill() - wrong size" << endl;
		return 0;
	}
	setPt(&centerPt, floor(centerPt.x), floor(centerPt.y), floor(centerPt.z) );
	
	
	//## USING DIFFERENT STARTING POINTS, TRY AND GENERATE A CONTOUR
	//## AROUND AN AREA OF PIXELS UNTIL AND AREA IS GENERATED
	//## THAT INCLUDES THE ORIGIONAL ON POINT:
	
	
	float xOffset = float(centerPt.x-radius)+0.5f;		// |-- represents the bottom corner
	float yOffset = float(centerPt.y-radius)+0.5f;		// |   of the bounding square
	
	bool endFound       = false;	// set true if algorithm finds a path back to the start 
	bool isCenterInside = false;	// set true if "contOut" includes "centerPt"
	
	int ptStartX  = radius;				// |-- represents the point to start searching for
	int ptStartY  = radius;				// |   the first pixel which is off
	
	for (int attempt=0; attempt<5 && (!isCenterInside); attempt++ )
	{
		int x = ptStartX;
		int y = ptStartY;
		
		//## IF THIS IS NOT THE FIRST PASS, CLEAR "contOut" AND ITERATE
		//## "ptStartX" RIGHT UNTIL FINDING THE FIRST PIXEL WHICH IS ON:
		
		if(attempt>0)
		{
			imodContourClearPoints( contOut );
			
			for(x = ptStartX+1; x<side; x++)
			{
				Ipoint *pt = getPt( cont, y*side + x );
				if( pt->z != PIX_OFF )
					break;
			}
			if(x==side)		return 0;
			else					ptStartX = x;
		}
		
		//## START AT STARTING POINT AND ITERATE RIGHT UNTIL FINDING
		//## THE FIRST PIXEL WHICH IS OFF:
		
		for(; x<side; x++)
		{
			Ipoint *pt = getPt( cont, y*side + x );
			if( pt->z == PIX_OFF )
				break;
		}
		x--;
		ptStartX  = x;
		
		//## USING THE STARTING POINT ABOVE, APPLY A MARCHING-SQUARES
		//## LIKE ALGORITHM TO GO ANTI-CLOCKWISE AROUND THE OUTER EDGE OF 
		//## OF THE ON AREA, AND OUTPUT THESE EDGE POINTS TO "contOut".
		
		int pixN[8];								// used to store on/off values for the 8 pixel neighborhood
		int endFound = false;				// set true if the algorithm finds a path back to the start 
		int lastMoveDir = PX_N;			// tracks the direction of the last move
		
		
		for(int i=0; i<(side*40) && (!endFound); i++)
		{
			//## ADD THIS POINT TO "contOut" AND GET IT'S 8 PIXEL NEIGHBORHOOD VALUES:
			
			imodPointAppendXYZ(contOut, float(x) + xOffset, float(y) + yOffset, centerPt.z);
			edit_getPixNeigh( cont, pixN, x, y, side, side );
			
			
			//## SEARCH AROUND THE CURRENT POINT'S NEIGHBORHOOD IN ANTI-CLOCKWISE ORDER 
			//## FOR THE FIRST POINT THAT IS ON:
			
			int startDir = (lastMoveDir-2+8)%8;		// direction from the current pt to check first
			int currDir;
			
			for(int i=0; i<8; i+=1)
			{
				currDir = (startDir+i)%8;
				if( pixN[currDir]==1 )
					break;
			}
			
			//## UPDATE THE X, Y AND DIRECTION VALUES TO USE FOR THE NEXT POINT:
			
			switch( currDir )
			{
				case(PX_N):		{	y += 1;							break;		}
				case(PX_S):		{	y -= 1;							break;		}
				case(PX_E):		{						x	+= 1;		break;		}
				case(PX_W):		{						x -= 1;		break;		}
				case(PX_NE):	{	y += 1;		x += 1;		break;		}
				case(PX_SE):	{	y -= 1;		x += 1;		break;		}
				case(PX_NW):	{	y += 1;		x -= 1;		break;		}
				case(PX_SW):	{	y -= 1;		x -= 1;		break;		}
			}
			lastMoveDir = currDir;
			
			//## IF THE NEXT POINT IS THE FIRST, THEN FINISH THE CONTOUR:
			
			endFound = ( x==ptStartX && y==ptStartY );
			if( endFound )
				break;
		}
		
		//## CHECK IF THE GENERATED CONTOUR CONTAINS THE POINT
		//## IN THE MIDDLE OF THE BOUNDING SQUARE: IF NOT TRY
		//## A NEW STARTING POINT FOR THIS ALGORITHM
		
		isCenterInside = imodPointInsideCont( contOut, &centerPt );
		if( isCenterInside )
			break;
	}
	
	
	
	//## CHECK RESULTS AND OUTPUT ERROR MESSAGE IF APPLICABLE:
	
	bool decentRadius = (ptStartX>radius+10);	// was last start point resonable size?
	
	if( psize(contOut) < minPts )
		imodContourClearPoints( contOut );
	
	int ptsAdded = psize(contOut);
	//cout << "ptsAdded=" << ptsAdded << endl; flush(cout);
	return (ptsAdded);
}








//------------------------
//-- Takes an input contour ("cont") and uses an input circle described
//-- by "centerPt" and "radius", by removing points in the circle.
//-- If the circle divides the contour into multiple segments, then
//-- only the biggest segment and/or the segment containing the specified
//-- "includePt" is remains.
//-- 
//-- If the remaining contour has less than a specified minimum of points
//-- "minPts", no change is made to the contour. This function will return
//-- the number of points in the new contour, or zero if no change was made
//-- to the contour.
//--
//-- NOTE: This tool is useful for removing automatic unwanted regions from 
//-- a contour which are often added by segmentation algorithms (undersegmentation).

int edit_contCorrectCircle( Icont *cont, Ipoint centerPt, int radius, int minPts,
													  Ipoint includePt )
{
	//## LOCATE THE POINT CLOSEST TO THE CENTER OF THE CIRCLE:
	
	float radSq = (radius*radius);
	
	int closestPtIdxInside  = -1;
	float minDistSq = radSq;
	
	for( int p=0; p<psize(cont); p++ )
	{
		float dist1Sq = line_sqDistBetweenPts2D( &centerPt, getPt(cont,p) );
		if( dist1Sq <= minDistSq )
		{
			minDistSq = dist1Sq;
			closestPtIdxInside = p;
		}
	}
	
	//## EXIT EARLY IF NOT ENOUGH POINTS OR ALL POINT ARE OUTSIDE CIRCLE
	//## ELSE REORDER CONTOUR TO START AT MIDDLE-MOST POINT:
	
	if( closestPtIdxInside==-1 || psize(cont) < minPts )
		return 0;
	
	cont_reorderPtsToStartAtIdx( cont, closestPtIdxInside );
	
	
	
	//## SPLIT CONTOUR INTO SEGMENTS USING THE CIRCLE TO DELETE 
	//## POINTS AND THEN LEAVE ONLY THE SEGMENT WHICH HAS THE MOST 
	//## POINTS AND/OR CONTAINS THE LAST MOUSE DOWN POSITION
	
	
	vector<IcontPtr> contSegments;
	cont_breakContByCircle(cont,contSegments,&centerPt,radius);
	
	if( contSegments.size() == 0 )
	{
		deleteContours(contSegments);
		return 0;
	}
	
	int maxPts = 0;
	int idxLongestSeg = 0;
	for( int i=0; i<contSegments.size(); i++ )
	{
		int numPtsFragment = psize( contSegments[i].cont );
		if ( imodPointInsideCont( contSegments[i].cont, &includePt ) )
			numPtsFragment *= 5;
		
		if( numPtsFragment > maxPts )
		{
			maxPts = numPtsFragment;
			idxLongestSeg = i;
		}
	}
	
	
	//## IF LARGEST SEGMENT DOESN'T HAVE ENOUGH POINTS THEN EXIT,
	//## OTHERWISE COPY THE SEGMENT POINTS INTO CONTOUR FOR OUTPUT:
	
	if( maxPts < minPts )
	{
		return 0;
		deleteContours(contSegments);
	}
	
	cont_copyPts( contSegments[idxLongestSeg].cont, cont, true );
	deleteContours(contSegments);
	
	return psize(cont);
}

