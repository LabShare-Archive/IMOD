//Added by qt3to4:
#include <QKeyEvent>
#include <QEvent>
#include <QMouseEvent>
/*   imodplug.h  -  public declarations for imodplug.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMODPLUG_H
#define IMODPLUG_H

class QKeyEvent;
class QMouseEvent;
class QStringList;
class QEvent;
#ifndef IMODP_H
typedef struct ViewInfo ImodView;
#endif

#ifndef IMODPLUGP_H
#include "imodplugP.h"
#endif

/* Define macro for export of functions under Windows */
#ifdef _WIN32
#define PLUG_EXPORT _declspec(dllexport)
#else
#define PLUG_EXPORT
#endif

extern "C" {

/*************************** Setup Functions *********************************/

/*
 * Generic Plugin interface.
 * 3dmod will call these functions at well defined times.
 */
/*!
 * Function to be defined by all plugins.  ^
 * Returns the name of the 
 * plugin, which will be inserted in the Special menu and used to send 
 * imodsendevent messages by name.  Bit flags made by OR'ing IMOD_PLUG_ values
 * for the type of plugin are returned in [type].  ^
 * Not all of 3dmod's data structures may be initialized at the time
 * this function is called, so no initialization should be done.  However,
 * an image loading plugin can call iiAddCheckFunction at this time.
 */
char PLUG_EXPORT *imodPlugInfo(int *type);

/*!
 * Menu execution function for plugins with the IMOD_PLUG_MENU bit set.  ^
 * [vw] is the pointer to the view structure, which should be stored and used
 * when calling back into the program.
 * imodPlugExecute will be called if available; if not defined then 
 * the following call will be made: ^
 * imodPlugExecuteType(inView, IMOD_PLUG_MENU, IMOD_REASON_EXECUTE);
 */
  void PLUG_EXPORT imodPlugExecute(ImodView *vw);
  
/*!
 * Function for notification that 3dmod initialization is complete (with 
 * [inType] equal to 0 and [inReason] equal to IMOD_REASON_STARTUP), and for
 * need to update for a change in model (with [inType] equal to 0 and
 * [inReason] equal to IMOD_REASON_MODUPDATE). It is also an alternate 
 * function for opening through the menu.  
 */
void PLUG_EXPORT imodPlugExecuteType(ImodView *inView, int inType, 
                                   int inReason);

/*!
 * Key input callback function to be defined by plugins with the
 * IMOD_PLUG_KEYS bit set.  ^
 * This function can be used to override 3dmod hot key settings; it is called
 * by image windows before they process a key event.
 * A nonzero return value indicates that the plugin handled the input key.
 * and that no other action should be taken by the 3dmod program.
 * A zero return value indicates that 3dmod should process the key as usual.
 */
int PLUG_EXPORT imodPlugKeys(ImodView *vw, QKeyEvent *event);

/*!
 * Mouse event callback function to be defined by plugins with the
 * IMOD_PLUG_MOUSE bit set.  ^
 * This function can be used to override 3dmod mouse actions in the Zap 
 * window.  [imx] and [imy] will contain the image position, and [but1], 
 * [but2], and [but3] will indicate the state of the 3 buttons as mapped by 
 * user preferences.  The return value should be the sum of two values: 
 * ^  1 if the plugin handled the mouse event, indicating that no other action
 * should be taken with the event itself by the 3dmod program.
 * ^  2 if the specific calling window should draw itself, without issuing a
 * general program redraw.  If this is not sufficient, the plugin should call
 * ivwRedraw instead of setting this bit.
 * ^  A zero return value indicates that 3dmod should process the event as 
 *usual.
 * ^This function is called only when a mouse button is down, unless mouse
 * tracking is enabled with ivwTrackMouseForPlugs.
 */
int PLUG_EXPORT imodPlugMouse(ImodView *vw, QMouseEvent *event, float imx,
                              float imy, int but1, int but2, int but3);

/*!
 * General event callback function to be defined by plugins with the
 * IMOD_PLUG_EVENT bit set.  ^
 * This function can be used to receive notification of selected events from a
 * Zap window, currently QEvent::Enter, QEvent::Leave, and QEvent::Wheel (which
 * can be cast to QWheelEvent). [imx] and [imy] will contain the image position
 * of the mouse at the time of the event.  The return value should be the sum 
 * of 1 and 2 as for imodPlugMouse, or 0 if the event should be processed as 
 * usual.  It should not be necessary to process Enter and Leave events
 * since the Zap window uses them to manage the removal of a cursor-like extra 
 * object created by a plugin.
 */
int PLUG_EXPORT imodPlugEvent(ImodView *vw, QEvent *event, float imx,
                              float imy);
/*!
 * Function to execute a message, to be defined by plugins with the
 * IMOD_PLUG_MESSAGE bit set.  ^
 * The entire message string is in the [strings] argument; [arg] is the index 
 * of the current string and should be returned with the index of the last 
 * string of the message executed by this plugin.
 * A non-zero return indicates an error.
 */
int PLUG_EXPORT imodPlugExecuteMessage(ImodView *vw, QStringList *strings,
                                         int *arg);
}
#endif

/*
  $Log$
  Revision 4.10  2008/01/17 22:33:24  mast
  Changed documentation for model update

  Revision 4.9  2008/01/11 18:11:29  mast
  Wheel events now work when declared correctly

  Revision 4.8  2008/01/11 17:32:54  mast
  Needed forward declaration of QEvent

  Revision 4.7  2007/12/04 22:05:30  mast
  Add function for handling event, improve documentation

  Revision 4.6  2006/02/13 05:10:44  mast
  Added mouse function

  Revision 4.5  2004/09/24 18:07:50  mast
  Added execute message declaration

  Revision 4.4  2004/06/04 02:58:20  mast
  It really needed to export unconditionally although it worked as import
  
  Revision 4.3  2004/05/31 23:10:56  mast
  Added macros for exporting/importing under Windows
  
  Revision 4.2  2003/10/01 05:07:11  mast
  Split out private file
  
  Revision 4.1  2003/02/10 20:41:55  mast
  Merge Qt source
  
  Revision 1.1.2.3  2003/01/29 01:43:57  mast
  switch back to extern C
  
  Revision 1.1.2.2  2003/01/27 00:30:07  mast
  Pure Qt version and general cleanup
    
  Revision 1.1.2.1  2003/01/13 01:06:53  mast
  Initial creation

*/
