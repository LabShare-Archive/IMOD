/*   imodplug.h  -  public declarations for imodplug.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$
    
$Date$

$Revision$
Log at end of file
*/

#ifndef IMODPLUG_H
#define IMODPLUG_H

class QKeyEvent;
class QStringList;
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
 * Function must be defined by plugin.
 *
 * Returns the name of the plugin.
 * Bit flags for the type of plugins are returned in the type.
 * Not all of 3dmod's data structures may be initialized at the time of
 * this function call so no initialization should be done.
 */
char PLUG_EXPORT *imodPlugInfo(int *type);

/*
 * Generic Plugin interface.
 * 3dmod will call these functions on your behalf at 
 * well defined times.
 *
 * Menu execution function for plugins with the IMOD_PLUG_MENU bit set.
 * imodPlugExecute will be called if available; if not defined then 
 * the following call will be made:
 * imodPlugExecuteType(inView, IMOD_PLUG_MENU, IMOD_REASON_EXECUTE);
 */
  void PLUG_EXPORT imodPlugExecute(ImodView *vw);
  void PLUG_EXPORT imodPlugExecuteType(ImodView *inView, int inType, 
                                     int inReason);

  /*
   * Key input callback function to be defined by plugins with the
   * IMOD_PLUG_KEYS bit set.
   * This function can be used to override edmod hot key settings.
   * A nonzero return value indicates that the plugin handled the input key.
   * and that no other action should be taken by the 3dmod program.
   * A zero return value indicates that 3dmod should process the key as usual.
   */
  int PLUG_EXPORT imodPlugKeys(ImodView *vw, QKeyEvent *event);

  /*
   * Function to execute a message, to be defined by plugins with the
   * IMOD_PLUG_MESSAGE bit set.  The entire message string is in the "strings"
   * argument; "arg" is the index of the current string and should be
   * returned with the index of the last string of the message executed
   * by this plugin.
   * A non-zero return indicates an error.
   */
  int PLUG_EXPORT imodPlugExecuteMessage(ImodView *vw, QStringList *strings,
                                         int *arg);
}
#endif

/*
  $Log$
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
