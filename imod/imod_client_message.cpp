/*  IMOD VERSION 2.7.2
 *
 *  imod_client_message.c   Used to handle X client messages - 
 *                             now QClipboard changes
 *
 *  Original author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/

/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <qdir.h>
#include <qregexp.h>
#include <qtimer.h>
#include <qapplication.h>
#include <qclipboard.h>
#include "imod.h"
#include "imod_display.h"
#include "imodv.h"
#include "imodv_objed.h"
#include "imodv_input.h"
#include "imodv_window.h"
#include "imod_io.h"
#include "xcramp.h"
#include "imod_info_cb.h"
#include "imod_info.h"
#include "imod_input.h"
#include "imod_client_message.h"
#include "imodplug.h"
#include "imod_workprocs.h"
#include "control.h"
#include "xzap.h"

//  Module variables
static int message_action = MESSAGE_NO_ACTION;
static char *message_string = NULL;
static int message_stamp = -1;

ImodClipboard::ImodClipboard()
  : QObject()
{
  QClipboard *cb = QApplication::clipboard();
  cb->setSelectionMode(false);
  mHandling = false;
  mExiting = false;
  mClipTimer = NULL;
  mClipHackTimer = NULL;

  // If the hack is needed, start a timer to check the clipboard
#ifdef CLIPBOARD_TIMER_HACK
  mClipHackTimer = new QTimer(this);
  connect(mClipHackTimer, SIGNAL(timeout()), this, SLOT(clipHackTimeout()));
  mSavedClipboard = cb->text();
  mClipHackTimer->start(CLIPBOARD_TIMER_HACK);
#else

  // Otherwise just connect to signal for changes
  connect(cb, SIGNAL(dataChanged()), this, SLOT(clipboardChanged()));
#endif
}

/*
 * Clipboard change event slot and timeout for performing actions
 */
void ImodClipboard::clipboardChanged()
{
  if (Imod_debug) {
    QClipboard *cb = QApplication::clipboard();
    cb->setSelectionMode(false);
    QString text = cb->text();
    imodPrintStderr("imodHandleClientMessage - clipboard = %s\n", 
            text.latin1());
    if (ImodInfoWin)
      wprint("clipboardChanged = %s\n", text.latin1());
  }
  // If already handling a change or the message is not for us, return
  if (mHandling || !handleMessage())
    return;

  // Otherwise create and start a timer to execute the action
  // Set flag because event comes in twice in Windows
  mHandling = true;
  mClipTimer = new QTimer(this);
  connect(mClipTimer, SIGNAL(timeout()), this, SLOT(clipTimeout()));
  mClipTimer->start(10, true);
}

void ImodClipboard::clipTimeout()
{
  // If exiting flag is set, then finally quit
  if (mExiting) {
    disconnect(mClipTimer);
    delete mClipTimer;
    mClipTimer = NULL;
    if (ImodvClosed || !Imodv->standalone)
      imod_quit();
    else
      imodvQuit();

  } else {
    
    // Otherwise, execute the message
    if (executeMessage()) {

      // If it returns true, set the exiting flag and start another timer
      mExiting = true;
      mClipTimer->start(200, true);          // 100 ms is too short on SGI
    } else if (mClipTimer){

      // Otherwise, all done with this timer
      disconnect(mClipTimer);
      delete mClipTimer;
      mClipTimer = NULL;
    }
    mHandling = false;
  }
}

// The hack for Windows: periodically check for clipboard changes
void ImodClipboard::clipHackTimeout()
{
  QClipboard *cb = QApplication::clipboard();
  cb->setSelectionMode(false);
  if (cb->text() == mSavedClipboard)
    return;
  mSavedClipboard = cb->text();
  clipboardChanged();
}


// Parse the message, see if it is for us, and save in local variables
bool ImodClipboard::handleMessage()
{
  int index, newStamp;
  if (!ImodInfoWin && ImodvClosed)
    return false;

  // get the text from the clipboard
  QClipboard *cb = QApplication::clipboard();
  cb->setSelectionMode(false);
  QString text = cb->text();
  if (Imod_debug) {
    imodPrintStderr("imodHCM in handleMessage = %s\n", 
            text.latin1());
    wprint("handleMessage = %s\n", text.latin1());
  }                   

  // Return if text is empty, starts with a space or has no spaces
  if (text.isEmpty())
    return false;
  index = text.find(" ");
  if (index <= 0)
    return false;

  // Remove multiple spaces
  text = text.replace(QRegExp("  *"), " ");

  // Return false if this is not our info window ID
  if ((text.left(index)).toInt() != ourWindowID())
    return false;

  // Return false if it is our own string
  QString text2 = text.mid(index + 1);
  index = text2.find(" ");
  if (text2.left(index) == "OK" || text2.left(index) == "ERROR")
    return false;

  // If we see the same message again, send the last response again
  newStamp = text2.left(index).toInt();
  if (newStamp == message_stamp) {
    sendResponse(-1);
    return false;
  }
  message_stamp = newStamp;

  QString text3 = text2.mid(index + 1);
  index = text3.find(" ");

  // get the action value, and the string if there is one
  message_action = text3.left(index).toInt();
  if (text3.length() > index + 1)
    message_string = strdup((text3.mid(index + 1)).latin1());
  return true;
}

// This function performs the action after the delay is up.  It returns
// true if the program is still to quit
bool ImodClipboard::executeMessage()
{
  int returnValue;
  int succeeded = 1;
  QString convName;
  int movieVal, xaxis, yaxis, zaxis, taxis;
  int objNum, type, symbol, symSize, ptSize;
  Imod *imod = App->cvi->imod;
  Iobj *obj;
  int symTable[] = 
    { IOBJ_SYM_NONE, IOBJ_SYM_CIRCLE, IOBJ_SYM_SQUARE, IOBJ_SYM_TRIANGLE };

  if (Imod_debug)
    wprint("Executing message\n");

  /* Execute the action */
  if (ImodvClosed || !Imodv->standalone) {
    switch (message_action) {

    case MESSAGE_OPEN_MODEL:
    case MESSAGE_OPEN_KEEP_BW:
      if (!message_string) {
        imodPrintStderr("imodExecuteMessage: no filename sent"
                " with command to open model\n");
        succeeded = 0;
        break;
      } else {

        // DNM 6/3/04: moved this down here, do it only if needed
        QDir *curdir = new QDir();
        convName = curdir->cleanDirPath(QString(message_string));
        delete curdir;
      }
    
      // Since this could open a dialog with an indefinite delay, just send
      // the OK signal now
      succeeded = -1;
      sendResponse(1);
      inputRaiseWindows();

      // DNM 6/3/04: switch to keeping BW values in the first place
      returnValue = openModel((char *)convName.latin1(), 
                              message_action == MESSAGE_OPEN_KEEP_BW);
      if(returnValue == IMOD_IO_SUCCESS) {
        wprint("%s loaded.\n", 
               (QDir::convertSeparators(QString(Imod_filename))).latin1());

      }
      else if(returnValue == IMOD_IO_SAVE_ERROR) {
        wprint("Error Saving Model. New model not loaded.\n");
      }
      else if(returnValue == IMOD_IO_SAVE_CANCEL) {
        wprint("Operation cancelled. New model not loaded.\n");
      }

      // The model does not exist yet.  Try creating a new model.
      else if(returnValue == IMOD_IO_DOES_NOT_EXIST) {
        returnValue = createNewModel((char *)convName.latin1());
        if(returnValue == IMOD_IO_SUCCESS) {
        
          wprint("New model %s created.\n", 
                 (QDir::convertSeparators(QString(Imod_filename))).latin1());
        }
        else {
          wprint("Could not create a new model %s.\n", message_string);
        }
      }
      else if(returnValue == IMOD_IO_NO_ACCESS_ERROR) {
        wprint("Error opening model. Check file permissions\n.");
      }
      else {
        wprint("Unknown return code, new model not loaded!!\n");
      }
      break;

    case MESSAGE_SAVE_MODEL:
      succeeded = -1;
      sendResponse(1);
      imod->blacklevel = App->cvi->black;
      imod->whitelevel = App->cvi->white;
      returnValue = SaveModel(imod);
      break;

    case MESSAGE_VIEW_MODEL:
      imod_autosave(imod);
      imodv_open();
      break;

    case MESSAGE_QUIT:
      //    imod_quit();
      break;

    case MESSAGE_RAISE_WINDOWS:
      inputRaiseWindows();
      break;

    case MESSAGE_MODEL_MODE:
      movieVal = 1;
      if (message_string)
        movieVal = atoi(message_string);
      if (movieVal > 0)
        imod_set_mmode(IMOD_MMODEL);
      else {
        imod_set_mmode(IMOD_MMOVIE);
        if (movieVal < 0) {
          xaxis = (-movieVal) & 1 ? 1 : 0;
          yaxis = (-movieVal) & 2 ? 1 : 0;
          zaxis = (-movieVal) & 4 ? 1 : 0;
          taxis = (-movieVal) & 8 ? 1 : 0;
          imodMovieXYZT(App->cvi, xaxis, yaxis, zaxis, taxis);
        }
      }
      break;

    case MESSAGE_OPEN_BEADFIXER:
      imodPlugOpenByName("Bead Fixer");
      break;

    case MESSAGE_ONE_ZAP_OPEN:
      if (imodDialogManager.windowCount(ZAP_WINDOW_TYPE) || !imodLoopStarted())
        inputRaiseWindows();
      else
        imod_zap_open(App->cvi);
      break;

    case MESSAGE_RUBBERBAND:
      zapReportRubberband();
      break;

    case MESSAGE_OBJ_PROPERTIES:
    case MESSAGE_NEWOBJ_PROPERTIES:
      objNum = 1;
      type = symbol = symSize = ptSize = -1;
      if (!message_string) {
        imodPrintStderr("imodExecuteMessage: no values sent with object"
                " property command\n");
        succeeded = 0;
        break;
      }
      
      sscanf(message_string, "%d %d %d %d %d", &objNum, &type, &symbol,
             &symSize, &ptSize);

      // Object is numbered from 1, so decrement and test for substituting
      // current object
      if (--objNum < 0)
        objNum = imod->cindex.object;
      if (objNum < 0 || objNum >= imod->objsize) {
        imodPrintStderr("imodExecuteMessage: illegal object # sent with object"
                " property command\n");
        succeeded = 0;
        break;
      }
      obj = &imod->obj[objNum];

      // If object has contours, skip for NEWOBJ message
      if (obj->contsize && message_action == MESSAGE_NEWOBJ_PROPERTIES)
        break;

      // Process the changes if not -1: object type
      if (type >= 0 && type < 3) {
        switch (type) {
        case 0:
          obj->flags &= ~(IMOD_OBJFLAG_OPEN | IMOD_OBJFLAG_SCAT);
          break;
        case 1:
          obj->flags |= IMOD_OBJFLAG_OPEN;
          obj->flags &= ~IMOD_OBJFLAG_SCAT;
          break;
        case 2:
          obj->flags |= IMOD_OBJFLAG_SCAT | IMOD_OBJFLAG_OPEN;
          break;
        }
      }

      // Symbol type and filled
      if (symbol >= 0) {
        if ((symbol & 7) < (sizeof(symTable) / sizeof(int)))
          obj->symbol = symTable[symbol & 7];
        if (symbol & 8)
          obj->symflags |= IOBJ_SYMF_FILL;
        else
          obj->symflags &= ~IOBJ_SYMF_FILL;
      }

      // Symbol size, 3d point size
      if (symSize > 0)
        obj->symsize = symSize;
      if (ptSize >= 0)
        obj->pdrawsize = ptSize;

      // The general draw updates object edit window, but need to call 
      // imodv object edit for it to update
      imodDraw(App->cvi, IMOD_DRAW_MOD);
      imodvObjedNewView();
      break;

    default:
      imodPrintStderr("imodExecuteMessage: action %d not recognized\n"
              , message_action);
      succeeded = 0;
    }
  } else {

    // Messages for 3dmodv
    switch (message_action) {
    case MESSAGE_QUIT:
      break;

    case MESSAGE_RAISE_WINDOWS:
      imodvInputRaise();
      break;

    default:
      imodPrintStderr("imodExecuteMessage: action %d not recognized by"
              " 3dmodv\n" , message_action);
      succeeded = 0;
    }
  }
  if (message_string)
    free(message_string);
  message_string = NULL;

  // Now set the clipboard with the response
  if (succeeded >= 0)
    sendResponse(succeeded);
  return message_action == MESSAGE_QUIT;
}

void ImodClipboard::sendResponse(int succeeded)
{
  static int lastResponse = 0;
  QString str;
  if (succeeded < 0)
    succeeded = lastResponse;
  lastResponse = succeeded;
  str.sprintf("%u %s", ourWindowID(), succeeded ? "OK" : "ERROR");
  QClipboard *cb = QApplication::clipboard();
  cb->setSelectionMode(false);
  cb->setText(str);
}

unsigned int ImodClipboard::ourWindowID()
{
  if (ImodvClosed || !Imodv->standalone)
    return (unsigned int)ImodInfoWin->winId();
  return (unsigned int)Imodv->mainWin->winId();
}

/*
$Log$
Revision 4.16  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.15  2004/05/31 02:14:58  mast
Added message to set object properties (type, symbol, 3D point)

Revision 4.14  2004/05/28 23:29:42  mast
Do not open a zap window until the event loop has started

Revision 4.13  2004/05/05 17:32:44  mast
Added message to get rubberband coordinates

Revision 4.12  2004/04/28 23:51:44  mast
Added ability to open zap if none are open

Revision 4.11  2004/01/07 01:54:51  mast
Add capability to start movie on any axis with negative value to 6

Revision 4.10  2003/11/12 18:48:37  mast
Added ability to process messages in imodv

Revision 4.9  2003/10/02 01:30:09  mast
Added message to open bead fixer

Revision 4.8  2003/09/24 16:20:05  mast
Remove multiple spaces in message before processing, and keep track of
and send last response if a message is repeated

Revision 4.7  2003/09/24 15:07:11  mast
Improved debug mode, added a repeated response to same message

Revision 4.6  2003/08/01 05:52:38  mast
Allowed model mode message to set back to movie mode, added message to open
file and keep black/white levels.

Revision 4.5  2003/06/04 23:42:33  mast
Add message to go to model mode

Revision 4.4  2003/05/23 02:45:29  mast
Add message to raise windows, make open model raise windows also

Revision 4.3  2003/02/27 20:57:42  mast
Adjusting timing on quit for the SGI

Revision 4.2  2003/02/27 19:22:16  mast
Qt version that works on windows

Revision 4.1  2003/02/10 20:28:59  mast
autox.cpp

Revision 1.1.2.3  2003/01/29 01:43:23  mast
Have it return true at end

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/23 23:05:57  mast
conversion to cpp

Revision 3.3.2.4  2003/01/23 19:54:09  mast
add include of imod_io

Revision 3.3.2.3  2003/01/13 01:15:42  mast
changes for Qt version of info window

Revision 3.3.2.2  2002/12/17 18:35:03  mast
Removing argument to call to open imodv

Revision 3.3.2.1  2002/12/05 16:23:52  mast
No changes - CVS detected as modified in branch

Revision 3.4  2002/12/03 15:53:57  mast
Make it set the forbid level before any action that can produce file
dialog box.  Have it distinguish cancel from error in error reports

Revision 3.3  2002/12/01 16:51:34  mast
Changes to eliminate warnings on SGI

Revision 3.2  2002/10/22 22:38:02  mast
Added include of imod_client_message.h, removed some obviously redundant
or unneeded includes carried over from imod_menu.c

Revision 3.1  2002/10/15 21:18:09  rickg
executeMessage() call moved so that it get called for all messages.

Revision 3.0  2002/09/27 20:30:06  rickg
Initital version of code moved from imod_menu_cb.c

*/
