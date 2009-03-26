/*  
 *  imod_client_message.c   Used to handle X client messages - 
 *                             now QClipboard changes or stdin messages
 *
 *  Original author: David Mastronarde   email: mast@colorado.edu
 *  $Id$
 *  Log at end of file
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifndef _WIN32
#include <sys/select.h>
#include <sys/time.h>
#endif
#include <qdir.h>
#include <qregexp.h>
#include <qtimer.h>
#include <qapplication.h>
#include <qclipboard.h>
#include "imod.h"
#include "imod_display.h"
#include "dia_qtutils.h"
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
#include "sslice.h"

//  Module variables
static int message_action = MESSAGE_NO_ACTION;
static QStringList messageStrings;
static int message_stamp = -1;

#define STDIN_INTERVAL  50
#define MAX_LINE 256
static char threadLine[MAX_LINE];
static bool gotLine = false;
static int lineLen;

#if defined(_WIN32) && defined(QT_THREAD_SUPPORT)
static QMutex mutex;
static StdinThread *stdThread = NULL;
#endif

static int readLine(char *line);

ImodClipboard::ImodClipboard(bool useStdin)
  : QObject()
{
  QClipboard *cb = QApplication::clipboard();
  //cb->setSelectionMode(false);
  mHandling = false;
  mExiting = false;
  mDisconnected = false;
  mClipHackTimer = NULL;
  mStdinTimer = NULL;
  mUseStdin = useStdin;

  if (useStdin) {
#if defined(_WIN32) && defined(QT_THREAD_SUPPORT)
    stdThread = new StdinThread();
    stdThread->start();
#endif
    mStdinTimer = new QTimer(this);
    connect(mStdinTimer, SIGNAL(timeout()), this, SLOT(stdinTimeout()));
    mStdinTimer->start(STDIN_INTERVAL);
  } else {

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
}

/*
 * Functions to disconnect on Windows using stadard input
 * First request the disconnect, which should send a blank line
 */
void ImodClipboard::startDisconnect()
{
#if defined(_WIN32) && defined(QT_THREAD_SUPPORT)
  if (mUseStdin && stdThread->isRunning())
    imodPrintStderr("REQUEST STOP LISTENING\n");
#endif
}

// Then wait until the thread exits or give error
int ImodClipboard::waitForDisconnect()
{
#if defined(_WIN32) && defined(QT_THREAD_SUPPORT)
  int timeout = 5000;
  if (mUseStdin && stdThread->isRunning()) {
    if (stdThread->wait(timeout))
      return 0;
    dia_err("3dmod is stuck listening for input and is unable to terminate"
            " cleanly\nYou need to kill it in Task Manager or with Ctrl C");
    stdThread->terminate();
  }
  return 1;
#else
  return 0;
#endif
}


/*
 * Clipboard change event slot and timeout for performing actions
 */
void ImodClipboard::clipboardChanged()
{
  if (Imod_debug) {
    QClipboard *cb = QApplication::clipboard();
    //cb->setSelectionMode(false);
    QString text = cb->text();
    imodPrintStderr("imodHCM in clipboardChanged - clipboard = %s\n", 
                    LATIN1(text));
    if (ImodInfoWin)
      wprint("clipboardChanged = %s\n", LATIN1(text));
  }
  // If already handling a change or the message is not for us, return
  if (mHandling || !handleMessage())
    return;

  // Otherwise create and start a timer to execute the action
  // Set flag because event comes in twice in Windows
  mHandling = true;
  QTimer::singleShot(10, this, SLOT(clipTimeout()));
}

void ImodClipboard::clipTimeout()
{
  // If exiting flag is set, then finally quit
  if (mExiting) {
    if (ImodvClosed || !Imodv->standalone)
      imod_quit();
    else
      imodvQuit();

  } else {
    
    // Otherwise, execute the message
    if (executeMessage()) {

      // If it returns true, set the exiting flag and start another timer
      // 100 ms is too short on SGI
      mExiting = true;
      QTimer::singleShot(200, this, SLOT(clipTimeout()));
    } 
    mHandling = false;
  }
}

// The hack for Windows: periodically check for clipboard changes
void ImodClipboard::clipHackTimeout()
{
  QClipboard *cb = QApplication::clipboard();
  //cb->setSelectionMode(false);
  if (cb->text() == mSavedClipboard)
    return;
  mSavedClipboard = cb->text();
  clipboardChanged();
}

// Timer to check for stdin messages
void ImodClipboard::stdinTimeout()
{
  QString text;
  if (mHandling)
    return;

#ifdef _WIN32
  // See if the thread got a line
  // Copy the line while we have the mutex
  bool gotOne;
  mutex.lock();
  gotOne = gotLine;
  gotLine = false;
  if (gotOne)
     text = threadLine;
  mutex.unlock();
  if (!gotOne)
    return;
#else
  fd_set readfds, writefds, exceptfds;
  struct timeval timeout;

  FD_ZERO(&readfds);
  FD_ZERO(&writefds);
  FD_ZERO(&exceptfds);
  FD_SET(fileno(stdin), &readfds);
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
  if (select(1, &readfds, &writefds, &exceptfds, &timeout) <= 0)
    return;

  lineLen = readLine(threadLine);
  text = threadLine;
#endif

  if (!lineLen) {
    mStdinTimer->stop();
    disconnect(mStdinTimer);
    //delete mStdinTimer;
    //mStdinTimer = NULL;
    sendResponse(1);
    mDisconnected = true;
    return;
  }

  messageStrings = text.split(" ", QString::SkipEmptyParts);

  // Start timer to execute message just as for clipboard
  mHandling = true;
  QTimer::singleShot(10, this, SLOT(clipTimeout()));
}

// Parse the message, see if it is for us, and save in local variables
bool ImodClipboard::handleMessage()
{
  int newStamp;
  if (!ImodInfoWin && ImodvClosed)
    return false;

  // get the text from the clipboard
  QClipboard *cb = QApplication::clipboard();
  //cb->setSelectionMode(false);
  QString text = cb->text();
  if (Imod_debug) {
    imodPrintStderr("imodHCM in handleMessage = %s\n", LATIN1(text));
    wprint("handleMessage = %s\n", LATIN1(text));
  }                   

  // Return if text is empty
  if (text.isEmpty())
    return false;

  // Split the string, ignoring multiple spaces, and return false if fewer
  // than 3 elements
  messageStrings = text.split(" ", QString::SkipEmptyParts);
  if (messageStrings.count() < 3)
    return false;

  // Return false if this is not our info window ID
  if (messageStrings[0].toInt() != ourWindowID())
    return false;

  // If we see the same message again, send the last response again
  newStamp = messageStrings[1].toInt();
  if (newStamp == message_stamp) {
    sendResponse(-1);
    return false;
  }
  message_stamp = newStamp;
  return true;
}

// This function performs the action after the delay is up.  It returns
// true if the program is still to quit
bool ImodClipboard::executeMessage()
{
  int returnValue, arg;
  int succeeded = 1;
  QString convName;
  QDir *curdir;
  int movieVal, xaxis, yaxis, zaxis, taxis;
  int objNum, type, symbol, symSize, ptSize, mode, mask, interval;
  bool props1;
  Imod *imod;
  Iobj *obj;
  int symTable[] = 
    { IOBJ_SYM_NONE, IOBJ_SYM_CIRCLE, IOBJ_SYM_SQUARE, IOBJ_SYM_TRIANGLE };

  // Number of arguments required - for backward compatibility, going to
  // model mode does not require one but should have one
  int requiredArgs[] = {0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 5, 5, 0, 2, 4, 4, 3};
  int numArgs = messageStrings.count();

  // Loop on the actions in the list; set arg to numArgs to break loop
  for (arg = mUseStdin ? 0 : 2; arg < numArgs; arg++) {
    imod = App->cvi->imod;

    // Get the action, then check that there are enough values for it
    message_action = messageStrings[arg].toInt();
    if (message_action < sizeof(requiredArgs) / sizeof(int) &&
        arg + requiredArgs[message_action] >= numArgs) {
        imodPrintStderr("imodExecuteMessage: not enough values sent"
                " with action command %d\n", message_action);
        succeeded = 0;
        break;
    }

    if (Imod_debug) {
      wprint("Executing message action %d\n", message_action);
      imodPrintStderr("imodHCM in executeMessage: executing message action "
                      "%d\n", message_action);
    }

    if (ImodvClosed || !Imodv->standalone) {
      switch (message_action) {
        
      case MESSAGE_OPEN_MODEL:
      case MESSAGE_OPEN_KEEP_BW:
        curdir = new QDir();
        convName = curdir->cleanPath(messageStrings[++arg]);
        delete curdir;
    
        // Since this could open a dialog with an indefinite delay, just send
        // the OK signal now
        succeeded = -1;
        sendResponse(1);
        inputRaiseWindows();

        // DNM 6/3/04: switch to keeping BW values in the first place
        returnValue = openModel(LATIN1(convName), 
                                message_action == MESSAGE_OPEN_KEEP_BW, false);
        if(returnValue == IMOD_IO_SUCCESS) {
          wprint("%s loaded.\n", 
                 LATIN1(QDir::convertSeparators(QString(Imod_filename))));

        }
        else if(returnValue == IMOD_IO_SAVE_ERROR) {
          wprint("Error Saving Model. New model not loaded.\n");
          arg = numArgs;
        }
        else if(returnValue == IMOD_IO_SAVE_CANCEL) {
          wprint("Operation cancelled. New model not loaded.\n");
          arg = numArgs;
        }

        // The model does not exist yet.  Try creating a new model.
        else if(returnValue == IMOD_IO_DOES_NOT_EXIST) {
          returnValue = createNewModel(LATIN1(convName));
          if(returnValue == IMOD_IO_SUCCESS) {
        
            wprint("New model %s created.\n", 
                   LATIN1(QDir::convertSeparators(QString(Imod_filename))));
          }
          else {
            wprint("Could not create a new model %s.\n", 
                   LATIN1(messageStrings[arg]));
            arg = numArgs;
          }
        }
        else if(returnValue == IMOD_IO_NO_ACCESS_ERROR) {
          wprint("Error opening model. Check file permissions\n.");
          arg = numArgs;
        }
        else {
          wprint("Unknown return code, new model not loaded!!\n");
          arg = numArgs;
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
        inputRaiseWindows();
        imodv_open();
        break;

      case MESSAGE_QUIT:
        arg = numArgs;
        break;

      case MESSAGE_RAISE_WINDOWS:
        inputRaiseWindows();
        break;

      case MESSAGE_MODEL_MODE:
        movieVal = 1;
        if (arg < numArgs - 1)
        movieVal = messageStrings[++arg].toInt();
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
        inputRaiseWindows();
        if (!imodDialogManager.windowCount(ZAP_WINDOW_TYPE) &&
            imodLoopStarted())
          imod_zap_open(App->cvi, 0);
        break;

      case MESSAGE_RUBBERBAND:
        zapReportRubberband();
        break;

      case MESSAGE_SLICER_ANGLES:
        slicerReportAngles();
        break;

      case MESSAGE_OBJ_PROPERTIES:
      case MESSAGE_NEWOBJ_PROPERTIES:
      case MESSAGE_OBJ_PROPS_2:
      case MESSAGE_NEWOBJ_PROPS_2:
        props1 = message_action == MESSAGE_OBJ_PROPERTIES ||
          message_action == MESSAGE_NEWOBJ_PROPERTIES;
        objNum = messageStrings[++arg].toInt();
        type = messageStrings[++arg].toInt();
        symbol = messageStrings[++arg].toInt();
        symSize = messageStrings[++arg].toInt();
        if (props1)
          ptSize = messageStrings[++arg].toInt();

        // Object is numbered from 1, so decrement and test for substituting
        // current object
        if (--objNum < 0)
          objNum = imod->cindex.object;
        if (objNum < 0 || objNum >= imod->objsize) {
          imodPrintStderr("imodExecuteMessage: illegal object # sent with "
                          "object property command\n");
          succeeded = 0;
          arg = numArgs;
          break;
        }
        obj = &imod->obj[objNum];

        // If object has contours, skip for NEWOBJ message
        if (obj->contsize && (message_action == MESSAGE_NEWOBJ_PROPERTIES ||
                              message_action == MESSAGE_NEWOBJ_PROPS_2))
          break;

        if (props1) {

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
            utilSetObjFlag(obj, 0, (symbol & 8) != 0, IOBJ_SYMF_FILL);
          }
          
          // Symbol size, 3d point size
          if (symSize > 0)
            obj->symsize = symSize;
          if (ptSize >= 0)
            obj->pdrawsize = ptSize;
        } else {

          // Points per contour
          if (type >= 0)
            obj->extra[IOBJ_EX_PNT_LIMIT] = type;

          // Planar contours
          if (symbol >= 0)
            utilSetObjFlag(obj, 0, symbol != 0, IMOD_OBJFLAG_PLANAR);

          // Sphere on sec only
          if (symSize >= 0)
            utilSetObjFlag(obj, 0, symSize != 0, IMOD_OBJFLAG_PNT_ON_SEC);
        }        

        // The general draw updates object edit window, but need to call 
        // imodv object edit for it to update
        imodDraw(App->cvi, IMOD_DRAW_MOD);
        imodvObjedNewView();

        // If no contours and only 1 obj, set checksum to avoid save requests
        if (imod->objsize == 1 && !obj->contsize)
          imod->csum = imodChecksum(imod);
        break;

      case MESSAGE_GHOST_MODE:
        mode = messageStrings[++arg].toInt();
        mask = messageStrings[++arg].toInt();
        App->cvi->ghostmode = (App->cvi->ghostmode & ~mask) | (mode & mask);
        interval = messageStrings[++arg].toInt();
        if (interval >= 0)
          App->cvi->ghostdist = interval;
        imodDraw(App->cvi, IMOD_DRAW_MOD);
        break;
        
      case MESSAGE_PLUGIN_EXECUTE:
        arg++;
        if (imodPlugMessage(App->cvi, &messageStrings, &arg)) {
          succeeded = 0;
          arg = numArgs;
          break;
        }
        break;

      default:
        imodPrintStderr("imodExecuteMessage: action %d not recognized\n"
                        , message_action);
        succeeded = 0;
        arg = numArgs;
      }
    } else {
      
      // Messages for 3dmodv
      switch (message_action) {
      case MESSAGE_QUIT:
        arg = numArgs;
        break;
        
      case MESSAGE_RAISE_WINDOWS:
        imodvInputRaise();
        break;
        
      default:
        imodPrintStderr("imodExecuteMessage: action %d not recognized by"
                        " 3dmodv\n" , message_action);
        succeeded = 0;
        arg = numArgs;
      }
    }
  }

  // Now set the clipboard with the response
  if (succeeded >= 0)
    sendResponse(succeeded);
  return message_action == MESSAGE_QUIT;
}

// Send response to clipboard or standard error
void ImodClipboard::sendResponse(int succeeded)
{
  static int lastResponse = 0;
  QString str;
  if (succeeded < 0)
    succeeded = lastResponse;
  lastResponse = succeeded;
  if (mUseStdin) {
    imodPrintStderr("%s\n", succeeded ? "OK" : "ERROR");
  } else {
    str.sprintf("%u %s", ourWindowID(), succeeded ? "OK" : "ERROR");
    QClipboard *cb = QApplication::clipboard();
    //cb->setSelectionMode(false);
    cb->setText(str);
  }
}

unsigned int ImodClipboard::ourWindowID()
{
  if (ImodvClosed || !Imodv->standalone)
    return (unsigned int)ImodInfoWin->winId();
  return (unsigned int)Imodv->mainWin->winId();
}

#if defined(_WIN32) && defined(QT_THREAD_SUPPORT)

// The thread loops on reading a line and sets the flag when it gets one
void StdinThread::run()
{
  bool gotOne;
  bool quit;
  while (1) {
    mutex.lock();
    gotOne = gotLine;
    mutex.unlock();

    // Do not go for another line until the main thread has cleared the flag
    if (gotOne) {
      msleep(20);
      continue;
    }
    
    lineLen = readLine(threadLine);
    quit = (strlen(threadLine) == 1 && threadLine[0] == '4') || !lineLen;
    mutex.lock();
    gotLine = true;
    mutex.unlock();
    if (quit)
      break;
  }
}    
#endif

// Common routine to read a line, strip endings, and return length
static int readLine(char *line)
{
  if (!fgets(line, MAX_LINE, stdin)) {
    line[0] = 0x00;
    return 0;
  }
  if (line[MAX_LINE - 1])
    line[MAX_LINE - 1] = 0x00;
  while (strlen(line) > 0 && 
         (line[strlen(line) - 1] == '\n' || line[strlen(line) - 1] == '\r'))
    line[strlen(line) - 1] = 0x00;
  return strlen(line);
}


/*
$Log$
Revision 4.31  2009/01/16 05:12:21  mast
Stop deleting tier in handler and switch to single-shot static for mClipTimer

Revision 4.30  2009/01/15 16:33:17  mast
Qt 4 port

Revision 4.29  2008/07/24 17:22:01  mast
Fixed reading of object properties 2 to not swallow another number

Revision 4.28  2008/07/16 04:29:08  mast
Added new messages for some more object properties

Revision 4.27  2008/02/06 20:28:37  mast
Added flag to keep track of whether stderr was disconnected

Revision 4.26  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 4.25  2006/07/03 19:55:29  mast
Replaced destructor with explicit calls to request a disconnect signal
from the source of standard input and to wait until thread exits

Revision 4.24  2006/06/20 17:26:53  mast
Changed stdin listening to use select function instead of thread
except in windows, because of problems killing thread in Linux

Revision 4.23  2006/06/19 05:30:17  mast
Added ability to use standard input instead of clipboard

Revision 4.22  2005/10/14 22:04:39  mast
Changes for Model reload capability

Revision 4.21  2004/09/24 18:08:59  mast
Added message for passing message to plugins

Revision 4.20  2004/08/12 17:15:02  mast
Added message to get slicer angles

Revision 4.19  2004/06/22 23:59:28  mast
Made it redo checksum after changing object type of blank model

Revision 4.18  2004/06/05 00:17:43  mast
Converted to allowing multiple actions in one message

Revision 4.17  2004/06/04 03:21:27  mast
Simplified code for keeping black/white level by just keeping the level
the same in the open function - seemed to help crashing problem on Linux

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
