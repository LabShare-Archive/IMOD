/*
 * imod_client_message.h
 *
 *  Original Author: David Mastronarde   email: mast@colorado.edu
 *  $Id$
 *  Log at end of file
 */

#ifndef IMOD_CLIENT_MESSAGE_H
#define IMOD_CLIENT_MESSAGE_H

#define MESSAGE_NO_ACTION   0
#define MESSAGE_OPEN_MODEL  1
#define MESSAGE_SAVE_MODEL  2
#define MESSAGE_VIEW_MODEL  3
#define MESSAGE_QUIT  4
#define MESSAGE_RAISE_WINDOWS 5
#define MESSAGE_MODEL_MODE  6
#define MESSAGE_OPEN_KEEP_BW 7
#define MESSAGE_OPEN_BEADFIXER 8
#define MESSAGE_ONE_ZAP_OPEN 9
#define MESSAGE_RUBBERBAND 10
#define MESSAGE_OBJ_PROPERTIES 11
#define MESSAGE_NEWOBJ_PROPERTIES 12
#define MESSAGE_SLICER_ANGLES 13
#define MESSAGE_PLUGIN_EXECUTE 14
#define MESSAGE_OBJ_PROPS_2    15
#define MESSAGE_NEWOBJ_PROPS_2 16
#define MESSAGE_GHOST_MODE     17

/* Definitions for plugins/special modules */
#define MESSAGE_BEADFIX_OPENFILE   1
#define MESSAGE_BEADFIX_REREAD     2
#define MESSAGE_BEADFIX_SEEDMODE   3
#define MESSAGE_BEADFIX_AUTOCENTER 4
#define MESSAGE_BEADFIX_DIAMETER   5
#define MESSAGE_BEADFIX_OPERATION  6
#define MESSAGE_BEADFIX_SKIPLIST   7
#define MESSAGE_BEADFIX_DELALLSEC  8
#define MESSAGE_BEADFIX_CLEARSKIP  9

#include <qobject.h>
#include <qstring.h>
class QTimer;

class ImodClipboard : public QObject
{
  Q_OBJECT

 public:
  ImodClipboard(bool useStdin);
  ~ImodClipboard() {};
  bool handleMessage();
  bool executeMessage();
  void sendResponse(int succeeded);
  unsigned int ourWindowID();
  void startDisconnect();
  int waitForDisconnect();
  bool disconnectedFromStderr() {return mDisconnected;};

  QTimer *mClipHackTimer;
  QTimer *mStdinTimer;

 public slots:
  void clipTimeout();
  void stdinTimeout();
  void clipHackTimeout();
  void clipboardChanged();

 private:  
  bool mHandling;
  bool mExiting;
  bool mUseStdin;
  bool mDisconnected;
  QString mSavedClipboard;
};

#if defined(_WIN32) && defined(QT_THREAD_SUPPORT)
#include <qthread.h>
#include <qmutex.h>

class StdinThread : public QThread
{
 public:
  StdinThread() {};
  ~StdinThread() {};

 protected:
  void run();
};
#endif


#endif /* IMOD_CLIENT_MESSAGE_H */
/*
$Log$
Revision 3.23  2009/08/11 15:52:24  mast
Message for delete on all sec

Revision 3.22  2009/03/26 05:40:14  mast
Changes for new ghost message

Revision 3.21  2009/01/16 05:11:48  mast
Switched to fancy new singleshot static for mcliptimer

Revision 3.20  2009/01/15 16:33:17  mast
Qt 4 port

Revision 3.19  2008/11/29 22:08:16  mast
Beadfixer message

Revision 3.18  2008/07/16 04:31:19  mast
Added new messages

Revision 3.17  2008/02/06 20:28:37  mast
Added flag to keep track of whether stderr was disconnected

Revision 3.16  2006/07/03 19:52:21  mast
Add disconnect methods

Revision 3.15  2006/07/03 04:11:21  mast
New beadfixer mode message

Revision 3.14  2006/06/20 17:28:23  mast
include thread only if Windows

Revision 3.13  2006/06/19 05:30:38  mast
Added thread and timer for using standard input

Revision 3.12  2006/02/13 05:09:13  mast
Added beadfixer messages

Revision 3.11  2004/09/24 17:59:02  mast
Added plugin definitions

Revision 3.10  2004/08/12 17:15:04  mast
Added message to get slicer angles

Revision 3.9  2004/05/31 02:15:15  mast
Added messages for setting object properties

Revision 3.8  2004/05/05 17:32:46  mast
Added message to get rubberband coordinates

Revision 3.7  2004/04/28 23:51:26  mast
Added message to open zap

Revision 3.6  2003/11/12 18:48:55  mast
Added method to get relevant window ID

Revision 3.5  2003/10/02 01:30:22  mast
Added message to open bead fixer

Revision 3.4  2003/08/01 05:52:54  mast
*** empty log message ***

Revision 3.3  2003/06/04 23:42:54  mast
Move message defines here to avoid recompiling everything

Revision 3.2  2003/02/27 19:22:40  mast
Qt version that works on windows

Revision 3.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 3.0.2.1  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 3.0  2002/09/27 20:35:04  rickg
Initital version of code moved from imod_menu_cb.c

*/
