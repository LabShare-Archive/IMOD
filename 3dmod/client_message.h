/*
 * client_message.h
 *
 *  Original Author: David Mastronarde   email: mast@colorado.edu
 *  $Id$
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
#define MESSAGE_ZAP_HQ_MODE    18
#define MESSAGE_OPEN_DIALOGS   19
// REMEMBER TO UPDATE requiredArgs in ImodClipboard::executeMessage FOR NEW MESSAGES

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
  ImodClipboard(bool useStdin, bool willLoadImages);
  ~ImodClipboard() {};
  bool handleMessage();
  bool executeMessage();
  void sendResponse(int succeeded);
  unsigned int ourWindowID();
  void startDisconnect();
  int waitForDisconnect();
  bool disconnectedFromStderr() {return mDisconnected;};
  void doneWithLoad();

  QTimer *mClipHackTimer;
  QTimer *mStdinTimer;

 public slots:
  void clipTimeout();
  void stdinTimeout();
  void clipHackTimeout();
  void clipboardChanged();

 private:  
  bool mHandling;
  int mExiting;
  bool mUseStdin;
  bool mDisconnected;
  int mDeferredHandling;
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
