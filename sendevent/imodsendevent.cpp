/*  
 *  imodsendevent.c -- To send messages to 3dmod via the clipboard
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <qclipboard.h>
#include <qdatetime.h>
//Added by qt3to4:
#include <QTimerEvent>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <imodconfig.h>
#include <imodsendevent.h>

#define LATIN1(a) ((const char *)a.toLatin1())

static int winID;
static int debugOut = 0;
static int retryLimit = 0;
static int retryCount = 0;
static QString timeStr;
static QString cmdStr;

int main(int argc, char **argv)
{
  int action;
  double timeout = 5.;
  int argIndex, numArgs;
  int interval;
  char *endptr;
  QString qstr;
  QTime curTime = QTime::currentTime();
  int timeStamp = 60000 * curTime.minute() + 1000 * curTime.second() +
    curTime.msec();

  for (argIndex = 1; argIndex < argc - 1 ; argIndex++) {
    if (argv[argIndex][0] == '-'){
      switch (argv[argIndex][1]){
        
      case 't': /* timeout interval */
        timeout = strtod(argv[++argIndex], &endptr);
        if (endptr - argv[argIndex] < (int)strlen(argv[argIndex])) {
          fprintf(stderr, "ERROR: imodsendevent - invalid timeout entry %s\n",
                  argv[argIndex]);
          exit(3);
        }
        break;

      case 'D': /* debug */
        debugOut = 1;
        break;

      default:
        fprintf(stderr, "ERROR: imodsendevent - invalid argument %s\n",
                argv[argIndex]);
        exit(3);
        break;
      } 
    } else
      break;
  }

  numArgs = argc - argIndex;

  if (numArgs < 2) {
    fprintf(stderr, "ERROR: imodsendevent - Wrong number of arguments\n"
            "   Usage: imodsendevent [-t timeout] [-D] Window_ID action "
            "[arguments]\n");
    exit(3);
  }

  // Check the arguments for odd characters
  winID = strtol(argv[argIndex], &endptr, 10);
  if (endptr - argv[argIndex] < (int)strlen(argv[argIndex])) {
    fprintf(stderr, "ERROR: imodsendevent - invalid characters in window ID"
            " entry %s\n", argv[argIndex]);
    exit(3);
  }
  action = strtol(argv[argIndex + 1], &endptr, 10);
  if (endptr - argv[argIndex + 1] < (int)strlen(argv[argIndex + 1])) {
    fprintf(stderr, "ERROR: imodsendevent - invalid characters in action "
            "entry %s\n", argv[argIndex + 1]);
    exit(3);
  }

  // Create the application
  ImodSendEvent a(argc, argv);
  setlocale(LC_NUMERIC, "C");

  // Pack the arguments into a QString
  timeStr.sprintf(" %d ", timeStamp);
  timeStr = QString(argv[argIndex]) + timeStr; 
  cmdStr = QString(argv[argIndex + 1]);
  for (; argIndex + 2 < argc; argIndex++)
    cmdStr += QString(" ") + QString(argv[argIndex + 2]);
  qstr = timeStr + cmdStr;

  // Connect to the clipboard, start the timeout, and send the text
  QClipboard *cb = QApplication::clipboard();

  // Default for setText and text() is Clipboard mode
  //cb->setSelectionMode(false);
  cb->blockSignals(true);
  QObject::connect(cb, SIGNAL(dataChanged()), &a, SLOT(clipboardChanged()));
  cb->blockSignals(false);

  // Start a timeout timer if the interval is not zero.
  interval = (int)(1000. * timeout + 0.5);
  if (interval > 0) {

    // If this hack is defined and > 0, divide the total timeout into intervals 
    // of this length and retry sending message
#ifdef SENDEVENT_RETRY_HACK
    if (SENDEVENT_RETRY_HACK > 0) {
      retryLimit = interval / SENDEVENT_RETRY_HACK;
      if (!retryLimit)
        retryLimit = 1;
      interval = SENDEVENT_RETRY_HACK;
    }
#endif
    a.startTimer(interval);
  }

  if (debugOut)
    fprintf(stderr, "Imodsendevent sending: %s\n", LATIN1(qstr));
  cb->setText(qstr);

  // If the hack is defined as zero, just process events and set the text again
#ifdef SENDEVENT_RETRY_HACK
  if (SENDEVENT_RETRY_HACK == 0) {
    qApp->processEvents();
    cb->setText(qstr);
  }
#endif

  return a.exec();

}

ImodSendEvent::ImodSendEvent(int &argc, char **argv)
  : QApplication(argc, argv)
{
}

// The timeout occurred - if count has not expired, resend message with an
// added space, otherwise write error message and exit with error
void ImodSendEvent::timerEvent(QTimerEvent *e)
{
  if (++retryCount < retryLimit) {
    timeStr += " ";
    QString qstr = timeStr + cmdStr;
    QClipboard *cb = QApplication::clipboard();
    //cb->setSelectionMode(false);
    if (debugOut)
      fprintf(stderr, "Imodsendevent - resending %s \n", LATIN1(qstr));
    cb->setText(qstr);
    return;
  }
  fprintf(stderr, "ERROR: imodsendevent - timeout before response received "
          "from target 3dmod\n");
  QApplication::exit(2);
}

// The clipboard changed - check for window ID and OK or ERROR
// NOTE: on Windows, Qt 3 needed to exit with ::exit instead of
// QApplication::exit,  otherwise it would hang for a long time or until the 
// timeout occurred.  It complains in Qt 4 about deleting an object during its
// event handler, but qapp::exit seems fine now.
void ImodSendEvent::clipboardChanged()
{
  QClipboard *cb = QApplication::clipboard();
  //cb->setSelectionMode(false);
  QString text = cb->text();
  if (debugOut)
    fprintf(stderr, "Imodsendevent - clipboard = %s\n", LATIN1(text));

  // Ignore if empty text, no spaces, starting space, or wrong ID
  if (text.isEmpty())
    return;
  int index = text.indexOf(" ");
  if (index <= 0)
    return;
  if ((text.left(index)).toInt() != winID) 
    return;

  // If OK, just exit; if ERROR, give message and exit with error status
  if (text.mid(index + 1) == "OK") {
    QApplication::exit(0);
    return;
  }
  if (text.mid(index + 1) != "ERROR")
    return;
  fprintf(stderr, "ERROR: imodsendevent - message received but error occurred "
          "executing it\n");
  QApplication::exit(3);
}
