/*  IMOD VERSION 2.7.2
 *
 *  imodsendevent.c -- To send ClientMessage events to imod
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <qclipboard.h>
#include <qdatetime.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <imodconfig.h>
#include <imodsendevent.h>

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

  // Pack the arguments into a QString
  timeStr.sprintf(" %d ", timeStamp);
  timeStr = QString(argv[argIndex]) + timeStr; 
  cmdStr = QString(argv[argIndex + 1]);
  for (; argIndex + 2 < argc; argIndex++)
    cmdStr += QString(" ") + QString(argv[argIndex + 2]);
  qstr = timeStr + cmdStr;

  // Connect to the clipboard, start the timeout, and send the text
  QClipboard *cb = QApplication::clipboard();
  cb->setSelectionMode(false);
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
    fprintf(stderr, "Imodsendevent sending: %s\n", qstr.latin1());
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
    cb->setSelectionMode(false);
    if (debugOut)
      fprintf(stderr, "Imodsendevent - resending %s \n", qstr.latin1());
    cb->setText(qstr);
    return;
  }
  fprintf(stderr, "ERROR: imodsendevent - timeout before response received "
          "from target 3dmod\n");
  ::exit(2);
}

// The clipboard changed - check for window ID and OK or ERROR
// NOTE: on Windows, needed to exit with ::exit instead of QApplication::exit,
// otherwise it would hang for a long time or until the timeout occurred.
// So do all exiting with ::exit()
void ImodSendEvent::clipboardChanged()
{
  QClipboard *cb = QApplication::clipboard();
  cb->setSelectionMode(false);
  QString text = cb->text();
  if (debugOut)
    fprintf(stderr, "Imodsendevent - clipboard = %s\n", text.latin1());

  // Ignore if empty text, no spaces, starting space, or wrong ID
  if (text.isEmpty())
    return;
  int index = text.find(" ");
  if (index <= 0)
    return;
  if ((text.left(index)).toInt() != winID) 
    return;

  // If OK, just exit; if ERROR, give message and exit with error status
  if (text.mid(index + 1) == "OK") {
    ::exit(0);
    return;
  }
  if (text.mid(index + 1) != "ERROR")
    return;
  fprintf(stderr, "ERROR: imodsendevent - message received but error occurred "
          "executing it\n");
  ::exit(3);
}

/*
$Log$
Revision 1.8  2004/06/04 03:14:46  mast
Fixed probelm with trailing space after filename

Revision 1.7  2004/05/31 19:54:00  mast
change name in error message from imod to 3dmod

Revision 1.6  2004/05/31 02:17:40  mast
Allowed multiple arguments after the action

Revision 1.5  2004/04/18 22:33:45  mast
Allowed SENDEVENT_RETRY_HACK to be defined as zero to set data a second time
after processing events.

Revision 1.4  2003/09/24 16:20:37  mast
Add include of imodconfig.h and add resending message for debug output

Revision 1.3  2003/09/24 15:05:58  mast
Switched to ::exit(), set selection mode false, added retry

Revision 1.2  2003/09/13 16:17:36  mast
Added option for debug output

Revision 1.1  2003/02/27 00:55:39  mast
qt clipboard version

Revision 3.3  2002/09/17 18:49:14  mast
Added a signature entry in the action packet because there seem to be
other events around that imod gets

Revision 3.2  2002/09/14 00:13:43  mast
SGI compiler flushed out a bug.

Revision 3.1  2002/09/13 21:56:06  mast
Initial addition to package

*/
