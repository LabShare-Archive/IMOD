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

$Log$
Revision 3.3  2002/09/17 18:49:14  mast
Added a signature entry in the action packet because there seem to be
other events around that imod gets

Revision 3.2  2002/09/14 00:13:43  mast
SGI compiler flushed out a bug.

Revision 3.1  2002/09/13 21:56:06  mast
Initial addition to package

*/
#include <qclipboard.h>
#include <qdatetime.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <imodsendevent.h>

static int winID;

int main(int argc, char **argv)
{
  int action;
  double timeout = 5.;
  int argIndex, numArgs;
  char *endptr;
  QString qstr, timeStr;
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
          exit(-1);
        }
        break;

      default:
        fprintf(stderr, "ERROR: imodsendevent - invalid argument %s\n",
                argv[argIndex]);
        exit(-1);
        break;
      } 
    } else
      break;
  }

  numArgs = argc - argIndex;

  if (numArgs < 2 || numArgs > 3 ) {
    fprintf(stderr, "ERROR: imodsendevent - Wrong number of arguments\n"
            "   Usage: imodsendevent [-t timeout] Window_ID action "
            "[text_string]\n");
    exit(-1);
  }

  // Check the arguments for odd characters
  winID = strtol(argv[argIndex], &endptr, 10);
  if (endptr - argv[argIndex] < (int)strlen(argv[argIndex])) {
    fprintf(stderr, "ERROR: imodsendevent - invalid characters in window ID"
            " entry %s\n", argv[argIndex]);
    exit(-1);
  }
  action = strtol(argv[argIndex + 1], &endptr, 10);
  if (endptr - argv[argIndex + 1] < (int)strlen(argv[argIndex + 1])) {
    fprintf(stderr, "ERROR: imodsendevent - invalid characters in action "
            "entry %s\n", argv[argIndex + 1]);
    exit(-1);
  }

  // Create the application
  ImodSendEvent a(argc, argv);

  // Pack the arguments into a QString
  timeStr.sprintf(" %d ", timeStamp);
  qstr = QString(argv[argIndex]) + timeStr + argv[argIndex + 1] + " ";
  if (numArgs > 2)
    qstr += argv[argIndex + 2];

  // Connect to the clpbard, start the timeout, and send the text
  QClipboard *cb = QApplication::clipboard();
  cb->blockSignals(true);
  QObject::connect(cb, SIGNAL(dataChanged()), &a, SLOT(clipboardChanged()));
  if (timeout > 0.)
    a.startTimer((int)(1000. * timeout + 0.5));
  cb->blockSignals(false);
  cb->setText(qstr);

  return a.exec();

}

ImodSendEvent::ImodSendEvent(int &argc, char **argv)
  : QApplication(argc, argv)
{
}

// The timeout occurred - write error message and exit with error
void ImodSendEvent::timerEvent(QTimerEvent *e)
{
  fprintf(stderr, "ERROR: imodsendevent - timeout before response received "
          "from target Imod\n");
  QApplication::exit(2);
}

// The clipboard changed - check for window ID and OK or ERROR
void ImodSendEvent::clipboardChanged()
{
  QClipboard *cb = QApplication::clipboard();
  QString text = cb->text();
  //  fprintf(stderr, "Imodsendevent - clipboard = %s\n", text.latin1());

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
    QApplication::exit(0);
    return;
  }
  if (text.mid(index + 1) != "ERROR")
    return;
  fprintf(stderr, "ERROR: imodsendevent - message received but error occurred "
          "executing it\n");
  QApplication::exit(3);
}
