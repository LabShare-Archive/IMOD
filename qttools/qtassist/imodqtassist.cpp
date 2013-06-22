/*
 *  imodqtassist.cpp - Runs Qt assistant and displays pages entered through
 *                     standard input
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id: processchunks.cpp,v e5ab12a1256a 2011/08/23 05:58:24 sueh 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#ifndef _WIN32
#include <sys/select.h>
#include <sys/time.h>
#endif
#include <qstring.h>
#include <qapplication.h>
#include <QTimerEvent>
#include "imod_assistant.h"
#include "imodqtassist.h"

static ImodAssistant *imodHelp;

#define TIMER_INTERVAL  50
#define MAX_LINE 1024
static char threadLine[MAX_LINE];
static bool gotLine = false;
static int lineLen;

static QMutex mutex;
static AssistantThread * assThread;

static int readLine(char *line);

// MAIN
int main(int argc, char *argv[])
{
  int retval, ind = 1;
  char *qhc = NULL;
  bool absPath = false;
  bool keepBar = false;
  bool prefAbs = false;
  const char *prefix = NULL;

  if (argc == 2 && !strcmp(argv[ind], "-t")) {
    printf("Ideal thread count = %d\n", QThread::idealThreadCount());
    exit(0);
  }

  // Start the application 
  QApplication qapp(argc, argv);
  setlocale(LC_NUMERIC, "C");

  if (argc < 2) {
    fprintf(stderr, "Usage: imodqtassist [options] path_to_help_collection_file\n");
    fprintf(stderr, "   Options:\n");
    fprintf(stderr, "\t-a\tpath_to_help_collection is absolute, not relative to"
            " $IMOD_DIR\n");
    fprintf(stderr, "\t-p file\tName of qhc (help collection) file (IMOD.adp \n "
            "\t\t   can be entered instead of IMOD.qhc)\n");
    fprintf(stderr, "\t-q pref\tPrefix to use in front of help page name\n");
    fprintf(stderr, "\t-b\tPrefix is absolute, not appended to qthelp://bl3demc/IMOD/\n");
    fprintf(stderr, "\t-k\tKeep the sidebar (do not hide it)\n");
    fprintf(stderr, "\t-t\tReport ideal thread count (number of processors) "
            "and exit\n");
    exit(1);
  }

  // Parse arguments
  while (argc - ind > 1) {
    if (argv[ind][0] == '-') {
      switch (argv[ind][1]) {

      case 'a':
        absPath = true;
        break;

      case 'k':
        keepBar = true;
        break;

      case 'p':
        qhc = argv[++ind];
        break;

      case 'q':
        prefix = argv[++ind];
        break;

      case 'b':
        prefAbs = true;
        break;

      default:
        fprintf(stderr, "ERROR: unknown argument %s\n", argv[ind]);
        exit(1);
      }
    } else {
      fprintf(stderr, "ERROR: too many arguments\n");
      exit(1);
    }
    ind++;
  }

  // start the help object
  imodHelp = new ImodAssistant(argv[ind], qhc, NULL, absPath, keepBar, prefix, prefAbs);

  // Get a listener for the errors and timer
  AssistantListener *listener = new AssistantListener();
  QObject::connect(imodHelp, SIGNAL(error(const QString&)), listener,
                   SLOT(assistantError(const QString&)));

  // If using threads, start a thread to read stdin
  assThread = new AssistantThread();
  assThread->start();

  // Start timer to watch for input
  listener->startTimer(TIMER_INTERVAL);
  retval = qapp.exec();

  // Upon exit, delete the help object to close help, and kill thread if
  // needed
  delete imodHelp;

  if (assThread->isRunning())
    assThread->terminate();
  return retval;
}

void AssistantListener::assistantError(const QString &msg)
{
  fprintf(stderr, "WARNING: Qt Assistant generated error: %s\n", 
          (const char *)msg.toLatin1());
  fflush(stderr);
  //QApplication::exit(1);
}


void AssistantListener::timerEvent(QTimerEvent *e)
{
  char line[MAX_LINE];
  int err;

  // 3/23/12: Left these conditionals for historical interest
#ifdef QT_THREAD_SUPPORT

  // If there is thread support, see if the thread got a line
  // Copy the line while we have the mutex
  bool gotOne;
  mutex.lock();
  gotOne = gotLine;
  gotLine = false;
  if (gotOne)
    strcpy(line, threadLine);
  mutex.unlock();
  if (!gotOne)
    return;
#else
#ifdef _WIN32

  // If there is no thread support, Windows fallback is to process events
  // before and after.  This didn't work on second display and shouldn't
  // happen anyway
  qApp->processEvents();
#else

  // Non-windows fallback from threads uses the select operation on stdin
  // Note that it did not work to keep these as statics and get first time only
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
#endif

  // read line here in non-thread case
  lineLen = readLine(line);
#endif

  // Exit if blank line
  if (lineLen == 1 && threadLine[0] == 'q') {
    QApplication::exit(0);
    return;
  }
  if (!lineLen)
    return;

  // Otherwise show page and report results ( err > 0 not possible any more)
  err = imodHelp->showPage(line);
  if (err > 0)
    fprintf(stderr, "WARNING: Page %s not found\n", line);
  else
    fprintf(stderr, "Page %s displayed\n", line);
  if (err < 0 && !mWarned) {
    fprintf(stderr, "WARNING: qhc file not found\n");
    mWarned = true;
  }
    
  fflush(stderr);

#ifndef QT_THREAD_SUPPORT
#ifdef _WIN32
  qApp->processEvents();
#endif
#endif
}

// The thread loops on reading a line and sets the flag when it gets one
void AssistantThread::run()
{
  bool gotOne;
  while (1) {
    mutex.lock();
    gotOne = gotLine;
    mutex.unlock();

    // Do not go for another line until the main thread has cleared the flag
    if (gotOne) {
      usleep(20000);
      continue;
    }
    
    lineLen = readLine(threadLine);
    mutex.lock();
    gotLine = true;
    mutex.unlock();
    if (lineLen == 1 && threadLine[0] == 'q')
      return;
  }
}    

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
