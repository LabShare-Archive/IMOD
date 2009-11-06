/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/*
 * Modified for IMOD by James Kremer and David Mastronarde 
 * Prints to the imod information window.
 *
 *  $Id$
 *  Log at end of file
 */

/* error_test.c -- test the error handlers and wprint() routine
 */
#include <stdio.h>
/*#include <varargs.h>  */
#include <stdarg.h>
#include <qtextedit.h>
#include <qscrollbar.h>
#include <qapplication.h>
#include "imod.h"
#include "preferences.h"
#ifdef _WIN32
#define vsnprintf _vsnprintf
#endif

static QTextEdit *Wprint_text_output = NULL;

void wprintWidget(QTextEdit *edit)
{
     
  Wprint_text_output = edit;
  edit->setReadOnly(true);
  //edit->setTextFormat(Qt::PlainText);
}

/*VARARGS*/
#define MSGBUF_LEN  512
void wprint(const char *fmt, ...)
{
  char msgbuf[MSGBUF_LEN];
  va_list args;
  bool nopos = false;
  int i, len, lastnl;
  int beep = 0;
  static QString str;
  static bool returnPending = false;
  static int lastbeep = 0;
  QString msgstr;

  if (!Wprint_text_output)
    return;

  va_start (args, fmt);
  /*     fmt = va_arg (args, char *); */

  len = strlen(fmt);
  for(i = 0; i < len; i++)
    if (fmt[i] == 0x07) {
      if (!ImodPrefs->silentBeep())
	QApplication::beep();
      beep = 1;
    }

  vsnprintf (msgbuf, MSGBUF_LEN - 1, fmt, args);

  // 3/31/09: removed alternative calls if no vsprintf
  va_end (args);
     
  msgstr = msgbuf;
  while ((i = msgstr.indexOf(0x07)) >= 0)
    msgstr = msgstr.remove(i, 1);

  // Strip \r's
  while ((lastnl = msgstr.indexOf('\r')) >= 0) {
    //    fprintf(stderr,"stripping ctrl-r at %d\n", lastnl);
    msgstr.remove(lastnl, 1);
    nopos = true;
  }

  // Add a \n if one was pending
  if (returnPending) {
    str += '\n';
  }

  // If there was a \r, remove the last line from existing string
  if (nopos && !str.isEmpty()) {
    lastnl = str.lastIndexOf('\n');
    //    fprintf(stderr, "backing up to %d\n", lastnl);
    if (lastnl >= 0)
      str.truncate(lastnl + 1);   // Truncate takes a length not an index!
    else
      str = "";
    Wprint_text_output->setText(str);
  }


  // If string ends with \n, remove it and set it as pending
  returnPending = msgstr.endsWith("\n");
  if (returnPending)
    msgstr.remove(msgstr.length() - 1, 1);
  
  //  fprintf(stderr, "%s-*-%s",  LATIN1(str), LATIN1(msgstr));
  str += msgstr;
  //  Wprint_text_output->setText(str);

#if QT_VERSION >= 0x040400
  if (beep) {
    Wprint_text_output->setTextBackgroundColor
      (lastbeep ? "yellow" : "magenta");
    lastbeep = 1 - lastbeep;
  } else if (lastbeep) {
    Wprint_text_output->setTextBackgroundColor("white");
    lastbeep = 0;
  }
#endif
  if (returnPending)
    Wprint_text_output->append(msgstr);
  else
    Wprint_text_output->setText(str);
  QScrollBar *scroll = Wprint_text_output->verticalScrollBar();
  scroll->setValue(scroll->maximum());
}

/*

$Log$
Revision 4.9  2009/04/01 03:24:43  mast
Switched to vsnprintf call to prevent buffer overruns

Revision 4.8  2009/01/15 16:33:18  mast
Qt 4 port

Revision 4.7  2008/02/06 16:14:21  sueh
bug# 1065 Changed wprint(char *) to wprint(const char*).

Revision 4.6  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.5  2003/10/01 05:02:05  mast
Need to define as extern C for plugins to use

Revision 4.4  2003/04/17 21:50:31  mast
Using older remove statement for QString

Revision 4.3  2003/04/17 19:05:26  mast
eliminate Ctrl-A from text strings

Revision 4.2  2003/03/24 17:58:09  mast
Changes for new preferences capability

Revision 4.1  2003/02/10 20:29:02  mast
autox.cpp

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/13 01:19:04  mast
Qt versions

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/
