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
 * Modified for IMOD by James Kremer. 
 * Prints to the imod information window.
 *
 */
/*  $Author$

$Date$

$Revision$

$Log$
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

/* error_test.c -- test the error handlers and wprint() routine
 */
#include <stdio.h>
/*#include <varargs.h>  */
#include <stdarg.h>
#include <qtextedit.h>
#include <qapplication.h>
#include "imod.h"
#include "preferences.h"

static QTextEdit *Wprint_text_output = NULL;

void wprintWidget(QTextEdit *edit)
{
     
  Wprint_text_output = edit;
  edit->setReadOnly(true);
  edit->setTextFormat(Qt::PlainText);
}

/*VARARGS*/
void wprint(char *fmt, ...)
{
  char msgbuf[1000];
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

#ifndef NO_VPRINTF
  (void) vsprintf (msgbuf, fmt, args);
#else /* !NO_VPRINTF */
  {
    FILE foo;
    foo._cnt = 256;
    foo._base = foo._ptr = msgbuf; /* (unsigned char *) ?? */
    foo._flag = _IOWRT+_IOSTRG;
    (void) _doprnt (fmt, args, &foo);
    *foo._ptr = '\0'; /* plant terminating null character */
  }
#endif /* NO_VPRINTF */
  va_end (args);
     
  msgstr = msgbuf;
  while ((i = msgstr.find(0x07)) >= 0)
    msgstr = msgstr.remove(i, 1);

  // Strip \r's
  while ((lastnl = msgstr.find('\r')) >= 0) {
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
    lastnl = str.findRev('\n');
    //    fprintf(stderr, "backing up to %d\n", lastnl);
    if (lastnl >= 0)
      str.truncate(lastnl + 1);   // Truncate takes a length not an index!
    else
      str = "";
  }


  // If string ends with \n, remove it and set it as pending
  returnPending = msgstr.endsWith("\n");
  if (returnPending)
    msgstr.remove(msgstr.length() - 1, 1);
  
  //  fprintf(stderr, "%s-*-%s",  str.latin1(), msgstr.latin1());
  str += msgstr;
  Wprint_text_output->setText(str);
  Wprint_text_output->scrollToBottom();

  if (beep) {
    len = Wprint_text_output->paragraphs();
    lastnl = len - 4;
    if (lastnl < 0)
      lastnl = 0;
    for (i = lastnl; i < len; i++)
      Wprint_text_output->setParagraphBackgroundColor
        (i, lastbeep ? "yellow" : "magenta");
    lastbeep = 1 - lastbeep;
  } else
    lastbeep = 0;
}


