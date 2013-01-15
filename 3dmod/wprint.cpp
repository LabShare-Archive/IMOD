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
#include <qfile.h>
#include <qtextstream.h>
#include "imod.h"
#include "preferences.h"
#ifdef _WIN32
#define vsnprintf _vsnprintf
#endif

static QTextEdit *wprintText = NULL;

void wprintWidget(QTextEdit *edit)
{
     
  wprintText = edit;

  // 3/31/10: Allow editing
  //edit->setReadOnly(true);
  edit->setTextInteractionFlags(Qt::TextEditorInteraction);
  //edit->setTextFormat(Qt::PlainText);
}

// So that info window can pass on Ctrl-C and Ctrl-V for copy and paste
void wprintCopy(void)
{
  if (wprintText)
    wprintText->copy();
}

void wprintPaste(void)
{
  if (wprintText)
    wprintText->paste();
}

/*VARARGS*/
#define MSGBUF_LEN  512
void wprint(const char *fmt, ...)
{
  char msgbuf[MSGBUF_LEN];
  va_list args;
  bool nextBackup;
  bool lineEnds;
  int i, len, lastnl;
  int beep = 0;
  static bool needInsert = false;
  static bool needBackup = false;
  static int lastbeep = 0;
  static int yellow = 0;
  QString msgstr;

  if (!wprintText)
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

  // Strip \r's after recording their purpose
  nextBackup = msgstr.endsWith("\r");
  if (msgstr.startsWith("\r"))
    needBackup = true;

  while ((lastnl = msgstr.indexOf('\r')) >= 0) {
    //fprintf(stderr,"stripping ctrl-r at %d\n", lastnl);
    msgstr.remove(lastnl, 1);
  }
  //fprintf(stderr,"needBackup %d  nextBackup %d\n", needBackup, nextBackup);

  // Then remove last block of text if needed
  if (needBackup) {
    wprintText->moveCursor(QTextCursor::End);
    wprintText->moveCursor(QTextCursor::StartOfBlock, QTextCursor::KeepAnchor);
    
    /* Initial notes were that using a QTextCursor to removeSelectedText 
       didn't work all the time. But cut() puts it on the clipboard which is 
       fatal for messaging in Windows.  Didn't see a problem after switching
       to this*/
    wprintText->textCursor().removeSelectedText();
    needInsert = true;
  }

  // If string ends with \n, remove it and keep track that line ended
  //fprintf(stderr, "- %s -", LATIN1(msgstr));
  lineEnds = msgstr.endsWith("\n");
  if (lineEnds)
    msgstr.remove(msgstr.length() - 1, 1);

  if (needInsert) {
    wprintText->moveCursor(QTextCursor::End);
    wprintText->insertPlainText(msgstr);
  } else {

    // Change color as needed when starting a new paragraph
#if QT_VERSION >= 0x040400
    if (beep) {
      wprintText->setTextBackgroundColor(yellow ? "yellow" : "magenta");
      yellow = 1 - yellow;
    } else if (lastbeep) {
      wprintText->setTextBackgroundColor("white");
    }
    lastbeep = beep;
#endif
    wprintText->append(msgstr);
  }

  // Set needInsert for next time depending on whether this line ends
  needInsert = !lineEnds;
  needBackup = nextBackup;
  QScrollBar *scroll = wprintText->verticalScrollBar();
  scroll->setValue(scroll->maximum());
}

/*
 * Write the edit field to a file
 */
void wprintWriteFile(void)
{
  QString str;
  QString qname = imodPlugGetSaveName(NULL, "File to save info text panel into:");
  if (qname.isEmpty())
      return;
  imodBackupFile((char *)LATIN1(qname));
  QFile file(qname);
  if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
    wprint("\aCould not open %s", LATIN1(qname));
    return;
  }
  QTextStream stream(&file);
  str = wprintText->toPlainText();
  stream << str;
  if (!str.endsWith("\n"))
    stream << "\n";
  file.close();
}

/*

$Log$
Revision 4.12  2010/04/19 23:52:15  mast
Switched to method that does not splat on clipboard

Revision 4.11  2010/04/01 02:31:30  mast
Changed to edit the text document directly and appropriately, which was needed
with the text edit editable, and added a call to save to file

Revision 4.10  2009/11/06 20:32:53  mast
Disable the one incompatibiity with Qt 4.3

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
