/*  IMOD VERSION 2.7.9
 *
 *  tooledit.cpp  - A subclass of QLineEdit that sends a signal when it loses 
 *                  focus.  It can also be set to a fixed width in columns.
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */
/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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
Revision 1.3  2003/02/28 21:34:58  mast
rename lostFocus to focusLost to avoid conflict with signals in 3.1

Revision 1.2  2003/02/10 20:51:22  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:35:32  mast
adding as library file

Revision 1.1.2.4  2002/12/17 17:35:53  mast
Added column width setting

*/

#include "tooledit.h"
ToolEdit::ToolEdit( QWidget * parent, int columns, const char * name)
  : QLineEdit(parent, name)
{
  mColumns = 0;
  setColumnWidth(columns);
}

ToolEdit::~ToolEdit()
{
}

void ToolEdit::focusOutEvent(QFocusEvent *event)
{
  emit focusLost();
}
void ToolEdit::fontChange(const QFont &oldFont)
{
  setColumnWidth();
  QLineEdit::fontChange(oldFont);
}

// Set the edit box to fit a given number of characters, or resize it to fit
// a previously specified number if the argument is zero
void ToolEdit::setColumnWidth(int columns)
{
  int i, width;
  QString str;
  if (!columns)
    columns = mColumns;
  mColumns = columns;
  if (columns) {
    for (i= 0; i < columns; i++)
      str += "8";

    // Need to add 1.5 columns for right-justified edit boxes at least
    width = ((2 * columns + 3) * fontMetrics().width(str) ) / (2 * columns);
    setFixedWidth(width);
  }
}
