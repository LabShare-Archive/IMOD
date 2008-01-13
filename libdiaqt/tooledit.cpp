/*
 *  tooledit.cpp  - A subclass of QLineEdit that sends a signal when it loses 
 *                  focus.  It can also be set to a fixed width in columns.
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include "tooledit.h"

/*!
 * A line edit widget that sends a signal, focusLost(), when it loses input 
 * focus, and that can be set to fixed column width by providing the number 
 * of characters in [columns].  [name] and [columns] default to 0.
 */
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

/*!
 * Set the edit box to fit the number of characters in [columns], or resize
 * it to fit a previously specified number if [columns] is zero
 */
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

/*

$Log$
Revision 1.5  2007/08/26 06:55:59  mast
Documentation changes

Revision 1.4  2003/03/26 06:23:45  mast
Adjust to font changes

Revision 1.3  2003/02/28 21:34:58  mast
rename lostFocus to focusLost to avoid conflict with signals in 3.1

Revision 1.2  2003/02/10 20:51:22  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:35:32  mast
adding as library file

Revision 1.1.2.4  2002/12/17 17:35:53  mast
Added column width setting

*/
