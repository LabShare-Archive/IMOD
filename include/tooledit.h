/*   tooledit.h  -  declarations for tooledit.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.1  2003/02/10 20:57:02  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:36:56  mast
includes for library

Revision 1.1.2.3  2002/12/17 17:36:08  mast
added column width setting

*/

#ifndef TOOLEDIT_H
#define TOOLEDIT_H
#include <qlineedit.h>
class ToolEdit : public QLineEdit
{
  Q_OBJECT

 public:
  ToolEdit( QWidget * parent, int columns = 0, const char * name = 0 );
  ~ToolEdit();
  void setColumnWidth(int columns = 0);

 signals:
  void focusLost();

 protected:
  void focusOutEvent(QFocusEvent *event);

 private:
  int mColumns;    // Number of columns it is sized for, of 0 if not

};
#endif
