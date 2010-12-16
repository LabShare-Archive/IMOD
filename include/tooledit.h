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
Revision 3.5  2009/01/15 16:31:02  mast
Qt 4 port

Revision 3.4  2004/06/04 03:00:42  mast
Implement export/import macro for making libdiaqt be a DLL

Revision 3.3  2003/03/26 06:23:32  mast
Adjust to font changes

Revision 3.2  2003/02/28 21:35:11  mast
renamed lostFocus

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
//Added by qt3to4:
#include <QFocusEvent>
#include "dllexport.h"

class DLL_IM_EX ToolEdit : public QLineEdit
{
  Q_OBJECT

 public:
  ToolEdit( QWidget * parent, int columns = 0, const char * name = 0 );
  ~ToolEdit();
  void setColumnWidth(int columns = 0);

 signals:
  void focusLost();

 public slots:
  void doneEditing();

 protected:
  void fontChange(const QFont &oldFont);

 private:
  int mColumns;    // Number of columns it is sized for, of 0 if not

};
#endif
