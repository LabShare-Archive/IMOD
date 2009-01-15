/*   dialog_frame.h  -  declarations for dialog_frame.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef DIALOG_FRAME_H
#define DIALOG_FRAME_H
#include <qwidget.h>
#include "dllexport.h"

#define BUTTON_ARRAY_MAX 10

class QVBoxLayout;
class QPushButton;

class DLL_IM_EX DialogFrame : public QWidget
{
  Q_OBJECT

 public:
  DialogFrame(QWidget *parent, int numButtons, char *labels[], char *tips[],
              bool equalSized, char *caption, char *fallback,
              const char *name = 0, Qt::WFlags fl = Qt::Window);
  DialogFrame(QWidget *parent, int numButtons, int numRows, char *labels[], 
              char *tips[], bool equalSized, bool rounded, char *caption,
              char *fallback, const char *name = 0,
              Qt::WFlags fl = Qt::Window);
  ~DialogFrame() {};
  void setFontDependentWidths();

 signals:
  void actionPressed(int which);
  void actionClicked(int which);

  public slots:
    void actionButtonPressed(int which);
    void actionButtonClicked(int which);

 protected:
  virtual void fontChange(const QFont &oldFont);
  QVBoxLayout *mLayout;
  QPushButton *mButtons[BUTTON_ARRAY_MAX];
  int mNumButtons;
  bool mRoundedStyle;

 private:
  void makeDialogFrame(QWidget *parent, int numButtons, int numRows,
                       char *labels[], char *tips[], bool equalSized,
                       bool rounded, char *caption, char *fallback,
                       Qt::WFlags fl = Qt::Window);
  bool mEqualSized;
};
#endif

/*

$Log$
Revision 3.7  2004/11/04 23:31:07  mast
Changes for rounded button style

Revision 3.6  2004/06/23 03:35:15  mast
Changed to allow multiple rows of buttons

Revision 3.5  2004/06/04 02:57:28  mast
Implement export/import macro for making libdiaqt be a DLL

Revision 3.4  2004/01/22 19:06:35  mast
Added actionClicked signal

Revision 3.3  2003/03/26 06:23:15  mast
Make fontChange virtual

Revision 3.2  2003/03/24 17:43:48  mast
Accommodate font changes

Revision 3.1  2003/02/10 20:57:02  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:36:52  mast
includes for library

Revision 1.1.2.4  2003/01/23 19:55:42  mast
switch from button pressed to clicked

Revision 1.1.2.3  2003/01/18 01:08:10  mast
add tooltips

Revision 1.1.2.2  2003/01/01 05:45:15  mast
rationalizing toplevel versus dialog style

Revision 1.1.2.1  2002/12/29 04:15:04  mast
Initial creation

*/
