/*   dialog_frame.h  -  declarations for dialog_frame.cpp
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

#ifndef DIALOG_FRAME_H
#define DIALOG_FRAME_H
#include <qwidget.h>

#define BUTTON_ARRAY_MAX 10

class QVBoxLayout;
class QPushButton;

class DialogFrame : public QWidget
{
  Q_OBJECT

 public:
  DialogFrame(QWidget *parent, int numButtons, char *labels[], char *tips[],
	      bool equalSized, char *caption, char *fallback,
	      const char *name = 0, 
	      WFlags fl = Qt::WDestructiveClose | Qt::WType_TopLevel);
  ~DialogFrame() {};
  void setFontDependentWidths();

 signals:
  void actionPressed(int which);

  public slots:
    void actionButtonPressed(int which);

 protected:
  void fontChange(const QFont &oldFont);
  QVBoxLayout *mLayout;
  QPushButton *mButtons[BUTTON_ARRAY_MAX];

 private:
  bool mEqualSized;
  int mNumButtons;
};
#endif
