/*   dialog_frame.h  -  declarations for dialog_frame.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
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
  DialogFrame(QWidget *parent, int numButtons, const char *labels[], const char *tips[],
              bool equalSized, const char *caption, const char *fallback,
              const char *name = 0, Qt::WFlags fl = Qt::Window);
  DialogFrame(QWidget *parent, int numButtons, int numRows, const char *labels[], 
              const char *tips[], bool equalSized, bool rounded, const char *caption,
              const char *fallback, const char *name = 0,
              Qt::WFlags fl = Qt::Window);
  ~DialogFrame() {};

 signals:
  void actionPressed(int which);
  void actionClicked(int which);

  public slots:
    void actionButtonPressed(int which);
    void actionButtonClicked(int which);

 protected:
  virtual void changeEvent(QEvent *e);
  QVBoxLayout *mLayout;
  QPushButton *mButtons[BUTTON_ARRAY_MAX];
  int mNumButtons;
  bool mRoundedStyle;

 private:
  void dfSetFontDependentWidths();
  void makeDialogFrame(QWidget *parent, int numButtons, int numRows,
                       const char *labels[], const char *tips[], bool equalSized,
                       bool rounded, const char *caption, const char *fallback,
                       Qt::WFlags fl = Qt::Window);
  bool mEqualSized;
};
#endif

