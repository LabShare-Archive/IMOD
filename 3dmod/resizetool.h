/*   resizetool.h  -  declarations for resizetool.cpp 
 *  $Id$
 */
#ifndef RESIZETOOL_H
#define RESIZETOOL_H

#include "dialog_frame.h"

class QCloseEvent;
class QKeyEvent;
class QSpinBox;
class QEvent;

class ResizeTool : public DialogFrame
{
  Q_OBJECT

 public:
  ResizeTool(QWidget *parent, int sizeX, int sizeY, int step);
  ~ResizeTool() {};
  void newSize(int sizeX, int sizeY);

 protected:
  void closeEvent(QCloseEvent *e);
  void keyPressEvent(QKeyEvent *e) {emit keyPress(e);};
  void keyReleaseEvent(QKeyEvent *e) {emit keyRelease(e);};
  void changeEvent(QEvent *e);

 signals:
  void resize(int sizeX, int sizeY);
  void closing();
  void keyPress(QKeyEvent *e);
  void keyRelease(QKeyEvent *e);

  public slots:
  void xSizeChanged(int value);
  void ySizeChanged(int value);
  void buttonClicked(int which);

 private:
  QSpinBox *xSpinBox;
  QSpinBox *ySpinBox;
};
    

#endif
