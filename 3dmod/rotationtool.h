/*   rotationtool.h  -  declarations for rotationtool.cpp 
 *  $Id$
 */
#ifndef ROTATIONTOOL_H
#define ROTATIONTOOL_H

#include <qwidget.h>

class QIcon;
class QToolButton;
class QLabel;

class RotationTool : public QWidget
{
  Q_OBJECT

 public:
  RotationTool(QWidget *parent, QIcon *centerIcon, const char *centerTip, int size,
               bool autoRaise, float stepSize);
  ~RotationTool() {};
  void setCenterState(bool state);
  void setStepLabel(float step);

 protected:
  void closeEvent(QCloseEvent *e);
  void keyPressEvent(QKeyEvent *e) {emit keyPress(e);};
  void keyReleaseEvent(QKeyEvent *e) {emit keyRelease(e);};

 signals:
  void rotate(int deltaX, int deltaY, int deltZ);
  void stepChanged(int delta);
  void centerButToggled(bool state);
  void closing();
  void keyPress(QKeyEvent *e);
  void keyRelease(QKeyEvent *e);

  public slots:
  void centerToggled(bool state);
  void buttonClicked(int which);

 private:
  QToolButton *mCenterBut;
  QLabel *mStepLabel;
};
    

#endif
