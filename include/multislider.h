/*   multislider.h  -  declarations for multislider.cpp
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

Revision 1.1.2.1  2003/01/26 20:37:04  mast
includes for library

Revision 1.1.2.2  2003/01/01 05:39:50  mast
add decimal point capability for value output

Revision 1.1.2.1  2002/12/27 01:19:47  mast
Initial creation

*/

#ifndef MULTISLIDER_H
#define MULTISLIDER_H
#include <qobject.h>
#include "dllexport.h"

class QSlider;
class QVBoxLayout;
class QLabel;

class DLL_IM_EX MultiSlider : public QObject
{
  Q_OBJECT

 public:
  MultiSlider(QWidget *parent, int numSliders, char *titles[], int minVal = 0,
              int maxVal = 255, int decimals = 0);
  ~MultiSlider();

  void setValue(int slider, int value);
  void setRange(int slider, int minVal, int maxVal);
  QSlider *getSlider(int slider);
  QVBoxLayout *getLayout() {return mBigLayout;};
  void setDecimals(int slider, int decimals);

 signals:
  void sliderChanged(int slider, int value, bool dragging);

  public slots:
    void valueChanged(int value);
  void sliderActive(int which);
  void sliderPressed(int which) {mPressed[which] = true;};
  void sliderReleased(int which);


 private:
  void processChange();  // Process a changed value if both signals are in
  void displayValue(int slider, int value);

  int mNumSliders;     // Number of sliders
  int mNewValue;        // incoming new value
  int mActiveSlider;    // Slider that changed
  bool *mPressed;       // pressed flags
  QVBoxLayout *mBigLayout;
  QSlider **mSliders;
  QLabel **mLabels;
  int *mDecimals;
};
#endif
