/*   multislider.h  -  declarations for multislider.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 * 
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef MULTISLIDER_H
#define MULTISLIDER_H
#include <qobject.h>
//Added by qt3to4:
#include <QLabel>
#include "dllexport.h"

class QSlider;
class QBoxLayout;
class QLabel;

class DLL_IM_EX MultiSlider : public QObject
{
  Q_OBJECT

 public:
  MultiSlider(QWidget *parent, int numSliders, const char *titles[], int minVal = 0,
              int maxVal = 255, int decimals = 0, bool horizontal = false);
  ~MultiSlider();

  void setValue(int slider, int value);
  void setRange(int slider, int minVal, int maxVal);
  void setMinMaxVal(int slider, int minVal, int maxVal, int value);
  QSlider *getSlider(int slider);
  void setEnabled(int slider, bool enabled);
  QBoxLayout *getLayout() {return mBigLayout;};
  void setDecimals(int slider, int decimals);
  void showWidgets(int slider, bool show);

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
  QBoxLayout *mBigLayout;
  QSlider **mSliders;
  QLabel **mLabels;
  QLabel **mTitleLabels;
  int *mDecimals;
  bool mHorizontal;
};
#endif
/*  

$Log$
Revision 3.8  2009/01/15 16:31:02  mast
Qt 4 port

Revision 3.7  2008/06/25 21:21:57  mast
Added method for show/hide

Revision 3.6  2008/01/19 23:09:41  mast
Cleanup comment problem in log


Revision 3.4  2008/01/17 22:31:12  mast
Added call to enable all components

Revision 3.3  2007/06/29 21:07:58  sueh
bug# 1021 Allow horizontal slider lists.

Revision 3.2  2004/06/04 02:57:28  mast
Implement export/import macro for making libdiaqt be a DLL

Revision 3.1  2003/02/10 20:57:02  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:37:04  mast
includes for library

Revision 1.1.2.2  2003/01/01 05:39:50  mast
add decimal point capability for value output

Revision 1.1.2.1  2002/12/27 01:19:47  mast
Initial creation

*/
