/*   mv_stereo.h  -  declarations for mv_stereo.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMODV_STEREO_H
#define IMODV_STEREO_H

#include <qgl.h>
//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>

typedef struct __imodv_struct ImodvApp;

// These MUST be maintained with these numbers for combo box indexes
#define IMODV_STEREO_OFF 0
#define IMODV_STEREO_RL  1
#define IMODV_STEREO_TB  2
#define IMODV_STEREO_HW  3

/* Stereo Control functions. */
void imodvStereoEditDialog(ImodvApp *a, int state);
void imodvStereoUpdate(void);
void imodvStereoToggle(void);
void stereoHWOff(void);
void stereoDrawBuffer(GLenum mode);
void imodvStereoClear(void);
int imodvStereoVoffset(void);

#include "dialog_frame.h"
class MultiSlider;
class QComboBox;
class QCheckBox;
class QSpinBox;

class ImodvStereo : public DialogFrame
{
  Q_OBJECT

 public:
  ImodvStereo(QWidget *parent, const char *name = NULL);
  ~ImodvStereo() {};

  void update();
  QComboBox *mComboBox;
  MultiSlider *mSlider;
  QCheckBox *mImageBox;
  QSpinBox *mViewsAreaSpin;
  QSpinBox *mDeltaZspin;

  public slots:
    void newOption(int item);
  void sliderMoved(int which, int value, bool dragging);
  void imageToggled(bool state);
  void viewsChanged(int value);
  void deltaChanged(int value);
  void buttonPressed(int which);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void changeEvent(QEvent *e);

 private:
  bool mCtrlPressed;
};

#endif
