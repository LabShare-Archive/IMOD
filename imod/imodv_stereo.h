/*   imodv_stereo.h  -  declarations for imodv_stereo.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1.2.3  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.2  2003/01/01 05:45:50  mast
Qt version

Revision 1.1.2.1  2002/12/18 04:10:30  mast
initial creation

*/

#ifndef IMODV_STEREO_H
#define IMODV_STEREO_H

#include <GL/gl.h>

typedef struct __imodv_struct ImodvApp;

#define IMODV_STEREO_OFF 0
#define IMODV_STEREO_RL  1
#define IMODV_STEREO_TB  2
#define IMODV_STEREO_HW  3

/* Stereo Control functions. */
void imodvStereoEditDialog(ImodvApp *a, int state);
void imodvStereoUpdate(void);
void imodvStereoToggle(void);
void stereoHWOff(void);
void stereoClear(GLbitfield mask);
void stereoDrawBuffer(GLenum mode);

#include "dialog_frame.h"
class MultiSlider;
class QComboBox;

class ImodvStereo : public DialogFrame
{
  Q_OBJECT

 public:
  ImodvStereo(QWidget *parent, const char *name = NULL);
  ~ImodvStereo() {};

  void update();
  QComboBox *mComboBox;
  MultiSlider *mSlider;

  public slots:
    void newOption(int item);
  void sliderMoved(int which, int value, bool dragging);
  void buttonPressed(int which);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );

 private:
  bool mCtrlPressed;
};

#endif
