/*   imodv_stereo.h  -  declarations for imodv_stereo.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
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
  void fontChange( const QFont & oldFont );

 private:
  bool mCtrlPressed;
};

#endif
/*

$Log$
Revision 4.5  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.4  2004/06/08 15:41:41  mast
Restore clear function

Revision 4.3  2004/06/06 21:28:15  mast
Eliminate unneeded clear function

Revision 4.2  2003/02/27 17:32:28  mast
Had to include qgl.h instead of GL/gl.h under windows

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.3  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.2  2003/01/01 05:45:50  mast
Qt version

Revision 1.1.2.1  2002/12/18 04:10:30  mast
initial creation

*/
