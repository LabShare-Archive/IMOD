/*   imodv_image.h  -  declarations for imodv_image.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1.2.4  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.3  2003/01/01 05:43:44  mast
rationalizing toplevel versus dialog style

Revision 1.1.2.2  2002/12/30 06:41:06  mast
qt version

Revision 1.1.2.1  2002/12/18 04:10:30  mast
initial creation

*/

#ifndef IMODV_IMAGE_H
#define IMODV_IMAGE_H

typedef struct __imodv_struct ImodvApp;

/* Image Control functions. */
void imodvDrawImage(ImodvApp *a);
void imodvImageEditDialog(ImodvApp *a, int state);
void imodvImageUpdate(ImodvApp *a);

#include "dialog_frame.h"
class MultiSlider;
class QCheckBox;

class ImodvImage : public DialogFrame
{
  Q_OBJECT

 public:
  ImodvImage(QWidget *parent, const char *name = NULL) ;
  ~ImodvImage() {};

  QCheckBox *mViewBox;

  public slots:
    void viewToggled(bool state);
  void falseToggled(bool state);
  void sliderMoved(int which, int value, bool dragging);
  void buttonPressed(int which);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );

 private:
  bool mCtrlPressed;
  MultiSlider *mSliders;
  QCheckBox *mFalseBox;
};

#endif
