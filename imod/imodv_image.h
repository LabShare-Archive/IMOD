//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>
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
Revision 4.5  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.4  2004/06/15 01:15:51  mast
Added functions for movie control

Revision 4.3  2004/05/16 20:17:40  mast
Added method for updating sliders

Revision 4.2  2004/05/03 19:12:22  mast
Added routines for enhanced image display

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

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

#define IMODV_DRAW_CZ 1
#define IMODV_DRAW_CY (1 << 1)
#define IMODV_DRAW_CX (1 << 2)

/* Image Control functions. */
void imodvDrawImage(ImodvApp *a, int drawTrans);
void imodvImageEditDialog(ImodvApp *a, int state);
void imodvImageUpdate(ImodvApp *a);
void imodvImageSetThickTrans(int slices, int trans);
int imodvImageGetThickness(void);
int imodvImageGetTransparency(void);

#include "dialog_frame.h"
class MultiSlider;
class QCheckBox;

class ImodvImage : public DialogFrame
{
  Q_OBJECT

 public:
  ImodvImage(QWidget *parent, const char *name = NULL) ;
  ~ImodvImage() {};
  void viewToggled(bool state, int flag);
  void updateCoords();

  QCheckBox *mViewXBox, *mViewYBox, *mViewZBox;
  MultiSlider *mSliders;

  public slots:
    void viewXToggled(bool state) {viewToggled(state, IMODV_DRAW_CX);};
    void viewYToggled(bool state) {viewToggled(state, IMODV_DRAW_CY);};
    void viewZToggled(bool state) {viewToggled(state, IMODV_DRAW_CZ);};
    void falseToggled(bool state);
    void sliderMoved(int which, int value, bool dragging);
    void buttonPressed(int which);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void fontChange( const QFont & oldFont );

 private:
  bool mCtrlPressed;
  QCheckBox *mFalseBox;
};

#endif
