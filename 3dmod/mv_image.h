//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>
/*   mv_image.h  -  declarations for mv_image.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
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
int imodvImageGetFlags(void);
void imodvImageCleanup();

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
    void copyBWclicked();
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
