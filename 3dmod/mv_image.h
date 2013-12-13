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
#include <QKeyEvent>
#include <QCloseEvent>
#include "mv_movie.h"

typedef struct __imodv_struct ImodvApp;

#define IMODV_DRAW_CZ 1
#define IMODV_DRAW_CY (1 << 1)
#define IMODV_DRAW_CX (1 << 2)

/* Image Control functions. */
void imodvDrawImage(ImodvApp *a, int drawTrans);
void mvImageEditDialog(ImodvApp *a, int state);
void mvImageUpdate(ImodvApp *a);
void mvImageSetThickTrans(int slices, int trans);
int mvImageGetThickness(void);
int mvImageGetTransparency(void);
int mvImageGetFlags(void);
bool mvImageDrawingZplanes(void);
void mvImageCleanup();
void mvImageGetMovieState(MovieSegment &segment);
void mvImageSetMovieEndState(int startEnd, MovieSegment &segment);
int mvImageSetMovieDrawState(MovieSegment &segment);
bool mvImageSubsetLimits(double &zoom, float &zoomUpLimit, float &zoomDownLimit, 
                         int &ixStart, int &iyStart, int &nxUse, int &nyUse);

#include "dialog_frame.h"
class MultiSlider;
class QCheckBox;

class ImodvImage : public DialogFrame
{
  Q_OBJECT

 public:
  ImodvImage(QWidget *parent, bool fillBut, const char *name = NULL) ;
  ~ImodvImage() {};
  void viewToggled(bool state, int flag);
  void updateCoords();

  QCheckBox *mViewXBox, *mViewYBox, *mViewZBox;
  QCheckBox *mFalseBox;
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
  void changeEvent(QEvent *e);

 private:
  bool mCtrlPressed;
};

#endif
