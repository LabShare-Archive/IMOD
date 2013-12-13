/*   rescale.h  -  declarations for rescale.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMOD_ISCALE_H
#define IMOD_ISCALE_H

#include "dialog_frame.h"
//Added by qt3to4:
#include <QTimerEvent>
#include <QKeyEvent>
#include <QCloseEvent>
#include <QLabel>
class QLabel;
class QLineEdit;

typedef struct ViewInfo ImodView;

class ImageScaleWindow : public DialogFrame
{
  Q_OBJECT

 public:
  ImageScaleWindow(QWidget *parent, const char *name = NULL);
  ~ImageScaleWindow() {};
  void showFileAndMMM();
  void updateLimits();

  public slots:
  void buttonPressed(int which);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void timerEvent(QTimerEvent *e);
  void changeEvent(QEvent *e);

 private:
  QLineEdit *mEditBox[2];
  QLabel *mFileLabel;
  QLabel *mMMMLabel;
  void applyLimits();
  void computeScale();
  int mTimerID;
};

void imodImageScaleDialog(ImodView *iv);
void imodImageScaleUpdate(ImodView *iv);

#endif
