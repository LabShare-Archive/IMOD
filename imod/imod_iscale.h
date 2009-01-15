/*   imod_iscale.h  -  declarations for imod_iscale.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.3  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.2  2003/06/04 23:31:05  mast
Add timer

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/23 19:58:52  mast
initial creation

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
  void fontChange( const QFont & oldFont );

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
