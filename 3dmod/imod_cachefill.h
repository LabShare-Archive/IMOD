/*   imod_cachefill.h  -  declarations for imod_cachefill.cpp
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

Revision 4.2  2004/01/05 18:06:12  mast
Make imodCacheFill return an error

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

*/

#ifndef IMOD_CACHEFILL_H
#define IMOD_CACHEFILL_H

#include "dialog_frame.h"
//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>

typedef struct ViewInfo ImodView;

class QButtonGroup;
class QRadioButton;
class QCheckBox;
class QGroupBox;

/* imod_cachefill.c */
int icfGetAutofill(void);
unsigned char *icfDoAutofill(ImodView *vw, int cz);
void imodCacheFillDialog(ImodView *vw);
int imodCacheFill(ImodView *vw);

class ImodCacheFill : public DialogFrame
{
  Q_OBJECT

 public:
  ImodCacheFill(QWidget *parent, const char *name = NULL);
  ~ImodCacheFill() {};

  public slots:
  void buttonPressed(int which);
  void fractionSelected(int which);
  void balanceSelected(int which);
  void overlapSelected(int which);
  void autoToggled(bool state);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void fontChange( const QFont & oldFont );

 private:
  QButtonGroup *mFillGroup;
  QButtonGroup *mBalanceGroup;
  QButtonGroup *mOverlapGroup;
  QRadioButton *mOverlapRadio[3];
  QCheckBox *mAutoCheck;
  QGroupBox *mOverlapBox;

};

#endif
