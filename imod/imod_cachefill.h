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
Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

*/

#ifndef IMOD_CACHEFILL_H
#define IMOD_CACHEFILL_H

#include "dialog_frame.h"

typedef struct ViewInfo ImodView;

class QHButtonGroup;
class QVButtonGroup;
class QRadioButton;
class QCheckBox;

/* imod_cachefill.c */
int icfGetAutofill(void);
unsigned char *icfDoAutofill(ImodView *vw, int cz);
void imodCacheFillDialog(ImodView *vw);
void imodCacheFill(ImodView *vw);

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

 private:
  QHButtonGroup *mFillGroup;
  QVButtonGroup *mBalanceGroup;
  QHButtonGroup *mOverlapGroup;
  QRadioButton *mOverlapRadio[3];
  QCheckBox *mAutoCheck;

};

#endif
