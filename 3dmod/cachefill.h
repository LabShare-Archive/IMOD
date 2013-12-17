/*   cachefill.h  -  declarations for cachefill.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
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
int imodCacheFill(ImodView *vw, int source = 0);

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
  void changeEvent(QEvent *e);

 private:
  QButtonGroup *mFillGroup;
  QButtonGroup *mBalanceGroup;
  QButtonGroup *mOverlapGroup;
  QRadioButton *mOverlapRadio[3];
  QCheckBox *mAutoCheck;
  QGroupBox *mOverlapBox;

};

#endif
