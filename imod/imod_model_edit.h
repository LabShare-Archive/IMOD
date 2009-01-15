/*   imod_model_edit.h  -  declarations for imod_model_edit.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.5  2006/01/14 18:15:39  mast
Added incremental Z scale

Revision 4.4  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 4.3  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.2  2004/11/01 23:28:42  mast
Added function to scale resolution

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.3  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.2  2003/01/23 20:01:14  mast
Full Qt version

Revision 1.1.2.1  2003/01/18 01:17:33  mast
Initial creation

*/
#ifndef MODELOFFSETWINDOW_H
#define MODELOFFSETWINDOW_H

#include "dialog_frame.h"
#include "imodel.h"
//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>
#include <QLabel>
class QLabel;
class QLineEdit;
class ToolEdit;
class QCheckBox;

typedef struct ViewInfo ImodView;
typedef struct Mod_Model Imod;

class ModelHeaderWindow : public DialogFrame
{
  Q_OBJECT

 public:
  ModelHeaderWindow(QWidget *parent, const char *name = NULL);
  ~ModelHeaderWindow() {};
  void update();

  public slots:
  void buttonPressed(int which);
  void valueEntered();
  void drawToggled(bool state);
  void setIncToggled(bool state);

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void fontChange( const QFont & oldFont );

 private:
  QCheckBox *mDrawBox;
  QCheckBox *mSetIncBox;
  ToolEdit *mEditBox[4];
  float mPixRatio;
  bool mSettingInc;
};

class ModelOffsetWindow : public DialogFrame
{
  Q_OBJECT

 public:
  ModelOffsetWindow(QWidget *parent, const char *name = NULL);
  ~ModelOffsetWindow() {};
  void updateLabels();

  public slots:
  void buttonPressed(int which);
  void valueEntered();

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void fontChange( const QFont & oldFont );

 private:
  QLabel *mBaseLabel[3];
  QLineEdit *mEditBox[3];
  QLabel *mAppliedLabel;
};

// Global functions
int openModelOffset(ImodView *vw);
void imodModelEditUpdate();
int openModelEdit(ImodView *vw);
void setPixsizeAndUnits(Imod *imod, const char *string);
float scaleModelRes(int res, float zoom);
void imodTransXYZ(Imod *imod, Ipoint trans);

#endif
