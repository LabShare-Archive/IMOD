/*   model_edit.h  -  declarations for model_edit.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
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
  void setPixelClicked();

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void changeEvent(QEvent *e);

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
  void changeEvent(QEvent *e);

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
