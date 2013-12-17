//Added by qt3to4:
#include <QLabel>
#include <QKeyEvent>
#include <QCloseEvent>
/*   cont_edit.h  -  declarations for cont_edit.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMOD_CONT_EDIT_H
#define IMOD_CONT_EDIT_H

#define MAX_POINT_SIZE 999

typedef struct ViewInfo ImodView;

#include "dialog_frame.h"
class QLabel;
class QSpinBox;
class QCheckBox;
class QButtonGroup;
class QRadioButton;

class ContourFrame : public DialogFrame
{
  Q_OBJECT

 public:
  ContourFrame(QWidget *parent, int numButtons, const char *labels[], const char *tips[],
               const char *name = NULL);
  ~ContourFrame() {};
 
 protected:
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
};  
 
class ContourMove : public ContourFrame
{
  Q_OBJECT

 public:
  ContourMove(QWidget *parent, const char *name = NULL);
  ~ContourMove() {};
  void manageCheckBoxes();
  QSpinBox *mObjSpinBox;

  public slots:
  void buttonPressed(int which);
  void objSelected(int which);
  void surfToggled(bool state);
  void toSurfToggled(bool state);
  void replaceToggled(bool state);
  void expandToggled(bool state);
  void keepSizeToggled(bool state);
  void shiftContClicked();
  void moveUpDownToggled(bool state);
  void upDownSelected(int which);

 protected:
  void closeEvent ( QCloseEvent * e );
  void changeEvent(QEvent *e);

 private:
  QLabel *mObjSurfLabel;
  QCheckBox *mMoveAllBox;
  QCheckBox *mToSurfBox;
  QCheckBox *mReplaceBox;
  QCheckBox *mExpandBox;
  QCheckBox *mKeepSizeBox;
  QCheckBox *mMoveUpDownBox;
  QButtonGroup *mUpDownGroup;
  QRadioButton *mUpButton;
  QRadioButton *mDownButton;

};

class ContourJoin : public ContourFrame
{
  Q_OBJECT

 public:
  ContourJoin(QWidget *parent, const char *name = NULL);
  ~ContourJoin() {};

  public slots:
  void buttonPressed(int which);
  void set1Pressed();
  void set2Pressed();
  void openTypeSelected(int which);
  void closedTypeSelected(int which);

 public:
  QLabel *mSet1Label;
  QLabel *mSet2Label;

 protected:
  void closeEvent ( QCloseEvent * e );
  void changeEvent(QEvent *e);

 private:
  void setFontDependentWidths();
  QPushButton *mButton1;
  QPushButton *mButton2;
  QButtonGroup *mOpenGroup;
  QButtonGroup *mClosedGroup;
};

class ContourBreak : public ContourFrame
{
  Q_OBJECT

 public:
  ContourBreak(QWidget *parent, const char *name = NULL);
  ~ContourBreak() {};
  void breakCont();

  public slots:
  void buttonPressed(int which);
  void set1Pressed();
  void set2Pressed();
  void unsetPressed();
  void currentToggled(bool state);

 protected:
  void closeEvent ( QCloseEvent * e );
  void changeEvent(QEvent *e);

 private:
  QLabel *mObjContLabel;
  QLabel *mSet1Label;
  QLabel *mSet2Label;
  QPushButton *mButton1;
  QPushButton *mButton2;
  QPushButton *mUnsetBut;
  QCheckBox *mCurrentBox;
  void setLabels();
  void setFontDependentWidths();
};

/* Entries from the rest of imod */
void imodContEditBreakOpen(ImodView *vw);
int imodContEditBreak();
void imodContEditJoinOpen(ImodView *vw);
void imodContEditJoin(ImodView *vw);
void imodContEditSurf(ImodView *vw);
void imodContEditSurfShow(void);
void imodContEditMove(void);
void imodContEditMoveDialog(ImodView *vw, int moveSurf);
void imodContEditMoveDialogUpdate(void);

/* Entries from the form class for surface etc */
void iceTimeChanged(int value);
void iceLabelChanged(const char *st, int contPoint);
void iceSurfNew();
void iceSurfGoto(int target);
void iceGhostInterval(int value);
void iceGhostToggled(int state, int flag);
void iceClosedOpen(int state);
void icePointSize(float size);;
void iceContInSurf(int direction);
void iceShowHelp();
void iceClosing();
void iceSetWheelForSize(int state);
int iceGetWheelForSize();


#endif
