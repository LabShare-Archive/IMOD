/*   imod_cont_edit.h  -  declarations for imod_cont_edit.cpp
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

Revision 1.1.2.1  2003/01/23 19:59:15  mast
Initial creation

*/

#ifndef IMOD_CONT_EDIT_H
#define IMOD_CONT_EDIT_H

#define MAX_POINT_SIZE 999

typedef struct ViewInfo ImodView;

#include "dialog_frame.h"
class QLabel;
class QSpinBox;
class QCheckBox;

class ContourFrame : public DialogFrame
{
  Q_OBJECT

 public:
  ContourFrame(QWidget *parent, int numButtons, char *labels[], char *tips[],
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
  void keepSizeToggled(bool state);

 protected:
  void closeEvent ( QCloseEvent * e );

 private:
  QLabel *mObjSurfLabel;
  QCheckBox *mMoveAllBox;
  QCheckBox *mToSurfBox;
  QCheckBox *mReplaceBox;
  QCheckBox *mKeepSizeBox;

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

 protected:
  void closeEvent ( QCloseEvent * e );

 private:
  QLabel *mSet1Label;
  QLabel *mSet2Label;
  void join();
};

class ContourBreak : public ContourFrame
{
  Q_OBJECT

 public:
  ContourBreak(QWidget *parent, const char *name = NULL);
  ~ContourBreak() {};

  public slots:
  void buttonPressed(int which);
  void set1Pressed();
  void set2Pressed();

 protected:
  void closeEvent ( QCloseEvent * e );

 private:
  QLabel *mObjContLabel;
  QLabel *mSet1Label;
  QLabel *mSet2Label;
  void setLabels();
  void breakCont();
};

/* Entries from the rest of imod */
void imodContEditBreak(ImodView *vw);
void imodContEditJoin(ImodView *vw, int x, int y);
void imodContEditSurf(ImodView *vw);
void imodContEditSurfShow(void);
void imodContEditMove(void);
void imodContEditMoveDialog(ImodView *vw);
void imodContEditMoveDialogUpdate(void);

/* Entries from the form class for surface etc */
void iceTimeChanged(int value);
void iceLabelChanged(char *st, int contPoint);
void iceSurfNew();
void iceSurfGoto(int target);
void iceGhostInterval(int value);
void iceGhostToggled(int state, int flag);
void iceClosedOpen(int state);
void icePointSize(float size);;
void iceContInSurf(int direction);
void iceShowHelp();
void iceClosing();


#endif
