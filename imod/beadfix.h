/*   beadfix.h  -  header for beadfixer plugin/module
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.4  2004/05/07 22:14:26  mast
Defined a variable instead of QT_THREAD_SUPPORT for the the Run Align button

Revision 1.3  2004/05/03 19:17:20  mast
Added thread class to run tiltalign

Revision 1.2  2004/04/29 00:28:51  mast
Added button to keep window on top

Revision 1.1  2003/10/01 05:10:58  mast
Incorporation as internal module in 3dmod

*/
#ifndef BEADFIX_H
#define BEADFIX_H

#include "dialog_frame.h"

// Changes to make internal module: include and declare class
#include "special_module.h"

class BeadFixerModule : public SpecialModule
{
 public:
  BeadFixerModule();
};

#ifdef QT_THREAD_SUPPORT
#define FIXER_CAN_RUN_ALIGN
#endif

#ifdef FIXER_CAN_RUN_ALIGN
#include <qthread.h>

class AlignThread : public QThread
{
 public:
  AlignThread() {};
  ~AlignThread() {};

 protected:
  void run();
};
#endif


class QPushButton;
class QCheckBox;
class BeadFixer : public DialogFrame
{
  Q_OBJECT

 public:
  BeadFixer(QWidget *parent, const char *name = NULL);
  ~BeadFixer() {};

  public slots:
  void buttonPressed(int which);
  void nextGap();
  void openFile();
  void rereadFile() {reread(0);};
  void nextLocal() {reread(1);};
  void nextRes();
  void backUp();
  void movePoint();
  void undoMove();
  void clearList();
  void onceToggled(bool state);
  void keepOnTop(bool state);
  void runAlign();

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void timerEvent(QTimerEvent *e);

 private:
  void reread(int which);
  int foundgap(int obj, int cont, int ipt, int before);
  void clearExtraObj();
  QPushButton *rereadBut;
  QPushButton *nextLocalBut;
  QPushButton *nextResBut;
  QPushButton *backUpBut;
  QPushButton *movePointBut;
  QPushButton *undoMoveBut;
  QPushButton *clearListBut;
  QPushButton *runAlignBut;
  QPushButton *openFileBut;
  bool mStayOnTop;
  bool mRunningAlign;
  int mTopTimerID;
#ifdef FIXER_CAN_RUN_ALIGN
  AlignThread *mTaThread;
#endif
};

#endif
