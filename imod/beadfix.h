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

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );

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
};

#endif
