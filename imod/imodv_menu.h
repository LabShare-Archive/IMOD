/*   imodv_menu.h  -  declarations for imodv_menu.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1.2.2  2002/12/27 01:24:13  mast
new background color selector

Revision 1.1.2.1  2002/12/17 17:41:09  mast
initial creation

*/

#ifndef IMODV_MENU_H
#define IMODV_MENU_H
#include <qobject.h>

class ColorSelector;

class ImodvBkgColor : public QObject
{
  Q_OBJECT

 public:
  ImodvBkgColor();
  ~ImodvBkgColor() {};
  void openDialog();

  ColorSelector *mSelector;

  public slots:
   void newColorSlot(int red, int green, int blue);
  void doneSlot();
  void closingSlot();

  void keyPressSlot ( QKeyEvent * e );
  void keyReleaseSlot ( QKeyEvent * e );

};

  /* menu.c functions */
void imodvMenuLight(int value);
void imodvMenuWireframe(int value);
void imodvMenuLowres(int value);
void imodvFileSave(void);
void imodvEditMenu(int item);
void imodvHelpMenu(int item);
void imodvFileMenu(int item);
void imodvViewMenu(int item);
void imodvMenuBgcolor(int state);
int imodvLoadModel();
void imodvSaveModelAs();

#endif

