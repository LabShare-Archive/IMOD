/*   mv_menu.h  -  declarations for mv_menu.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMODV_MENU_H
#define IMODV_MENU_H
#include <qobject.h>
//Added by qt3to4:
#include <QKeyEvent>

class ColorSelector;
typedef struct __imodv_struct ImodvApp;

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
void imodvMenuInvertZ(int value);
void imodvFileSave(void);
void imodvEditMenu(int item);
void imodvHelpMenu(int item);
void imodvFileMenu(int item);
void imodvViewMenu(int item);
void imodvMenuBgcolor(int state);
int imodvLoadModel();
void imodvSaveModelAs();
int imodvAddBoundingBox(ImodvApp *a);
void imodvOpenSelectedWindows(const char *keys);

#endif

