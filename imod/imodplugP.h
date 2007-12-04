/*   imodplugP.h  -  private function declarations for imodplug.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.4  2006/02/13 05:10:14  mast
Added mouse function

Revision 1.3  2004/09/24 18:09:31  mast
Added message function

Revision 1.2  2003/10/02 01:31:24  mast
Added open by name

Revision 1.1  2003/10/01 05:06:56  mast
split off from imodplug.h

*/
#ifndef IMODPLUGP_H
#define IMODPLUGP_H
class QPopupMenu;
class QStringList;

int imodPlugInit(void);
int imodPlugLoaded(int type);
int imodPlugCall(ImodView *vw, int type, int reason);
void imodPlugMenu(QPopupMenu *parent); /* build plugin menu. */
int imodPlugHandleKey(ImodView *vw, QKeyEvent *event);
int imodPlugHandleMouse(ImodView *vw, QMouseEvent *event, float imx, float imy,
                        int but1, int but2, int but3);
int imodPlugHandleEvent(ImodView *vw, QEvent *event, float imx, float imy);
void imodPlugOpen(int item);
void imodPlugOpenByName(char *name);
int imodPlugMessage(ImodView *vw, QStringList *strings, int *arg);

#endif
