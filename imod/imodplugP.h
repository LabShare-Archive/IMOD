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
*/
#ifndef IMODPLUGP_H
#define IMODPLUGP_H
class QPopupMenu;

int imodPlugInit(void);
int imodPlugLoaded(int type);
int imodPlugCall(ImodView *vw, int type, int reason);
void imodPlugMenu(QPopupMenu *parent); /* build plugin menu. */
int imodPlugHandleKey(ImodView *vw, QKeyEvent *event);
void imodPlugOpen(int item);

#endif
