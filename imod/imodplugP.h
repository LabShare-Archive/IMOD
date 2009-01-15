//Added by qt3to4:
#include <QKeyEvent>
#include <QEvent>
#include <QMouseEvent>
/*   imodplugP.h  -  private function declarations for imodplug.cpp
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMODPLUGP_H
#define IMODPLUGP_H
class QMenu;
class QSignalMapper;
class QStringList;

int imodPlugInit(void);
int imodPlugLoaded(int type);
int imodPlugCall(ImodView *vw, int type, int reason);
void imodPlugMenu(QMenu *parent, QSignalMapper *mapper);
int imodPlugHandleKey(ImodView *vw, QKeyEvent *event);
int imodPlugHandleMouse(ImodView *vw, QMouseEvent *event, float imx, float imy,
                        int but1, int but2, int but3);
int imodPlugHandleEvent(ImodView *vw, QEvent *event, float imx, float imy);
void imodPlugOpen(int item);
void imodPlugOpenByName(char *name);
int imodPlugMessage(ImodView *vw, QStringList *strings, int *arg);
void imodPlugOpenAllExternal(void);

#endif
/* 

$Log$
Revision 1.6  2008/01/21 05:56:47  mast
Adde function to open all plugs

Revision 1.5  2007/12/04 22:05:03  mast
Add function for handling event

Revision 1.4  2006/02/13 05:10:14  mast
Added mouse function

Revision 1.3  2004/09/24 18:09:31  mast
Added message function

Revision 1.2  2003/10/02 01:31:24  mast
Added open by name

Revision 1.1  2003/10/01 05:06:56  mast
split off from imodplug.h

*/
