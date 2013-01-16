//Added by qt3to4:
#include <QKeyEvent>
#include <QEvent>
#include <QMouseEvent>
/*   imodplugP.h  -  private function declarations for imodplug.cpp
 *
 *  $Id$
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
void imodPlugOpenByName(const char *name);
int imodPlugMessage(ImodView *vw, QStringList *strings, int *arg);
void imodPlugOpenAllExternal(void);
QString imodPlugGetOpenName(QWidget *parent, const QString &caption,
                            const QString &dir, const QString &filter);
QStringList imodPlugGetOpenNames(QWidget *parent, const QString &caption,
                                 const QString &dir, const QString &filter);

#endif
