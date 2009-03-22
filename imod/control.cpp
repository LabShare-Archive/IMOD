/* 
 *  control.c -- Document callback control for drawing, passing keys to, and
 *                  closing image windows
 *               Also implements the DialogManager class for hiding, showing
 *                  and closing dialog  or image windows together
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 * Log at end
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include <qwidget.h>
#include <qtimer.h>
#include <qobject.h>
#include <qapplication.h>
#include <QDesktopWidget>
//Added by qt3to4:
#include <QKeyEvent>
#include "imod.h"
#include "imod_info.h"
#include "imod_info_cb.h"
#include "imodv.h"
#include "control.h"
#include "imod_workprocs.h"
#include "preferences.h"
#include "dia_qtutils.h"

/* Structure used by dialog manager */
typedef struct imodv_dialog
{
  QWidget *widget;
  int iconified;
  int dlgClass;
  int dlgType;
  QPoint position;
} ImodvDialog;

/* Global resident instances of dialog managers for imod and imodv */

DialogManager imodvDialogManager;
DialogManager imodDialogManager;


static int removeControl(ImodView *iv, int inCtrlId, int callClose);

/****************************************************************************/

/* Add a control to the imodDraw command, and to the quit command.
 * the return value is used as the inCtrlId field in other control
 * funtions.
 */
int ivwNewControl(ImodView *iv,
                  ImodControlProc draw_cb,
                  ImodControlProc quit_cb,
                  ImodControlKey key_cb,
                  void *data)
{
  ImodControl ctrl;
  static int ctrlId  = 0;
  ctrl.userData = data;
  ctrl.draw_cb  = draw_cb;
  ctrl.close_cb = quit_cb;
  ctrl.key_cb   = key_cb;
  ctrl.id       = 0;
  ctrl.status   = 0;
     
  if (!iv->ctrlist){
    iv->ctrlist = (ImodControlList *)malloc
      (sizeof(ImodControlList));
    if (!iv->ctrlist){
      return(0);
    }
    iv->ctrlist->list = ilistNew(sizeof(ImodControl), 4);
    if (!iv->ctrlist->list){
      free(iv->ctrlist);
      return(0);
    }
    iv->ctrlist->active = 0;
    iv->ctrlist->top    = 0;
  }
  ctrlId++;
  ctrl.id = ctrlId;
  iv->ctrlist->top    = ctrlId;
  iv->ctrlist->active = ctrlId;
  ilistPush(iv->ctrlist->list, &ctrl);
  if (imodDebug('c'))
    imodPrintStderr("Control id %d\n", ctrlId);
  return(ctrlId);
}

/* remove the control associated with the inCtrlId value.
 * do not call the close method of the control
 * This avoids convoluted close logic in the controls!
 */
int ivwRemoveControl(ImodView *iv, int inCtrlId)
{
  return removeControl(iv, inCtrlId, 0);
}

/* delete the control associated with the inCtrlId value.
 * this will also call the close or quit method of the contorl.
 */
int ivwDeleteControl(ImodView *iv, int inCtrlId)
{
  return removeControl(iv, inCtrlId, 1);
}

static int removeControl(ImodView *iv, int inCtrlId, int callClose)
{
  ImodControl *ctrlPtr;
  int element = 0;

  if (!iv->ctrlist) return(0);
  if (iv->ctrlist->list->size < 1) return(0);
  ivwControlListDrawCancel(iv);
  iv->ctrlist->active = 0;
  ctrlPtr = (ImodControl *)ilistFirst(iv->ctrlist->list);
  while(ctrlPtr){
    if (ctrlPtr->id == inCtrlId){
      if (callClose)
        (*ctrlPtr->close_cb)(iv, ctrlPtr->userData, 0);
      ilistRemove(iv->ctrlist->list, element);
      ctrlPtr = (ImodControl *)ilistFirst(iv->ctrlist->list);
      if (ctrlPtr)
	iv->ctrlist->top = ctrlPtr->id;
      return(0);
    }
    element++;
    ctrlPtr = (ImodControl *)ilistNext(iv->ctrlist->list);
  }
  return(1);
}

/* move control to top of control Q if it exists 
 * also sets the control active flag.
 */
int ivwControlPriority(ImodView *iv, int inCtrlId)
{
  ImodControl *ctrlPtr;
  int element = 0;

  if (imodDebug('c'))
    imodPrintStderr("ivwControlPriority: %d\n", inCtrlId);

  if (!iv->ctrlist) return(0);

  iv->ctrlist->active = inCtrlId;
  if (!inCtrlId) return(iv->ctrlist->top);

  if (iv->ctrlist->top == inCtrlId)
    return(inCtrlId);

  ivwControlListDrawCancel(iv);
  ctrlPtr = (ImodControl *)ilistFirst(iv->ctrlist->list);
  while(ctrlPtr){
    if (ctrlPtr->id == inCtrlId){
      ilistFloat(iv->ctrlist->list, element);
      iv->ctrlist->top = inCtrlId;
      return(0);
    }
    element++;
    ctrlPtr = (ImodControl *)ilistNext(iv->ctrlist->list);
  }
  return(iv->ctrlist->top);
}

void ivwControlActive(ImodView *iv, int inCtrlId)
{
  if (iv->ctrlist)
    iv->ctrlist->active = inCtrlId;
}

void ivwControlDraw(ImodView *iv, int reason, int inCtrlId)
{
  ivwControlListDrawCancel(iv);
  ivwControlPriority(iv, inCtrlId);
  ivwControlListDraw(iv, reason);
}

void ivwControlListDrawCancel(ImodView *iv)
{
  if (!iv->ctrlist)
    return;
  iv->timers->mControlTimer->stop();
}

void ivwWorkProc(ImodView *iv)
{

  ImodControl *ctrlPtr;

  if ((!iv->ctrlist)||(!iv->ctrlist->list->size)){
    iv->timers->mControlTimer->stop();
    return;
  }

  ctrlPtr = (ImodControl *)ilistNext(iv->ctrlist->list);
  if (!ctrlPtr){
    iv->timers->mControlTimer->stop();
    return;
  }
  if (imodDebug('c'))
    imodPrintStderr("Drawing %d\n", ctrlPtr->id); 
  (*ctrlPtr->draw_cb)(iv, ctrlPtr->userData, iv->ctrlist->reason);
}

/*
 *  Draw first top priority control and setup workproc 
 *  to update the rest of the controls on the list.
 */
void ivwControlListDraw(ImodView *iv, int reason)
{
  ImodControl *ctrlPtr;

  if (!iv->ctrlist) return;
  if (!iv->ctrlist->list->size) return;
  iv->ctrlist->reason = reason;
  ivwControlListDrawCancel(iv);
     
  ctrlPtr = (ImodControl *)ilistFirst(iv->ctrlist->list);
  if (ctrlPtr) {
    if (imodDebug('c'))
      imodPrintStderr("Drawing priority %d\n", ctrlPtr->id);
    if (ctrlPtr->id == iv->ctrlist->active){
      iv->ctrlist->active = 0;
      (*ctrlPtr->draw_cb)(iv, ctrlPtr->userData,
			  reason | IMOD_DRAW_TOP | IMOD_DRAW_ACTIVE);
    }else{
      (*ctrlPtr->draw_cb)(iv, ctrlPtr->userData, 
			  reason | IMOD_DRAW_TOP);
    }
  }

  /* draw the rest of the windows. */
  iv->timers->mControlTimer->start(1);

  /*     while(NULL != (ctrlPtr = ilistNext(iv->ctrlist->list))){
	 (*ctrlPtr->draw_cb)(iv, ctrlPtr->userData, reason);
	 }
  */
  return;
}

void ivwControlListDelete(ImodView *iv)
{
  ImodControl *ctrlPtr;
     
  if (!iv->ctrlist) return;
  ctrlPtr = (ImodControl *)ilistFirst(iv->ctrlist->list);
  while(ctrlPtr){
    (*ctrlPtr->close_cb)(iv, ctrlPtr->userData, 0);
    ctrlPtr = (ImodControl *)ilistNext(iv->ctrlist->list);
  }
  ilistDelete(iv->ctrlist->list);
  free(iv->ctrlist);
  iv->ctrlist = NULL;
}

// Pass a key event on to the first control on the list that has a callback
// Leave out the iv parameter so form classes can call directly
void ivwControlKey(/*ImodView *iv, */int released, QKeyEvent *e)
{
  ImodView *iv = App->cvi;
  ImodControl *ctrlPtr;
  
  if (!iv->ctrlist) return;
  if (!iv->ctrlist->list->size) return;
     
  ctrlPtr = (ImodControl *)ilistFirst(iv->ctrlist->list);
  while (ctrlPtr) {
    if (imodDebug('c'))
      imodPrintStderr("checking %d\n", ctrlPtr->id);
    if (ctrlPtr->key_cb) {
      if (imodDebug('c'))
	imodPrintStderr("sending to %d\n", ctrlPtr->id);
      (*ctrlPtr->key_cb)(iv, ctrlPtr->userData, released, e);
      return;
    }
    ctrlPtr = (ImodControl *)ilistNext(iv->ctrlist->list);
  }
}

/***************************************************************************
 * DIALOG MANAGER CLASS - FOR HIDING, SHOWING, AND CLOSING DIALOGS
 *      This may include image windows
 */

DialogManager::DialogManager()
{
  mDialogList = NULL;
  mLastZapGeom.setRect(0, 0, 0, 0);
}

// Add a dialog to the list
void DialogManager::add(QWidget *widget, int dlgClass, int dlgType)
{
  ImodvDialog dia;

  if (!mDialogList) {
    mDialogList = ilistNew(sizeof(ImodvDialog), 4);
    if (!mDialogList) {
      imodError(NULL, "3DMOD WARNING: Failure to get memory for dialog list\n"
	      );
      return;
    }
  }

  dia.widget = widget;
  dia.iconified = 0;
  dia.dlgClass = dlgClass;
  dia.dlgType = dlgType;
  ilistAppend(mDialogList, &dia);

  // Set the icon from the appropriate place
  switch (dlgClass) {
  case IMOD_DIALOG:
  case IMOD_IMAGE:
    widget->setWindowIcon(*(App->iconPixmap));
    break;
  case IMODV_DIALOG:
    widget->setWindowIcon(*(Imodv->iconPixmap));
    break;
  }
}

// Remove a dialog from the list
void DialogManager::remove(QWidget * widget)
{
  int index = 0;
  ImodvDialog *dia;
  if (!mDialogList)
    return;

  dia = (ImodvDialog *)ilistFirst(mDialogList);
  while (dia){
    if (dia->widget == widget) {
      if (dia->dlgType == ZAP_WINDOW_TYPE)
        mLastZapGeom = ivwRestorableGeometry(widget);
      ilistRemove(mDialogList, index);
      return;
    }
    dia = (ImodvDialog *)ilistNext(mDialogList);
    index++;
  }
  imodError(NULL, "3DMOD WARNING: Failed to find closing dialog on list\n");
}

// Close all dialogs and delete the list
void DialogManager::close()
{
  ImodvDialog *dia;
  Ilist *tempList = mDialogList;

  // Put list in a local variable and null out the static one so that removals
  // are ignored
  if (!mDialogList)
    return;
  mDialogList = NULL;

  dia = (ImodvDialog *)ilistFirst(tempList);
  while (dia){
    dia->widget->close();
    dia = (ImodvDialog *)ilistNext(tempList);
  }
  ilistDelete(tempList);
}

// Iconify any dialogs that are shown now; keep track of them
void DialogManager::hide()
{
  int hideTogether[6];
  ImodvDialog *dia;

  // Get flags for whether dialog classes should be hidden together
  hideTogether[IMODV_DIALOG] = ImodPrefs->iconifyImodvDlg();
  hideTogether[IMOD_DIALOG] = ImodPrefs->iconifyImodDlg();
  hideTogether[IMOD_IMAGE] = ImodPrefs->iconifyImageWin();
  dia = (ImodvDialog *)ilistFirst(mDialogList);
  while (dia){
    // Do not iconify if it is already minimized or if this class should
    // not be hidden together
    if (dia->widget->isHidden() || !hideTogether[dia->dlgClass])
      dia->iconified = 0;
    else {
      dia->iconified = 1;
      dia->position = (ivwRestorableGeometry(dia->widget)).topLeft();

      // This was showMinimized for everyone originally; worked fine on
      // Windows and Linux under Qt 3.0.5, didn't work on SGI because the boxes
      // didn't come back up.  Then the boxes didn't come up under RH 9 
      // (Qt 3.1), so switched to hide, which works on Linux, SGI, and Windows
      // and has the advantage of producing a single icon.
      // But on the Mac it made the main imodv window bounce back out!
      // showNormal and show are interchangeable for getting back up
#ifdef Q_OS_MACX
      dia->widget->showMinimized();
#else
      dia->widget->hide();
#endif
    }
    dia = (ImodvDialog *)ilistNext(mDialogList);
  }
}

// Show dialogs that were brought down by the hide operation
void DialogManager::show()
{
  ImodvDialog *dia;
  dia = (ImodvDialog *)ilistFirst(mDialogList);
  while (dia){
    if (dia->iconified) {
      dia->widget->move(dia->position);
      dia->widget->showNormal();
    }
    dia = (ImodvDialog *)ilistNext(mDialogList);
  }
}

// Return the appropriate parent based on type of winodw and OS
QWidget *DialogManager::parent(int dlgClass)
{
#ifdef Q_OS_MACX
  if (dlgClass == IMODV_DIALOG)
    return (QWidget *)Imodv->mainWin;
  else
    return (QWidget *)ImodInfoWin;
#else
  return NULL;
#endif
}

// Raise all windows of the given class
void DialogManager::raise(int dlgClass)
{
  ImodvDialog *dia;
  dia = (ImodvDialog *)ilistFirst(mDialogList);
  while (dia){
    if (dia->dlgClass == dlgClass && dia->widget->isVisible())
      dia->widget->raise();
    dia = (ImodvDialog *)ilistNext(mDialogList);
  }
}

// Return the number of windows of a given type
int DialogManager::windowCount(int dlgType)
{
  int num = 0;
  ImodvDialog *dia;
  dia = (ImodvDialog *)ilistFirst(mDialogList);
  while (dia){
    if (dia->dlgType == dlgType) 
      num++;
    dia = (ImodvDialog *)ilistNext(mDialogList);
  }
  return num;
}

// Return the size of the biggest Zap window still open, or of the last
// Zap window closed if none are open
QRect DialogManager::biggestGeometry(int dlgType)
{
  QRect biggest(0, 0, 0, 0);
  
  ImodvDialog *dia;
  dia = (ImodvDialog *)ilistFirst(mDialogList);
  while (dia){
    if (dia->dlgType == dlgType) {
      QRect geom = ivwRestorableGeometry(dia->widget);
      if (geom.width() * geom.height() > biggest.width() * biggest.height())
        biggest = geom;
    }
    dia = (ImodvDialog *)ilistNext(mDialogList);
  }
  if (!biggest.width())
    return mLastZapGeom;
  return biggest;
}

// Construct a list of widgets matching the given class and type; pass -1 to
// match any class or type
void DialogManager::windowList(QObjectList *objList, int dlgClass, int dlgType)
{
  ImodvDialog *dia;
  dia = (ImodvDialog *)ilistFirst(mDialogList);
  while (dia){
    if ((dlgType < 0 || dia->dlgType == dlgType) && 
        (dlgClass < 0 || dia->dlgClass == dlgClass))
      objList->append((QObject *)dia->widget);
    dia = (ImodvDialog *)ilistNext(mDialogList);
  }
}

// Adjust size 
void adjustGeometryAndShow(QWidget *widget, int dlgClass, bool doSize)
{
  QWidget *parwidg = NULL;
  int deskWidth, deskHeight, xleft, ytop;
  QRect pos, parpos, newpos;

  // Adjust the size and get it on the screen, then show it
  imod_info_input();
  if (doSize)
    widget->adjustSize();
  pos = widget->frameGeometry();
  xleft = pos.x();
  ytop = pos.y();
  //imodPrintStderr("%d %d %d %d\n", xleft, ytop, pos.width(), pos.height());
  diaLimitWindowPos(pos.width(), pos.height(), xleft, ytop);
  if (xleft != pos.x() || ytop != pos.y())
    widget->move(xleft, ytop);
  newpos = pos;
  newpos.moveTo(xleft, ytop);
  //imodPrintStderr("%d %d %d %d\n", xleft, ytop, pos.width(), pos.height());
  widget->show();

  // Get parent if appropriate (Mac OSX) and make sure it doesn't intersect
  // it, but at least restore window manager's position
  if (dlgClass >= 0)
    parwidg = imodDialogManager.parent(dlgClass);
  if (parwidg) {

    // Get the geometry again since Y size was not right for tallest widgets
    // in cocoa Qt 4.5.0
    pos = widget->frameGeometry();
    //imodPrintStderr("%d %d %d %d\n",pos.x(),pos.y(),pos.width(),pos.height());
    parpos = parwidg->frameGeometry();
    /*imodPrintStderr("parent %d %d %d %d\n", parpos.x(), parpos.y(),
      parpos.width(), parpos.height());*/
    // If parent intersects, check if can be moved to below, above, right, left
    if (newpos.intersects(parpos)) {
      deskWidth = QApplication::desktop()->width();
      deskHeight = QApplication::desktop()->height();
      if (pos.height() <= deskHeight - (parpos.y() + parpos.height()))
        ytop = parpos.y() + parpos.height();
      else if (pos.height() < parpos.y() - 32)
        ytop = parpos.y() - pos.height();
      else if (pos.width() <= deskWidth - (parpos.x() + parpos.width()))
        xleft = parpos.x() + parpos.width();
      else if (pos.width() < parpos.x())
        xleft = parpos.x() - pos.width();
    }
    diaLimitWindowPos(pos.width(), pos.height(), xleft, ytop);
    widget->move(xleft, ytop);
    //imodPrintStderr("%d %d\n", xleft, ytop);
  }
}

// Return system-dependent rectangle that can be used to restore window size
// and position
// This is supposed to be the frame position and the client size, but on
// the SGI we need the client size since move() sets the client position
QRect ivwRestorableGeometry(QWidget *widget)
{
#ifdef SGI_GEOMETRY_HACK
  return widget->geometry();
#else
  return QRect(widget->pos(), widget->size());
#endif
}


/*
$Log$
Revision 4.19  2009/03/22 19:44:05  mast
New routine for adjusting size generally, and position on make, when showing

Revision 4.18  2009/01/15 16:33:17  mast
Qt 4 port

Revision 4.17  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 4.16  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.15  2004/05/05 17:33:17  mast
Added function to get list of windows of one type

Revision 4.14  2004/04/28 23:52:26  mast
Added method to get count of a window type

Revision 4.13  2003/11/01 18:12:16  mast
changed to put out virtually all error messages to a window

Revision 4.12  2003/09/24 17:32:44  mast
Add and use restorable geometry routine

Revision 4.11  2003/09/24 00:48:48  mast
Switched from keeping track of geometry to keeping track of pos() and
size() when saving and restoring positions and sizes

Revision 4.10  2003/09/17 05:54:19  mast
Make it keep track of geometry of last zap window closed

Revision 4.9  2003/09/17 04:46:21  mast
Added function to return size of biggest zap window

Revision 4.8  2003/08/26 02:04:49  mast
Save geometry when hiding and set it when restoring to keep windows from
moving around

Revision 4.7  2003/08/01 01:01:29  mast
Switch from showMinimized to hide (except on Mac) because it didn't work
under RedHat 9.0

Revision 4.6  2003/05/23 02:45:46  mast
Added a function to raise managed windows

Revision 4.5  2003/05/18 22:08:48  mast
Changes to add an application icon

Revision 4.4  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.3  2003/04/17 19:00:59  mast
new function to provide machine-dependent parent widget

Revision 4.2  2003/03/24 17:58:09  mast
Changes for new preferences capability

Revision 4.1  2003/02/10 20:28:59  mast
autox.cpp

Revision 1.1.2.6  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.5  2003/01/14 21:46:32  mast
renamed dialog manager for imod

Revision 1.1.2.4  2003/01/13 07:20:04  mast
Adapted dialog manager calss from imodv_input

Revision 1.1.2.3  2003/01/10 23:48:47  mast
Made diagnostic output conditional on a flag

Revision 1.1.2.2  2003/01/04 03:45:12  mast
Add a function to remove a control without calling its close function
to deconvolute the closing logic

Revision 1.1.2.1  2003/01/02 15:37:07  mast
Added key callback so that keys can be passed from dialog boxes to active
window

Revision 3.2  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.1  2002/11/25 19:18:37  mast
Eliminated conditional on USE_IMOD_CONTROL

*/
