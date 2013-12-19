/* 
 *  control.cpp -- Document callback control for drawing, passing keys to, and
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
#include "info_setup.h"
#include "info_cb.h"
#include "imodv.h"
#include "control.h"
#include "xxyz.h"
#include "xzap.h"
#include "zap_classes.h"
#include "sslice.h"
#include "slicer_classes.h"
#include "workprocs.h"
#include "preferences.h"
#include "dia_qtutils.h"

/* Structure used by dialog manager */
typedef struct imodv_dialog
{
  QWidget *widget;
  int iconified;
  int dlgClass;
  int dlgType;
  int ctrlId;
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
void DialogManager::add(QWidget *widget, int dlgClass, int dlgType, int ctrlId)
{
  ImodvDialog dia;

  if (!mDialogList) {
    mDialogList = ilistNew(sizeof(ImodvDialog), 4);
    if (!mDialogList) {
      imodError(NULL, "3DMOD WARNING: Failure to get memory for dialog list\n");
      return;
    }
  }

  dia.widget = widget;
  dia.iconified = 0;
  dia.dlgClass = dlgClass;
  dia.dlgType = dlgType;
  dia.ctrlId = ctrlId;
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

// Find the window of a given type closest to top of control list, where dlgType
// can be any of the IMOD_IMAGE types that can open multiple copies
QObject *DialogManager::getTopWindow(int dlgType)
{
  int found;
  return getTopWindow(dlgType, dlgType, found);
}

// Find the first window of either of two types closest to top of control list,
// where the two types can be any of the IMOD_IMAGE types that can open multiple copies.
QObject *DialogManager::getTopWindow(int dlgType, int dlgType2, int &typeFound)
{
  ImodControl *ctrlPtr;
  int i, j, curSave;
  bool match = false;
  ImodvDialog *dia;

  if (!ilistSize(mDialogList) || !App->cvi->ctrlist)
    return NULL;

  // Look through the current control list, find first dialog with matching ID and type
  // Here we MUST save and restore current item to avoid screwing up draws
  typeFound = -1;
  curSave = App->cvi->ctrlist->list->current;
  for (j = 0; !match && j < ilistSize(App->cvi->ctrlist->list); j++) {
    ctrlPtr = (ImodControl *)ilistItem(App->cvi->ctrlist->list, j);
    for (i = 0; i < ilistSize(mDialogList); i++) {
      dia = (ImodvDialog *)ilistItem(mDialogList, i);
      if (dia->dlgType == dlgType || dia->dlgType == dlgType2) {
        match = ctrlPtr->id == dia->ctrlId;
        if (match) {
          typeFound = dia->dlgType;
          break;
        }
      }
    }
  }
  App->cvi->ctrlist->list->current = curSave;
  if (!match)
    return NULL;
  return dia->widget;
}

/*
 * Find either the first Zap window of the given type (default regular zap) or the first 
 * slicer window.  If seeking a Zap window, it looks for one with a rubberband 
 * if flag is set, or with a lasso if that flag is set (default false) or with either one
 * if both flags are set.  It tests that the band is usable for file extraction (either
 * starting with low-high set, or big enough).  If seeking a slicer, it looks for one
 * with a rubberband starting or present, with no tests for extraction validity.
 * Returns the window list index in *index if index is nonNULL.
 */
QObject *DialogManager::getTopWindow(bool withBand, bool withLasso, int type, int *index)
{
  QObjectList objList;
  ZapFuncs *zap;
  SlicerFuncs *slicer;
  ImodControl *ctrlPtr;
  int ixl, ixr, iyb, iyt;
  int i, j, curSave, topOne;

  if (index)
    *index = -1;
  imodDialogManager.windowList(&objList, -1, type);
  if (!objList.count())
    return NULL;

  // Loop through the control list and find the first window that is a zap with a viable
  // rubberband if required, which is either a drawn band or Z limits set & starting band
  // It is best to save and restore ctrlist current item
  topOne = -1;
  curSave = App->cvi->ctrlist->list->current;
  for (j = 0; topOne < 0 && j < ilistSize(App->cvi->ctrlist->list); j++) {
    ctrlPtr = (ImodControl *)ilistItem(App->cvi->ctrlist->list, j);
    for (i = 0; i < objList.count(); i++) {
      if (type == SLICER_WINDOW_TYPE) {

        // Check a slicer
        slicer = ((SlicerWindow *)objList.at(i))->mFuncs;
        if (ctrlPtr->id == slicer->mCtrl && 
            (!withBand || slicer->mRubberband || slicer->mStartingBand)) {
          if (index)
            *index = j;
          topOne = i;
          break;
        }
      } else {

        // Or check a zap
        zap = ((ZapWindow *)objList.at(i))->mZap;
        if (ctrlPtr->id == zap->mCtrl && 
            ((!withLasso || (zap->mLassoOn && !zap->mDrawingLasso)) ||
             (!withBand || zap->mRubberband || 
              (zap->mStartingBand && zap->getLowHighSection(ixl, ixr))))) {
          if (zap->mRubberband) {
            ixl = (int)floor(zap->mRbImageX0 + 0.5);
            ixr = (int)floor(zap->mRbImageX1 - 0.5);
            iyb = (int)floor(zap->mRbImageY0 + 0.5);
            iyt = (int)floor(zap->mRbImageY1 - 0.5);
            if (ixr < 1 || iyt < 1 || ixl > zap->mVi->xsize - 2 || 
                iyb > zap->mVi->ysize - 2)
              continue;
          }
          topOne = i;
          if (index)
            *index = j;
          break;
        }
      }
    }
  }
  App->cvi->ctrlist->list->current = curSave;
  if (topOne < 0 && (withBand || withLasso))
    return NULL;
  if (topOne < 0)
    topOne = 0;
  return objList.at(topOne);
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


