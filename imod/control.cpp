/*  IMOD VERSION 2.41
 *
 *  control.c -- Document callback control for drawing, passing keys to, and
 *                  closing image windows
 *               Also implements the DialogManager class for hiding, showing
 *                  and closing dialog  or image windows together
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/

/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include <qwidget.h>
#include <qtimer.h>
#include "imod.h"
#include "imod_info.h"
#include "imodv.h"
#include "control.h"
#include "imod_workprocs.h"
#include "preferences.h"

/* Structure used by dialog manager */
typedef struct imodv_dialog
{
  QWidget *widget;
  int iconified;
  int dlgClass;
} ImodvDialog;

/* Global resident instances of dialog managers for imod and imodv */

DialogManager imodvDialogManager;
DialogManager imodDialogManager;


static int removeControl(ImodView *iv, int inCtrlId, int callClose);

static int controlDebug = 0;

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
  if (controlDebug)
    fprintf(stderr, "Control id %d\n", ctrlId);
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

  if (controlDebug)
    fprintf(stderr, "ivwControlPriority: %d\n", inCtrlId);

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
  if (controlDebug)
    fprintf(stderr, "Drawing %d\n", ctrlPtr->id); 
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
    if (controlDebug)
      fprintf(stderr, "Drawing priority %d\n", ctrlPtr->id);
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
    if (controlDebug)
      fprintf(stderr, "checking %d\n", ctrlPtr->id);
    if (ctrlPtr->key_cb) {
      if (controlDebug)
	fprintf(stderr, "sending to %d\n", ctrlPtr->id);
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
}

// Add a dialog to the list
void DialogManager::add(QWidget *widget, int dlgClass)
{
  ImodvDialog dia;

  if (!mDialogList) {
    mDialogList = ilistNew(sizeof(ImodvDialog), 4);
    if (!mDialogList) {
      fprintf(stderr, "3DMOD WARNING: Failure to get memory for dialog list\n"
	      );
      return;
    }
  }

  dia.widget = widget;
  dia.iconified = 0;
  dia.dlgClass = dlgClass;
  ilistAppend(mDialogList, &dia);
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
      ilistRemove(mDialogList, index);
      return;
    }
    dia = (ImodvDialog *)ilistNext(mDialogList);
    index++;
  }
  fprintf(stderr, "3DMOD WARNING: Failed to find closing dialog on list\n");
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
    if (dia->widget->isMinimized() || !hideTogether[dia->dlgClass])
      dia->iconified = 0;
    else {
      dia->iconified = 1;
      dia->widget->showMinimized();
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
    if (dia->iconified)
      dia->widget->showNormal();
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


/*
$Log$
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
