/*  IMOD VERSION 2.41
 *
 *  control.c -- Document callback control.
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

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include "mrcfiles.h"
#include "imod.h"

/****************************************************************************/
#ifdef USE_IMOD_CONTROL

/* Add a control to the imodDraw command, and to the quit command.
 * the return value is used as the inCtrlId field in other control
 * funtions.
 */
int ivwNewControl(ImodView *iv,
		  ImodControlProc draw_cb,
		  ImodControlProc quit_cb,
		  void *data)
{
     ImodControl ctrl;
     ImodControl *ctrlPtr;
     static int ctrlId  = 0;
     ctrl.userData = data;
     ctrl.draw_cb  = draw_cb;
     ctrl.close_cb = quit_cb;
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
	  iv->ctrlist->workID = (XtWorkProcId)0;
     }
     ctrlId++;
     ctrl.id = ctrlId;
     iv->ctrlist->top    = ctrlId;
     iv->ctrlist->active = ctrlId;
     ilistPush(iv->ctrlist->list, &ctrl);
     return(ctrlId);
}

/* delete the control associated with the inCtrlId value.
 * this will also call the close or quit method of the contorl.
 */
int ivwDeleteControl(ImodView *iv, int inCtrlId)
{
     ImodControl *ctrlPtr;
     int element = 0;

     if (!iv->ctrlist) return(0);
     if (iv->ctrlist->list->size < 1) return(0);
     ivwControlListDrawCancel(iv);
     iv->ctrlist->active = 0;
     ctrlPtr = ilistFirst(iv->ctrlist->list);
     while(ctrlPtr){
	  if (ctrlPtr->id == inCtrlId){
	       (*ctrlPtr->close_cb)(iv, ctrlPtr->userData, 0);
	       ilistRemove(iv->ctrlist->list, element);
	       ctrlPtr = ilistFirst(iv->ctrlist->list);
	       if (ctrlPtr)
		    iv->ctrlist->top = ctrlPtr->id;
	       return(0);
	  }
	  element++;
	  ctrlPtr = ilistNext(iv->ctrlist->list);
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

/*     printf("ivwControlPriority: %d\n", inCtrlId);  */

     if (!iv->ctrlist) return(0);

     iv->ctrlist->active = inCtrlId;
     if (!inCtrlId) return(iv->ctrlist->top);

     if (iv->ctrlist->top == inCtrlId)
	  return(inCtrlId);

     ivwControlListDrawCancel(iv);
     ctrlPtr = ilistFirst(iv->ctrlist->list);
     while(ctrlPtr){
	  if (ctrlPtr->id == inCtrlId){
	       ilistFloat(iv->ctrlist->list, element);
	       iv->ctrlist->top = inCtrlId;
	       return(0);
	  }
	  element++;
	  ctrlPtr = ilistNext(iv->ctrlist->list);
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
     if (!iv->ctrlist) return;
     if (!iv->ctrlist->workID) return;
     XtRemoveWorkProc(iv->ctrlist->workID);
     iv->ctrlist->workID = 0;
}

Boolean ivwWorkProc(XtPointer client_data)
{
     ImodView *iv = (ImodView *)client_data;

     ImodControl *ctrlPtr;

     if ((!iv->ctrlist)||(!iv->ctrlist->list->size)){
	  iv->ctrlist->workID = 0;
	  return(True);
     }

     ctrlPtr = ilistNext(iv->ctrlist->list);
     if (!ctrlPtr){
	  iv->ctrlist->workID = 0;
	  return(True);
     }
/*     printf("Drawing %d\n", ctrlPtr->id); */
     (*ctrlPtr->draw_cb)(iv, ctrlPtr->userData, iv->ctrlist->reason);
     return(False);
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
     
     ctrlPtr = ilistFirst(iv->ctrlist->list);
     if (ctrlPtr) {
/*	  printf("Drawing %d\n", ctrlPtr->id); */
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
     iv->ctrlist->workID = XtAppAddWorkProc
	  (App->context, (XtWorkProc)ivwWorkProc, (XtPointer)iv);

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
     ctrlPtr = ilistFirst(iv->ctrlist->list);
     while(ctrlPtr){
	  (*ctrlPtr->close_cb)(iv, ctrlPtr->userData, 0);
	  ctrlPtr = ilistNext(iv->ctrlist->list);
     }
     ilistDelete(iv->ctrlist->list);
     free(iv->ctrlist);
     iv->ctrlist = NULL;
}
#endif

