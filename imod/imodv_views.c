/*  IMOD VERSION 2.50
 *
 *  imodv_views.c -- Edit, store, and access view list in model
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowB.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <dia.h>
#include <stdio.h>
#include <string.h>
#include "imodv.h"

struct imodv_viewed
{
     diaDialog *dia;
     ImodvApp  *a;
     Widget     viewno_widget;
     Widget     wList;
     Widget     text;
};

static struct imodv_viewed *ViewEdit = NULL;
static Widget make_work_area(ImodvApp *a, Widget top);
static void build_list(ImodvApp *a);

static int auto_store = 1;

void imodvUpdateView(ImodvApp *a)
{
    imodvControlSetView(a);
    imodvObjedNewView();
    imodvSetLight(a->imod->view);
    imodvMenuLight(a->imod->view->world & VIEW_WORLD_LIGHT);

    if (a->imod->view->world & VIEW_WORLD_WIREFRAME)
	a->wireframe = 1;    
    else
	a->wireframe = 0;
    imodvMenuWireframe(a->imod->view->world & VIEW_WORLD_WIREFRAME);

    if (a->imod->view->world & VIEW_WORLD_LOWRES)
	a->lowres = 1;    
    else
	a->lowres = 0;
    imodvMenuLowres(a->imod->view->world & VIEW_WORLD_LOWRES);
    imodvDepthCueSetWidgets();
}

void imodvUpdateModel(ImodvApp *a)
{
     imodvUpdateView(a);
     if (!ViewEdit)
          return;
     XmListDeleteAllItems(ViewEdit->wList);
     build_list(a);
}

static void manage_world_flags(ImodvApp *a, Iview *view)
{
     if (a->lighting)
       view->world |= VIEW_WORLD_LIGHT;
     else
       view->world &= ~VIEW_WORLD_LIGHT;
     if (a->wireframe)
       view->world |= VIEW_WORLD_WIREFRAME;
     else
       view->world &= ~VIEW_WORLD_WIREFRAME;
     if (a->lowres)
       view->world |= VIEW_WORLD_LOWRES;
     else
       view->world &= ~VIEW_WORLD_LOWRES;
}

void imodvAutoStoreView(ImodvApp *a)
{
     if (!auto_store || !a->imod->cview)
	  return;
     manage_world_flags(a, a->imod->view);
     imodViewStore(a->imod, a->imod->cview);
}

static int imodvListGetKbdItemPos(Widget w)
{
#if XmVERSION == 1
#if XmREVISION == 1
#define NOXmListGetKbdItemPos
#endif
#endif

#ifdef NOXmListGetKbdItemPos
    int *pos;
    int npos;
    if (XmListGetSelectedPos(w, &pos, &npos)){
	if (npos > 0){
	    npos = *pos;
	    XtFree(pos);
	    return npos;
	}else return 0;
    }else return 0;
#else
    return(XmListGetKbdItemPos(w));
#endif
}

static void help_cb()
{
     dia_vasmsg
	  ("View Edit Dialog Help.\n\n",
	   "\tWhen you store a view, you save the orientation, size, and "
	   "lighting conditions of the whole model, and also the color,"
	   " display type, material, and other properties of all of the "
	   "objects\n\n"
	   "\tClick once in the list to select a view, and click twice to"
	   " display it.\n",
	   "\tThe default view can be seen by pressing the [Default] button "
	   "or double clicking the Default View in the list.\n",
	   "\tThe [Goto View] button displays the currently selected view.\n",
	   "\tThe [New View] button adds a new view to the list, with the "
	   "properties of the current display.\n",
	   "\tThe [Store] button stores the properties of the current display"
	   " in the currently selected view.\n"
	   "\tThe [Delete] button deletes the currently selected view.\n"
	   "\tThe [Save Model] button will save the model to a file and make "
	   "the existing file be a backup with extension ~ (available only "
	   "in imodv standalone mode).\n"
	   "\tThe [Autostore] toggle button controls whether view changes are "
	   "automatically saved for you.  If this button is selected, the "
	   "current display properties are stored into the current view "
	   "whenever you go to a different view and whenever you save the "
	   "model.  In other words, what you see is what you save, without "
	   "your having to push the [Store] button.  To return to the stored "
	   "settings of a view when operating in this mode, select and "
	   "redisplay that view (i.e., double-click or push [Goto View]) "
	   "before going on to a different view or saving the model.  If "
	   "[Autostore] is not selected, your display changes are not saved "
	   "into the current view unless you push [Store]."
	   "\n\n"
	   "\tYou can edit the name of the currently selected view in the "
	   "edit box at the bottom.\n\n"
	   "\tNote the distinction between the currently selected view, which "
	   "is the one highlighted in the list, and the currently displayed "
	   "view.  The selected view is not displayed unless you double-click"
	   " it or push the [Goto View] button.\n",
	   "\tThe Default View is set when the Model View window is opened"
	   " and cannot be changed.  It will cease to exist when the Model"
	   " View window is closed.  If this view is precious to you, you"
	   " should either store it as a separate view, or redisplay it before"
	   " exiting Model View.\n",
	   NULL);
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     struct imodv_viewed *ved = (struct imodv_viewed *)client;
     Widget parent = w;
     
     XtManageChild(make_work_area(ved->a, w));
     return;
}

static void done_cb(Widget w, XtPointer client, XtPointer call)
{
     /* DNM: passing dia in the call spot didn't work on PC, and the
	client data is enough for the job */
     /*  diaDialog *dia = (diaDialog *)call; */
     struct imodv_viewed *ved = (struct imodv_viewed *)client;

     ViewEdit =  NULL;
     diaDestroyDialog(ved->dia);
     ved->dia = NULL;
     return;
}

static void SetNewLabel(struct imodv_viewed *ved, Iview *vw)
{
    static int i = 1;
    char label[32];
    XmString str;
    while(i < 1000){
	sprintf(label, "view %d", i);
	str = XmStringCreateSimple(label);
	if(!XmListItemExists(ved->wList, str)){
	    strcpy(vw->label, label);
	    XmStringFree(str);
	    return;
	}
	XmStringFree(str);
	i++;
    }
    strcpy(vw->label, "new label");
}

static void default_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imodv_viewed *ved = (struct imodv_viewed *)client;

     if (!ved->a->imod) return;

     /* Store the current view before changing to default */
     imodvAutoStoreView(ved->a);
     
     ved->a->imod->cview = 0;
     imodViewModelDefault(ved->a->imod, ved->a->imod->view);
     imodViewUse(ved->a->imod);
     imodvUpdateView(ved->a);
     imodvDraw(ved->a);
     return;
}

void imodvViewEditDialog(ImodvApp *a, int state)
{
     static struct imodv_viewed ved;
     static int first = 1;

     if (first){
	  ved.dia = NULL;
	  first = 0;
     }
     if (!state){
	  /* DNM: just pass client, that is all that's needed */
	  if (ved.dia)
	       done_cb(NULL, (XtPointer)&ved, NULL);
	  return;
     }
     if (ved.dia){
	  XRaiseWindow(a->display, XtWindow(ved.dia->dialog));
	  return;
     }
     ViewEdit = &ved;
     ved.a = a;

     ved.dia = diaVaCreateDialog
	  ("Imodv: View Edit", a->topLevel, a->context,
	   DiaNcontrolButton, "Done", done_cb, (XtPointer)&ved,
	   DiaNcontrolButton, "Help", help_cb, (XtPointer)&ved,
	   DiaNworkAreaFunc, workarea_cb, (XtPointer)&ved,
	   DiaNwindowQuit, done_cb, (XtPointer)&ved,
	   0);
     return;
}



/* Handles actions on the Edit Field list. */
static void list_cb(Widget w, XtPointer client, XtPointer call)
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *)call;
    struct imodv_viewed *ved = (struct imodv_viewed *)client;
    int currentview =   cbs->item_position - 1;
    
    /* If changing views, store the current view before changing */
    if (currentview != ved->a->imod->cview)
	 imodvAutoStoreView(ved->a);

    ved->a->imod->cview = currentview;
    if (ved->a->imod->cview == 0) {
         default_cb(w, client, call);
	 return;
    }

    imodViewUse(ved->a->imod);
    XtVaSetValues(ved->text, XmNvalue, ved->a->imod->view[currentview].label,
		  NULL);
    imodvUpdateView(ved->a);
    imodvDraw(Imodv);
    return;
}

static void select_cb(Widget w, XtPointer client, XtPointer call)
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *)call;
    struct imodv_viewed *ved = (struct imodv_viewed *)client;
    int whichview =   cbs->item_position - 1;
    XtVaSetValues(ved->text, XmNvalue, ved->a->imod->view[whichview].label,
		  NULL);
}

/*
 * The goto view callback.
 */
static void see_view_cb(Widget w, XtPointer client, XtPointer call)
{
    struct imodv_viewed *ved = (struct imodv_viewed *)client;
    int pos;
    if (!ved->a->imod) return;
    pos = imodvListGetKbdItemPos(ved->wList);
    if (pos)
	 pos--;

    /* If changing views, store the current view before changing */
    if (pos != ved->a->imod->cview)
	 imodvAutoStoreView(ved->a);

    ved->a->imod->cview = pos;

    if (ved->a->imod->cview == 0) {
         default_cb(w, client, call);
	 return;
    }
     
    imodViewUse(ved->a->imod);
    

    imodvUpdateView(ved->a);
    imodvDraw(Imodv);
}


/* 
 * The store view callback.
 * Sets the view in the window to the current view. 
 */
static void set_view_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imodv_viewed *ved = (struct imodv_viewed *)client;
     int pos = imodvListGetKbdItemPos(ved->wList);
     Iview *view = ved->a->imod->view;
     if (pos)
	  pos--;

    /* If changing views, store the current view before changing */
    if (pos != ved->a->imod->cview)
	 imodvAutoStoreView(ved->a);

     ved->a->imod->cview = pos;
     
     if (!ved->a->imod->cview)
          return;

     manage_world_flags(ved->a, view);

     imodViewStore(ved->a->imod, ved->a->imod->cview);


     return;
}

/* Make a new view  */
static void nnew_view_cb(Widget w, XtPointer client, XtPointer call)
{
    struct imodv_viewed *ved = (struct imodv_viewed *)client;
    int cview;
    Iview *view = ved->a->imod->view;
    XmString str;

    if (!ved->a->imod) return;

    imodvAutoStoreView(ved->a);

    cview = ved->a->imod->cview;
    imodViewModelNew(ved->a->imod);
    view = ved->a->imod->view;
    ved->a->imod->cview = ved->a->imod->viewsize - 1;
    
    if (cview == ved->a->imod->cview) {
	fprintf(stderr,"imodv: create view error\n");
	return; /* no view created. */
    }

    manage_world_flags(ved->a, view);

    SetNewLabel(ved, &ved->a->imod->view[ved->a->imod->cview]);
    imodViewStore(ved->a->imod, ved->a->imod->cview);

    str = XmStringCreateSimple(ved->a->imod->view[ved->a->imod->cview].label);
    XtVaSetValues(ved->text, XmNvalue, 
		  ved->a->imod->view[ved->a->imod->cview].label, NULL);
    XmListAddItem(ved->wList, str, ved->a->imod->viewsize);
    XmListSelectPos(ved->wList, ved->a->imod->viewsize, False);
    XmStringFree(str);
    return;
}

static void delete_cb(Widget w, XtPointer client, XtPointer call)
{
    struct imodv_viewed *ved = (struct imodv_viewed *)client;
    int cview = ved->a->imod->cview;
    Imod *imod = ved->a->imod;
    int i;
    int pos = imodvListGetKbdItemPos(ved->wList);
    if (!imod) return;
    if (pos <= 1) return;
    
    if (imod->viewsize < 2) return;

    for(i = pos-1; i < imod->viewsize; i++){
	imod->view[i] = imod->view[i+1];
    }
    XmListDeletePos(ved->wList, pos);

    imod->viewsize--;
    imod->cview = 0;
    return;
}

static void label_cb(Widget w, XtPointer client, XtPointer call)
{
    struct imodv_viewed *ved = (struct imodv_viewed *)client;
    XmString str;
    char *st;
    int pos = imodvListGetKbdItemPos(ved->wList);
    if (pos <= 1) return;
    st = XmTextGetString(w);
    strncpy( ved->a->imod->view[pos-1].label,
	    st, 31);
    XtFree(st);

    str = XmStringCreateSimple(ved->a->imod->view[pos-1].label);
    XmListReplaceItemsPos(ved->wList, &str, 1, pos);
    XmStringFree(str);

    return;
}

static void autostore_cb(Widget w, XtPointer client, XtPointer call)
{
     auto_store = 1 - auto_store;
}

static Widget make_work_area(ImodvApp *a, Widget top)
{
     Widget container, frame, col, row, button, bcol;
     char listlabel[32];
     int i, n = 0;
     Arg args[7];

     frame = XtVaCreateWidget
	  ("frame", xmFrameWidgetClass, top, NULL);

     container = XtVaCreateManagedWidget
          ("rowcol", xmRowColumnWidgetClass, frame,
/*	   XmNorientation, XmHORIZONTAL, */
           NULL);

     XtVaCreateManagedWidget
	  ("Select View:", xmLabelWidgetClass, container, NULL);

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, container,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     {
/*	 if (a->imod){
	     while(a->imod->viewsize < 2)
		 imodViewModelNew(a->imod);
	 } */
	 XtSetArg(args[n], XmNvisibleItemCount, 11); n++;
	 XtSetArg(args[n], XmNvalue, 1); n++;
	 XtSetArg(args[n], XmNselectionPolicy, XmSINGLE_SELECT); n++; 
	 ViewEdit->wList = XmCreateScrolledList(col, "ViewList", args, n);

	 build_list(a);
	 XtAddCallback(ViewEdit->wList, XmNdefaultActionCallback, 
		       list_cb, (XtPointer)ViewEdit);
	 XtAddCallback(ViewEdit->wList, XmNsingleSelectionCallback, 
		       select_cb, (XtPointer)ViewEdit);

	 XtManageChild(ViewEdit->wList);


	 row = XtVaCreateWidget
	     ("rowcol", xmRowColumnWidgetClass, col,
	      NULL);
	 {
	     button = XtVaCreateManagedWidget
		 ("Default", xmPushButtonWidgetClass, row,
		  NULL);
	     XtAddCallback(button, XmNactivateCallback,
			   default_cb, (XtPointer)ViewEdit);
	     
	     button = XtVaCreateManagedWidget
		 ("Store", xmPushButtonWidgetClass, row, NULL);
	     XtAddCallback(button, XmNactivateCallback,
			   set_view_cb, (XtPointer)ViewEdit);
	     
	     
	     button = XtVaCreateManagedWidget
		 ("New View", xmPushButtonWidgetClass, row,
		  NULL);
	     XtAddCallback(button, XmNactivateCallback,
			   nnew_view_cb, (XtPointer)ViewEdit);
	     
	     button = XtVaCreateManagedWidget
		 ("Go To View", xmPushButtonWidgetClass, row,
		  NULL);
	     XtAddCallback(button, XmNactivateCallback,
			   see_view_cb, (XtPointer)ViewEdit);
	     button = XtVaCreateManagedWidget
		 ("Delete", xmPushButtonWidgetClass, row,
		  NULL);
	     XtAddCallback(button, XmNactivateCallback,
			   delete_cb, (XtPointer)ViewEdit);
	     button = XtVaCreateManagedWidget
		  ("Save Model", xmPushButtonWidgetClass, row,
		   NULL);
	     XtAddCallback(button, XmNactivateCallback,
			   imodv_file_save_cb, (XtPointer)a);
	     if (!a->standalone) XtSetSensitive(button, False);

	     bcol = XtVaCreateWidget
		  ("rowcol", xmRowColumnWidgetClass, row,
		   NULL);
	     button = XtVaCreateManagedWidget
		  ("Autostore", xmToggleButtonWidgetClass, bcol,
		   NULL);

	     XtAddCallback(button,
			   XmNvalueChangedCallback, autostore_cb, NULL);
	     if (auto_store)
		  XmToggleButtonSetState(button, True, False);
	     else
		  XmToggleButtonSetState(button, False, False);
	     XtManageChild(bcol);


	 }XtManageChild(row);
     }XtManageChild(col);
     
     
     ViewEdit->text = XtVaCreateManagedWidget
	 ("View Label", xmTextWidgetClass, container,
	  XmNcolumns, 31,
	   XmNvalue, ViewEdit->a->imod->view[a->imod->cview].label,
	  NULL);
     XtAddCallback( ViewEdit->text, XmNactivateCallback,
		   label_cb, (XtPointer)ViewEdit);
     
     XmListSelectPos(ViewEdit->wList, Imodv->imod->cview + 1, False);
     
     XtManageChild(container);
     XtManageChild(frame);
     return(frame);
}     

static void build_list(ImodvApp *a)
{
     XmString str;
     int i;

     sprintf(a->imod->view->label, "Original Default View");
     str = XmStringCreateSimple("Original Default View");
     XmListAddItem(ViewEdit->wList, str, 1);
     XmStringFree(str);
     
     for(i = 1; i < a->imod->viewsize; i++){
          if (!a->imod->view[i].label[0])
	       SetNewLabel(ViewEdit, &a->imod->view[i]);
	  str = XmStringCreateSimple(a->imod->view[i].label);
	  XmListAddItem(ViewEdit->wList, str, i+1);
	  XmStringFree(str);
     }
}
