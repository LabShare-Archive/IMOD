#include <string.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/List.h>
/*#include "diaP.h"*/
#include <diaListPane.h>


static void list_cb(Widget w, XtPointer client, XtPointer call)
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *)call;
    diaListPane *lp = (diaListPane *)client;
    diaListPaneField *field;

    lp->curField = cbs->item_position - 1;
    field = &lp->field[lp->curField];

    if (lp->active)
	XtUnmanageChild(lp->active);

    if ((field) && (field->control)){
	XtManageChild(field->control);
	lp->active = field->control;
	XRaiseWindow(XtDisplay(field->control), XtWindow(field->control));
	if (field->setwidget)
	    field->setwidget(field->control, field->clientData, field);

    }
    return;
}

diaListPane *diaNewListPane(Widget parent)
{
    Arg args[7]; int n = 0;
    XmString str;

    diaListPane *lp = (diaListPane *)malloc(sizeof(diaListPane));
    if (!lp) return NULL;

    /* Initialize data to default settings. */
    lp->maxField = lp->curField = 0;
    lp->active = 0;
    lp->field = NULL;
    
    lp->form = XtVaCreateWidget
	("diaListPaneForm", xmFormWidgetClass, parent, NULL);

  
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;

    XtSetArg(args[n], XmNvisibleItemCount, 10); n++;
    XtSetArg(args[n], XmNvalue, 1); n++;
    XtSetArg(args[n], XmNselectionPolicy, XmSINGLE_SELECT); n++;
    lp->list = XmCreateScrolledList(lp->form, "EditList", args, n);

    XtAddCallback(lp->list, XmNsingleSelectionCallback, 
		  list_cb, (XtPointer)lp);

    lp->frame = XtVaCreateWidget
          ("frame", xmFrameWidgetClass, lp->form,
           XmNleftAttachment, XmATTACH_WIDGET,
           XmNleftWidget, lp->list,
           XmNtopAttachment, XmATTACH_WIDGET,
           XmNtopWidget, lp->form,
           XmNrightAttachment, XmATTACH_FORM,
           XmNbottomAttachment, XmATTACH_FORM,
           XmNshadowType, XmSHADOW_IN,
           NULL);

    lp->controlForm = XtVaCreateWidget
          ("diaControlForm", xmFormWidgetClass, lp->frame, NULL);

    return(lp);
}

void diaListPaneManage(diaListPane *lp)
{
    if ((lp) && (lp->field) && (lp->field[lp->curField].control))
	 XtManageChild(lp->field[lp->curField].control);

    XtManageChild(lp->controlForm);
    XtManageChild(lp->frame);
    XtManageChild(lp->list);
    XtManageChild(lp->form);
    return;
}

void diaListPaneUpdate(diaListPane *lp)
{
    if ((lp) && (lp->field) && (lp->field[lp->curField].setwidget))
	lp->field[lp->curField].setwidget
	    (lp->field[lp->curField].control,
	     (XtPointer)lp->field[lp->curField].clientData,
	     (XtPointer)&lp->field[lp->curField]);
}

int diaListPaneAddField(diaListPane *lp, 
		    char *inName,
		    XtCallbackProc makeProc,
		    XtCallbackProc setProc,
		    XtPointer client)
{
    diaListPaneField *field;
    XmString str;

    if ((!lp) || (!inName)) return -1;

    field = (diaListPaneField *)
	malloc(sizeof(diaListPaneField) * (lp->maxField + 1));
    if (!field) return -1;
    if (lp->field){
	memcpy(field, lp->field, sizeof(diaListPaneField) * lp->maxField);
	free(lp->field);
    }
    lp->field = field;
    field = &lp->field[lp->maxField];
    lp->maxField++;

    str = XmStringCreateSimple(inName);
    XmListAddItem(lp->list, str, 0);
    XmStringFree(str);

    field->label      = inName; 
    field->mkwidget   = makeProc;
    field->setwidget  = setProc;
    field->clientData = client;
    field->control    = 0;

    if (makeProc){
	makeProc(lp->controlForm, client, field);
    }
    /*
    if (field->control){
	XtSetMappedWhenManaged(field->control, True);
	XtUnmanageChild( field->control);
    }
*/
    return(lp->maxField - 1);
}

