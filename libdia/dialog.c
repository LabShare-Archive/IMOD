#include <stdarg.h>
#include <stdlib.h>
#include <Xm/DialogS.h>
#include <Xm/PushBG.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Protocols.h>
#include <Xm/MainW.h>
#include <Xm/AtomMgr.h>
#include "diaP.h"

static void button_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)client;
     int i;

     dia->call_data = call;
     for (i = 0; i < dia->nbuttons; i++){
	  if (w == dia->button[i]){
	       if (dia->func[i])
		    dia->func[i](w, dia->udata[i], (XtPointer)dia);
	       return;
	  }
     }
     return;
}

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)client;

     if (dia->qfunc){
	  dia->call_data = call;
	  dia->qfunc(w, dia->qudata, dia);
     }else{
	  diaDestroyDialog(dia);
     }
     return;
}

diaDialog *diaVaCreateDialog(char *name,
			     Widget topLevel, XtAppContext context, ...)
{
     Atom    wmclose;
     va_list ap;
     int     flag;
     diaDialog *dia;
     Dimension h;
     Widget mainWin, workArea, menuBar = 0;
     int width = 0, height = 0;
     int i;

     dia = (diaDialog *)malloc(sizeof(diaDialog));
     if (!dia)
	  return(NULL);
     dia->context = context;
     dia->topLevel = topLevel;
     dia->nbuttons = 0;
     dia->placement = True;
     dia->qfunc = NULL;
     dia->widgetClass = xmDialogShellWidgetClass;
     dia->pane = dia->controlArea = 0;
     dia->workAreaFunc = NULL;
     dia->menuBarFunc  = NULL;
     dia->popup = True;
     va_start(ap, context);
     while( flag = va_arg(ap, int)){
	  switch(flag){
	     case DiaNcontrolButton:
	       if (dia->nbuttons == DIA_MAX_BUTTONS)
		    dia->nbuttons--;
	       dia->buttonName[dia->nbuttons] = va_arg(ap, char *);
	       dia->func[dia->nbuttons] = 
		 (void (*)(Widget,XtPointer,XtPointer))va_arg(ap, void *);
	       dia->udata[dia->nbuttons++] = va_arg(ap, void *);
	       break;
	       
	     case DiaNwindowQuit:
	       dia->qfunc = 
		 (void (*)(Widget,XtPointer,XtPointer))va_arg(ap, void *);
	       dia->qudata = va_arg(ap, void *);
	       break;

	     case DiaNautoPlacement:
	       dia->placement = va_arg(ap, int);
	       break;

	     case DiaNwidgetClass:
	       dia->widgetClass = va_arg(ap, WidgetClass);
	       break;

	     case DiaNworkAreaFunc:
	       dia->workAreaFunc = 
		 (void (*)(Widget,XtPointer,XtPointer))va_arg(ap, void *);
	       dia->workAreaFuncData = va_arg(ap, void *);
	       break;

	     case DiaNmenuBarFunc:
	       dia->menuBarFunc = 
		 (Widget (*)(Widget,XtPointer,XtPointer))va_arg(ap, void *);
	       dia->menuBarFuncData = va_arg(ap, void *);
	       break;

	     default:
	       break;
	  }
     }
     va_end(ap);

     XtVaGetValues(dia->topLevel, XmNvisual, &(dia->visual), NULL);
     
     dia->dialog = XtVaCreatePopupShell
	  (name, dia->widgetClass, dia->topLevel,
	   XmNvisual, dia->visual,
	   NULL);
     if (!dia->dialog)
	  return(NULL);
     if (dia->placement){
	  XtVaSetValues(dia->dialog, XmNdefaultPosition, True, NULL);
	  XtAddCallback(dia->dialog, XmNpopupCallback, dia_map_cb, NULL);
     }else{
	  
     }

     /* try and create menu bar. */
     mainWin = dia->dialog;
     if (dia->menuBarFunc){
	  mainWin = XtVaCreateWidget
	       ("mainwin", xmMainWindowWidgetClass, dia->dialog,
		NULL);
	  menuBar = dia->menuBarFunc(mainWin, dia->menuBarFuncData, dia);
	  if (!menuBar){
	       XtDestroyWidget(mainWin);
	       mainWin = dia->dialog;
	  }
     }

     if (dia->nbuttons){
	  workArea = dia->pane = XtVaCreateWidget
	       ("pane", xmPanedWindowWidgetClass, mainWin,
		XmNsashWidth,  1,
		XmNsashHeight, 1,
		NULL);
	  
	  dia->workArea = XtVaCreateWidget
	       ("frame", xmFrameWidgetClass, dia->pane, 
		NULL);

	  if (dia->workAreaFunc)
	       dia->workAreaFunc(dia->workArea, dia->workAreaFuncData, dia);

	  XtManageChild(dia->workArea);

	  dia->controlArea = XtVaCreateWidget 
	       ("form", xmFormWidgetClass, dia->pane,
		XmNfractionBase,    (dia->nbuttons * 3) + 1,
		NULL);
	  
	  for(i = 0; i < dia->nbuttons; i++){
	       dia->button[i] = XtVaCreateManagedWidget
		    (dia->buttonName[i], xmPushButtonGadgetClass, 
		     dia->controlArea,
		     XmNtopAttachment,    XmATTACH_FORM,
		     XmNbottomAttachment, XmATTACH_FORM,
		     XmNleftAttachment,   XmATTACH_POSITION,
		     XmNleftPosition,     (i*3)+1,
		     XmNrightAttachment,  XmATTACH_POSITION,
		     XmNrightPosition,    (i*3)+3,  
		     NULL);
	       XtAddCallback(dia->button[i], XmNactivateCallback,
			     button_cb, (XtPointer)dia);
	  }
	  XtManageChild(dia->controlArea);
	  XtVaGetValues (dia->button[0], XmNheight, &h, NULL);
	  XtVaSetValues (dia->controlArea, 
			 XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);

	  XtManageChild(dia->pane);
	  
     }else{
	  workArea = dia->workArea = XtVaCreateWidget
	       ("frame", xmFrameWidgetClass, mainWin, NULL);
	  if (dia->workAreaFunc)
	       dia->workAreaFunc(dia->workArea, dia->workAreaFuncData, dia);
	  XtManageChild(dia->workArea);
     }
     wmclose = XmInternAtom( XtDisplay(dia->topLevel),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(dia->dialog, wmclose, quit_cb,
			     (caddr_t)(dia));

     /* DNM 3/9/01: add this to prevent double destruction of windows */
     XtVaSetValues(dia->dialog, XmNdeleteResponse, XmDO_NOTHING, NULL);

     if (mainWin != dia->dialog){
	 XtManageChild(menuBar);
	 XtManageChild(mainWin);
	 XmMainWindowSetAreas (mainWin, menuBar, NULL,
			       NULL, NULL, workArea);
	  
     }

     if (dia->popup)
	  XtPopup (dia->dialog, XtGrabNone);

     return(dia);
}

void diaPopup(diaDialog *dia)
{
     if (!dia)
	  return;
     if (!dia->dialog)
	  return;
     XtManageChild(dia->workArea);

     XtPopup (dia->dialog, XtGrabNone);
     return;
}

Widget diaGetWorkArea(diaDialog *dia)
{
     return(dia->workArea);
}

XtPointer diaGetCallData(diaDialog *dia)
{
     return(dia->call_data);
}
void  diaDestroyDialog(diaDialog *dia)
{
     if (dia){
	  if (dia->dialog){
	       XtDestroyWidget(dia->dialog);
	  }
	  free(dia);
     }
     return;
}
