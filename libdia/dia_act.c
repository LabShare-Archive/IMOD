#include <Xm/DialogS.h>
#include <Xm/SelectioB.h>
#include <Xm/FileSB.h>
#include <stdlib.h>
#include "diaP.h"


struct fsdstruct 
{ 
     Widget dia;
     Widget box;
     XtPointer udata;
     void (*cbf)(Widget,XtPointer,XtPointer);
     char *name;
};

static void efile_quit_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     XmFileSelectionBoxCallbackStruct *data =
          (XmFileSelectionBoxCallbackStruct *) call_data;
     struct fsdstruct *fsd = ( struct fsdstruct *) client_data;
     XtUnmanageChild(fsd->dia);
     XtDestroyWidget(fsd->dia);
     free(fsd);
}

static void efile_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     XmFileSelectionBoxCallbackStruct *data =
          (XmFileSelectionBoxCallbackStruct *) call_data;
     struct fsdstruct *fsd = ( struct fsdstruct *) client_data;
     char *filename;

     XmStringGetLtoR(data->value, XmSTRING_DEFAULT_CHARSET, 
		     &(filename));
     fsd->cbf(w, fsd->udata, filename);
     efile_quit_cb(w, client_data, call_data);
     return;
}
static void efile_app_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     XmFileSelectionBoxCallbackStruct *data =
          (XmFileSelectionBoxCallbackStruct *) call_data;
     struct fsdstruct *fsd = ( struct fsdstruct *) client_data;
     char *filename;

     XmStringGetLtoR(data->value, XmSTRING_DEFAULT_CHARSET, 
		     &(filename));
     fsd->cbf(w, fsd->udata, filename);
     return;
}

static void efile_help_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{ 
     struct fsdstruct *fsd = ( struct fsdstruct *) client_data;
     dia_puts(fsd->name);

}

void diaEasyFileAct(char *name, void (*fcb)(), XtPointer udata)
{
     Widget hw; /* the help widget */
     XmString can = XmStringCreateSimple("Cancel");
     XmString app = XmStringCreateSimple("Apply");
     struct fsdstruct *fsd = ( struct fsdstruct *)
	  malloc(sizeof( struct fsdstruct));
     
     if (!fsd) return;
     fsd->cbf = fcb;
     fsd->udata = udata;
     fsd->name = name;


     fsd->dia = XtVaCreatePopupShell
	  (name, xmDialogShellWidgetClass, dia_get_toplevel(),
	   XmNvisual, dia_getvisual(),
	   NULL);
     if (!fsd->dia){
	  free(fsd);
	  return;
     }
     fsd->box = XmCreateFileSelectionBox(fsd->dia, name, 0 , 0);

     XtAddCallback( fsd->box, XmNokCallback, efile_cb, fsd);
     XtAddCallback( fsd->box, XmNcancelCallback, efile_app_cb, fsd);
     XtAddCallback( fsd->box, XmNhelpCallback, efile_quit_cb, fsd);

     XtVaSetValues(fsd->box,
		   XmNvisual, dia_getvisual(),
		   XmNcancelLabelString, app, 
		   XmNhelpLabelString, can,
		   NULL); 

     XmStringFree(app); 
     XmStringFree(can);
     XtManageChild (fsd->box);
     XtPopup (fsd->dia, XtGrabNone);
}
