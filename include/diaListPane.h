#ifndef DIA_LSPANE_H
#define DIA_LSPANE_H


typedef struct
{
     char      *label;        /* Label used for list widget.      */

     /* Function used to make edit area. */
     XtCallbackProc mkwidget;

     /* Function to adjust internal data */
     XtCallbackProc setwidget;

     XtPointer clientData;    /* Client data                      */
     Widget    control;       /* Runtime widget storage.          */

 }diaListPaneField;


typedef struct
{
    int maxField;
    int curField;
    diaListPaneField *field;

    Widget list;
    Widget active;

    Widget form, frame;
    Widget controlForm;

}diaListPane;


diaListPane *diaNewListPane(Widget parent);
void diaListPaneManage(diaListPane *lp);
void diaListPaneUpdate(diaListPane *lp);

int diaListPaneAddField(diaListPane *lp, 
			char *inName,
			XtCallbackProc makeProc,
			XtCallbackProc setProc,
			XtPointer client);


#endif
