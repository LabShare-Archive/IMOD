/*****************************************************************************
 *                                                                           *
 *   FILE: dia_fact.c                                                        *
 *                                                                           *
 *   PURPOSE: Act on file(s) dialog.                                         *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0  James Kremer  kremer@beagle.colorado.edu               *
 *                                                                           *
 *****************************************************************************
 *   Copyright (C) 1994,1995 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <Xm/DialogS.h>
#include <Xm/PushBG.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <stdlib.h>
#include "diaP.h"
#include "dia.h"


struct Fact_dialog{
     char     *infilename;
     char     *outfilename;
     void     (*cb)(Widget,  XtPointer, XtPointer);
     XtPointer client;
     Widget    dialog;
     Widget    fintext;
     Widget    fouttext;
     Widget    finbut;
     Widget    foutbut;
     int       exitonok;
     int       gettingfile;
};

int dia_fact_driver(char *prompt, char *def1, char *def2, 
		    void (*cb)(), void *client_data, int files, int exitonok);

void dia_fact_cancel_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Fact_dialog *fd = (struct Fact_dialog *)client_data;

     if (fd->gettingfile)
	  return;

     XtDestroyWidget(fd->dialog);
     if (fd->infilename)
	  free(fd->infilename);
     if (fd->outfilename)
	  free(fd->outfilename);
     free(fd);
}

void dia_fact_ok_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Fact_dialog *fd = (struct Fact_dialog *)client_data;
     
     if (fd->infilename)
	  free(fd->infilename);
     if (fd->outfilename)
	  free(fd->outfilename);

     XtVaGetValues(fd->fintext, XmNvalue, &(fd->infilename), NULL);

     if (fd->fouttext)
	  XtVaGetValues(fd->fouttext, XmNvalue, &(fd->outfilename), NULL);

     fd->cb(w, fd->client, (XtPointer)fd);
     if (fd->exitonok){
	  dia_fact_cancel_cb(w, client_data, call_data);
     }
}

void dia_fact_file_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Fact_dialog *fd = (struct Fact_dialog *)client_data;
     char *filename;

     if (w == fd->finbut){
	  fd->gettingfile++;
	  filename = dia_filename("Enter Input Filename.");
	  XtVaSetValues(fd->fintext, XmNvalue, filename, NULL);
	  fd->gettingfile--;
     }else{
	  fd->gettingfile++;
	  filename = dia_filename("Enter Output Filename.");
	  XtVaSetValues(fd->fouttext, XmNvalue, filename, NULL);
	  fd->gettingfile--;
     }
     free(filename);
}

int dia_fact(char *prompt, char *def1, char *def2,
	     void (*cb)(), void *client_data)
{
     return(dia_fact_driver(prompt, def1, def2, cb, client_data, 2, 0));
}

int dia_fload(char *prompt, char *def1,
	      void (*cb)(), void *client_data)
{
     return(dia_fact_driver(prompt, def1, NULL, cb, client_data, 1, 0));
}

int dia_fact_driver(char *prompt, char *def1, char *def2, 
	     void (*cb)(), void *client_data, int files, int exitonok)
{
     struct Fact_dialog *fd;
     Widget dialog, pane, formw, form;
     Widget label, scale, button;
     Widget f1but, f2but;
     Widget window, rowcol, row;
     int done = 0;
     Dimension h;

     fd = (struct Fact_dialog *)malloc(sizeof(struct Fact_dialog));
     if (!fd)
	  return(-1);
     fd->cb = cb;
     fd->client = (XtPointer)client_data;
     fd->infilename = NULL;
     fd->outfilename = NULL;
     
     dialog = XtVaCreatePopupShell
	  (Dia_title,  xmDialogShellWidgetClass,  Dia_toplevel,
	   XmNvisual, dia_getvisual(),
	   XmNdefaultPosition, True,
	   XmNdeleteResponse, XmDESTROY,
	   XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
	   NULL);
     if (!dialog){
	  free(fd);
	  return(-1);
     }
     fd->dialog = dialog;
     fd->gettingfile = 0;
     XtAddCallback(dialog, XmNpopupCallback, dia_map_cb, NULL);
         
     pane = XtVaCreateWidget
	  ("pane", xmPanedWindowWidgetClass, dialog,
	   XmNsashWidth,  1,
	   XmNsashHeight, 1,
	   NULL);

     formw = XtVaCreateWidget
	  ("form1", xmFormWidgetClass, pane, 
	   NULL);

     /**********************************************************************/
     /*  Work area.                                                        */
     
     label = XtVaCreateManagedWidget
	  (prompt, xmLabelWidgetClass, formw,
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNtopAttachment,    XmATTACH_FORM,
	   NULL);

     fd->finbut = XtVaCreateManagedWidget
	  ("Input  File", xmPushButtonGadgetClass, formw,
	   XmNtopAttachment,    XmATTACH_WIDGET,
	   XmNtopWidget,        label,
	   XmNleftAttachment,   XmATTACH_FORM,
	   XmNwidth, 100,
	   NULL);
     XtAddCallback(fd->finbut, XmNactivateCallback, dia_fact_file_cb, 
		   (XtPointer)fd); 

     fd->fintext = XtVaCreateManagedWidget
	  ( "text", xmTextWidgetClass, formw,
	   XmNtopAttachment,    XmATTACH_WIDGET,
	   XmNtopWidget,        label,
	   XmNrightAttachment,  XmATTACH_FORM,
	   XmNleftAttachment,   XmATTACH_WIDGET,
	   XmNleftWidget,       fd->finbut,
	   NULL);
     if (def1)
	  XtVaSetValues(fd->fintext, XmNvalue, def1, NULL);

     if (files > 1){
	  fd->foutbut = XtVaCreateManagedWidget
	       ("Output File", xmPushButtonGadgetClass, formw,
		XmNtopAttachment,    XmATTACH_WIDGET,
		XmNtopWidget,        fd->fintext,
		XmNleftAttachment,   XmATTACH_FORM,
		XmNwidth, 100,
		NULL);
	  XtAddCallback(fd->foutbut, XmNactivateCallback, dia_fact_file_cb, 
			(XtPointer)fd); 
	  
	  fd->fouttext = XtVaCreateManagedWidget
	       ( "text", xmTextWidgetClass, formw,
		XmNrightAttachment,   XmATTACH_FORM,
		XmNbottomAttachment,  XmATTACH_FORM,
		XmNtopAttachment,     XmATTACH_WIDGET,
		XmNtopWidget,         fd->fintext,
		XmNleftAttachment,    XmATTACH_WIDGET,
		XmNleftWidget,        fd->foutbut,
		NULL);
	  if (def2)
	       XtVaSetValues(fd->fouttext, XmNvalue, def2, NULL);
     }else{
	  fd->foutbut = 0;
	  fd->fouttext = 0;
     }
	  



     /**********************************************************************/
     XtManageChild (formw);

     /* ACTION AREA */
     form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane,
			      XmNfractionBase,    11,
			      NULL);
     
     button = XtVaCreateManagedWidget
	  ("Apply", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         1,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        5,
	   XmNshowAsDefault,        True,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   dia_fact_ok_cb, (XtPointer)fd);

     button = XtVaCreateManagedWidget
	  ("Done", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         6,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        10,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   dia_fact_cancel_cb, (XtPointer)fd);

     XtManageChild(form);
     XtVaGetValues (button, XmNheight, &h, NULL);
     XtVaSetValues (form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
     XtManageChild (pane);

     XtPopup (dialog, XtGrabNone);

     return(0);
}

