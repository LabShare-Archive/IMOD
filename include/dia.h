/*****************************************************************************
 *                                                                           *
 *   FILE: dia.h                                                             *
 *                                                                           *
 *   PURPOSE: Dialog library header file.                                    *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0  James Kremer  kremer@beagle.colorado.edu               *
 *                                                                           *
 *****************************************************************************
 *   Copyright (C) 1994-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

    $Log$
    Revision 3.1  2002/11/25 19:13:31  mast
    Added arguments to (*cb)() in function calls

*/

#ifndef DIA_H
#define DIA_H

#include <Xm/Xm.h>
#include <xcramp.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/*****************************************************************************/
/* dia reason defines for callback functions.                                */
#define DIA_INIT            1
#define DIA_SLIDER_CHANGED  9
#define DIA_VALUE_CHANGED  10
#define DIA_SLIDER_DRAG    11
#define DIA_APPLY          12
#define DIA_CANCEL         13
#define DIA_OK             14
#define DIA_HELP           15
#define DIA_EXIT           99

/*****************************************************************************/
/* Act on a file.                                                            */
typedef struct fileaction_cbs{
     char     *infilename;
     char     *outfilename;
} DiaFactCallbackStruct;

typedef struct fileaction_input{
     int files;         /* no. of files 0-2.             */
     char *prompt;      /*                               */
     void (*cb)(Widget,XtPointer,XtPointer); /* callback function */
     void *client_data; /* client data                   */
     char **labels;     /* labels for filename buttons.  */
     char **filenames;  /* default filenames. ok if NULL */
     char *ok;          /* will use "ok" if NULL         */
     char *cancel;      /* will use "cancel" if NULL     */
} DiaFactInputStruct;

/****************************************************************************/
/* color dialog functions.                                                  */
/* all colors are represented as an int ranging from 0 to 255.              */

typedef struct dia_color_cbs{
     int reason;              /* dia reason defines. */
     int red, green, blue;    
     int dred, dgreen, dblue; /* default red,green, blue */
     int pad;
}DiaColorCallbackStruct;

int dia_cbcolor(short red, short green, short blue, char *prompt,
		void (*cb)(Widget,XtPointer,XtPointer), void *client_data);
     

/****************************************************************************/
/* dialog creation fuction.                                                 */
#define DIA_MAX_BUTTONS 6
typedef struct dia_dialog_struct{

     Widget  dialog;
     Widget  workArea;
     Widget  controlArea;
     Widget  pane;
     Widget  topLevel;
     XtAppContext context;
     WidgetClass widgetClass;
     Visual *visual;
     XtPointer call_data;
     int    placement;
     int    popup;

     int    nbuttons;
     Widget button[DIA_MAX_BUTTONS];
     char   *buttonName[DIA_MAX_BUTTONS];
     void   (*func[DIA_MAX_BUTTONS])(Widget, XtPointer,XtPointer);
     XtPointer udata[DIA_MAX_BUTTONS];

     void   (*qfunc)(Widget,XtPointer,XtPointer);
     XtPointer  *qudata;

     void (*workAreaFunc)(Widget,XtPointer,XtPointer);
     XtPointer workAreaFuncData;

     Widget (*menuBarFunc)(Widget,XtPointer,XtPointer);
     XtPointer menuBarFuncData;

}diaDialog;

/* varg defines */
/* add control buttons to dialog. */
#define DiaNcontrolButton 1  /* char *name, void(*cb)(),  void *client_data  */

/* add function to window manager quit function. */
#define DiaNwindowQuit    2  /* void(*cb)(), void *client_data               */

/* use dia auto placement function. */
#define DiaNautoPlacement 3  /* True or False , default True                 */

/* dialog widget class */
#define DiaNwidgetClass   4  /* WidgetClass, default xmDialogShellWidgetClass*/

/* create a work area. */
#define DiaNworkAreaFunc  5  /* void(*cb)(), void *client_data               */

/* popup dialog automaticlly. */
#define DiaNpopup         6  /* True or False, default True                  */

/* callback function to create and return a menubar. */
#define DiaNmenuBarFunc   7  /*Widget(*cb)(), void *client_data */

Widget     diaGetWorkArea(diaDialog *dia);
void       diaDestroyDialog(diaDialog *dia);
XtPointer  diaGetCallData(diaDialog *dia);
void       diaPopup(diaDialog *dia);

/***************************** Function prototypes ***************************/

XtAppContext dia_get_context(void);
Widget       dia_get_toplevel(void);
Visual      *dia_getvisual(void);
int          dia_getdepth(void);
Colormap     dia_getcolormap(void);
char        *dia_get_title(void);
int          dia_init(int *argc, char **argv);
int          dia_xinit(Widget w, XtAppContext context, char *title);
void         dia_input(void);
void         dia_busy(int state);
void         diaBusyWindow(Widget w, int flag);
void         diaBusyCursor(int flag);
int          dia_mainloop(void);
void         diaWindowQuit(Widget window, 
			   XtCallbackProc quit_cb, XtPointer data);


diaDialog *diaVaCreateDialog(char *name, Widget topLevel, 
			     XtAppContext context, ...);
void diaEasyFileAct(char *name, void (*fcb)(Widget,  XtPointer, XtPointer),
		    XtPointer udata);


int  dia_color(int *red, int *green, int *blue);
void dia_setcolor(short red, short green, short blue, char *prompt,
		  void (*cb)(Widget,  XtPointer, XtPointer),
		  void *client_data);
int dia_sgicolor(short red, short green, short blue, char *prompt,
		 void (*cb)(Widget,  XtPointer, XtPointer), void *client_data);
int  dia_err(char *msg);
int  dia_abort(char *message, int cpid);
int  dia_wait(char *message, int cpid);
int  dia_ask(char *question);
int  dia_choice(char *question, char *lab1, char *lab2, char *lab3);
int  dia_puts(char *msg);
void dia_smsg( char **msg);
int  dia_abort(char *message, int cpid);
int  dia_wait(char *message, int cpid);
int  dia_fact(char *prompt, char *def1, char *def2,
	     void (*cb)(Widget,  XtPointer, XtPointer), void *client_data);
int  dia_fload(char *prompt, char *def1,
	      void (*cb)(Widget,  XtPointer, XtPointer), void *client_data);
char *dia_filename(char *reason);
char *dia_gets(char *string, char *prompt);
int  dia_int(int low, int high, int value, int decimal, char *prompt);
void dia_cint(int *val, int low, int high, int decimal, char *prompt);    
void dia_vasmsg(char *msg, ...);
FILE *dia_print(char *defaultValue);

/* menu creation functions. */
Widget diaMakeMenuItem(Widget parent, 
		       char *name, char mnemonic, char *accel,
		       XtCallbackProc cb, XtPointer data);
Widget diaGetMenuItem(Widget parent, char *name, char mnemonic,
	       XtCallbackProc cb, XtPointer data);
Widget diaGetCascadeMenu(Widget parent, char *name, char mnemonic, 
			 Widget *cascade);
Widget diaGetPulldownMenu(Widget parent, char *name, char mnemonic);

#ifdef __cplusplus
}
#endif
/*****************************************************************************/

#endif /* dia.h */
