#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <Xm/PushB.h>

#include <stdio.h>
#include "diaP.h"


Widget diaMakeMenuItem(Widget parent, 
		       char *name, char mnemonic, char *accel,
		       XtCallbackProc cb, XtPointer data)
{
    char accel_label[32];
    XmString accel_text;
    Widget item;

    sprintf(accel_label, "Ctrl-%c", accel[strlen(accel)-1]);
    accel_text = XmStringCreateSimple(accel_label);
    item = XtVaCreateManagedWidget
        (name, xmPushButtonWidgetClass, parent,
	 XmNaccelerator,     accel,
	 XmNacceleratorText, accel_text,
	 XmNdepth,    dia_getdepth(),
         XmNvisual,   dia_getvisual(),
         XmNcolormap, dia_getcolormap(),
         NULL);
    if (mnemonic)
        XtVaSetValues (item, XmNmnemonic, mnemonic, NULL);
    XtAddCallback(item, XmNactivateCallback, cb, data);
    XmStringFree(accel_text);
    return(item);
}


Widget diaGetMenuItem(Widget parent, char *name, char mnemonic,
	       XtCallbackProc cb, XtPointer data)
{
    Widget item = XtVaCreateManagedWidget
	(name, xmPushButtonWidgetClass, parent,
	 XmNdepth,    dia_getdepth(),
	 XmNvisual,   dia_getvisual(),
	 XmNcolormap, dia_getcolormap(),
	 NULL);
    if (mnemonic)
	XtVaSetValues (item, XmNmnemonic, mnemonic, NULL);
    XtAddCallback(item, XmNactivateCallback, cb, data);

    return(item);
}


Widget diaGetCascadeMenu(Widget parent, char *name, char mnemonic, 
			 Widget *cascade)
{

    Arg args[4];
    int n = 0;
    Widget pullDown;
    XmString str;
    
    XtSetArg(args[n], XmNdepth,    dia_getdepth()); n++;
    XtSetArg(args[n], XmNvisual,   dia_getvisual()); n++;
    XtSetArg(args[n], XmNcolormap, dia_getcolormap());n++;
    
    pullDown = XmCreatePulldownMenu
	(parent, "pulldown", args, n);
    str = XmStringCreateSimple(name);
    
    *cascade = XtVaCreateManagedWidget
	("cascade", xmCascadeButtonWidgetClass, parent,
	 XmNsubMenuId, pullDown,
	 XmNlabelString, str,
	 XmNmnemonic, mnemonic,
	 XmNdepth,    dia_getdepth(),
	 XmNvisual,   dia_getvisual(),
	 XmNcolormap, dia_getcolormap(),
	 NULL);

    XmStringFree(str);
    return(pullDown);
     
}

Widget diaGetPulldownMenu(Widget parent, char *name, char mnemonic)
{
    Widget cascade;
    return(diaGetCascadeMenu(parent, name, mnemonic, &cascade));
}
