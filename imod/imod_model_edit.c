/*  IMOD VERSION 2.40
 *
 *  imod_model_edit.c -- model edit dialog functions.
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

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <stdio.h>
#include <math.h>

#include "diaP.h"
#include "imod.h"

static struct
{
     diaDialog     *dia;
     ImodView      *vw;        /* image data to model                       */

     /* Widgets used in model edit. */
     Widget wDraw;
     Widget wZscale;
     Widget wPixelSize;
     Widget wResolution;
     Widget wTimeCopy;

}ThisDialog = { NULL, 0 };

static void workarea_cb(Widget w, XtPointer client, XtPointer call);
static void setvw(void);

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     setvw();
     diaDestroyDialog(ThisDialog.dia);
     ThisDialog.dia = NULL;
     return;
}

int openModelEdit(ImodView *vw)
{
     
     if (ThisDialog.dia){
	  XRaiseWindow(XtDisplay(ThisDialog.dia->dialog), 
		       XtWindow(ThisDialog.dia->dialog));
	  return(0);
     }
     
     ThisDialog.vw = vw;
     ThisDialog.dia = diaVaCreateDialog
	  ("Imod: Model Edit",
	   App->toplevel, App->context,
	   DiaNcontrolButton, "Done",
	   quit_cb, (XtPointer)&ThisDialog,
	   DiaNworkAreaFunc, workarea_cb, (XtPointer)&ThisDialog,
	   DiaNwindowQuit, quit_cb, (XtPointer)&ThisDialog,
	   0);     
     
     return 0;
}

/****************************************************************************/

static void setwidgets(void)
{
     char string[32];

     XmToggleButtonSetState
	  (ThisDialog.wDraw,
	   (ThisDialog.vw->imod->drawmode > 0) ? True : False,
	   False);


     sprintf(string, "%g", ThisDialog.vw->imod->zscale);
     XmTextSetString(ThisDialog.wZscale, string);

     sprintf(string, "%d", ThisDialog.vw->imod->res);
     XmTextSetString(ThisDialog.wResolution, string);
	
     switch(ThisDialog.vw->imod->units){

	case IMOD_UNIT_KILO:
	  sprintf(string, "%g km", ThisDialog.vw->imod->pixsize);
	  break;
	case IMOD_UNIT_METER:
	  sprintf(string, "%g m", ThisDialog.vw->imod->pixsize);
	  break;
	case IMOD_UNIT_CM:
	  sprintf(string, "%g cm", ThisDialog.vw->imod->pixsize);
	  break;
	case IMOD_UNIT_MM:
	  sprintf(string, "%g mm", ThisDialog.vw->imod->pixsize);
	  break;
	case IMOD_UNIT_UM:
	  sprintf(string, "%g um", ThisDialog.vw->imod->pixsize);
	  break;
	case IMOD_UNIT_NM:
	  sprintf(string, "%g nm", ThisDialog.vw->imod->pixsize);
	  break;
	case IMOD_UNIT_ANGSTROM:
	  sprintf(string, "%g A", ThisDialog.vw->imod->pixsize);
	  break;
	case IMOD_UNIT_PM:
	  sprintf(string, "%g pm", ThisDialog.vw->imod->pixsize);
	  break;
	default:
	  sprintf(string, "%g", ThisDialog.vw->imod->pixsize);
	  break;
     }
     XmTextSetString(ThisDialog.wPixelSize, string);



}

static void setvw(void)
{
     char *string;
     float fscale;

     string = XmTextGetString(ThisDialog.wZscale);
     ThisDialog.vw->imod->zscale = atof(string);
     free(string);

     string = XmTextGetString(ThisDialog.wResolution);
     ThisDialog.vw->imod->res = atoi(string);
     free(string);

     string = XmTextGetString(ThisDialog.wPixelSize);
     /* DNM: protect sscanf from empty strings for PC */
     fscale = 0.;
     if (string && string[0] != 0x00)
	  sscanf(string, "%f", &fscale);
     if (fscale != 0)
	  ThisDialog.vw->imod->pixsize = fscale;
     if (strstr(string, "km"))
	  ThisDialog.vw->imod->units = IMOD_UNIT_KILO;
     if (strstr(string, "m"))
	  ThisDialog.vw->imod->units = IMOD_UNIT_METER;
     if (strstr(string, "cm"))
	  ThisDialog.vw->imod->units = IMOD_UNIT_CM;
     if (strstr(string, "mm"))
	  ThisDialog.vw->imod->units = IMOD_UNIT_MM;
     if (strstr(string, "um"))
	  ThisDialog.vw->imod->units = IMOD_UNIT_UM;
     if (strstr(string, "nm"))
	  ThisDialog.vw->imod->units = IMOD_UNIT_NM;
     if (strstr(string, "A"))
	  ThisDialog.vw->imod->units = IMOD_UNIT_ANGSTROM;
     if (strstr(string, "pm"))
	  ThisDialog.vw->imod->units = IMOD_UNIT_PM;
     free(string);

}

static void setvw_cb(Widget w, XtPointer client, XtPointer call)
{
     setvw();
}

static void modeldraw_cb(Widget w, XtPointer client, XtPointer call)
{
     ThisDialog.vw->imod->drawmode -= (2 * ThisDialog.vw->imod->drawmode);
     setwidgets();
     imodDraw(ThisDialog.vw, IMOD_DRAW_MOD);
}

static void timecopy_cb(Widget w, XtPointer client, XtPointer call)
{
     Imod *imod         = ThisDialog.vw->imod;
     int currentObject  = imod->cindex.object;
     int currentContour = imod->cindex.contour;
     int currentPoint   = imod->cindex.point;
     int currentTime    = ThisDialog.vw->ct;
     int nextTime       = currentTime + 1;
     int whichTime      = (int)client;
     char *string;
     Iobj  *obj;
     Icont *cont, *dupcont;
     int ob, co;
     
     if (whichTime){
	  string = XmTextGetString(ThisDialog.wTimeCopy);
	  nextTime = atoi(string);
	  free(string);
     }

     if (nextTime > ThisDialog.vw->nt){
	  wprint("Warning: Copy failed.\n"
		 "\tNext time point is invalid.\n");
	  return;
     }

     for (ob = 0; ob < imod->objsize; ob++){
	  obj = &imod->obj[ob];
	  if (!iobjFlagTime(obj)) continue;
	  imod->cindex.object = ob;
	  for(co = 0; co < obj->contsize; co++){
	       cont = &obj->cont[co];
	       if (cont->type != currentTime)
		    continue;
	       dupcont = imodContourDup(cont);
	       dupcont->type = nextTime;
	       NewContour(imod);
	       cont  = imodContourGet(imod);
	       *cont = *dupcont;
	       free(dupcont);
	  }
     }

     imod->cindex.object  = currentObject;
     imod->cindex.contour = currentContour;
     imod->cindex.point   = currentPoint;
     imod_info_setocp();
     wprint("Time copy completed.\n");
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     Widget rowcol, grid;

     rowcol = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, w, NULL);

     ThisDialog.wDraw = XtVaCreateManagedWidget
	  ("Draw Model",  xmToggleButtonWidgetClass, rowcol, NULL);
     XtAddCallback(ThisDialog.wDraw, XmNvalueChangedCallback, 
		   modeldraw_cb, NULL);

     grid = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, rowcol, 
	   XmNpacking, XmPACK_COLUMN,
	   XmNnumColumns, 2,
	   XmNorientation, XmVERTICAL,
	   NULL);

     XtVaCreateManagedWidget
	  ("Z-Scale", xmLabelWidgetClass, grid, NULL);
     XtVaCreateManagedWidget
	  ("Resolution", xmLabelWidgetClass, grid, NULL);
     XtVaCreateManagedWidget
	  ("Pixel Size", xmLabelWidgetClass, grid, NULL);

     ThisDialog.wZscale = XtVaCreateManagedWidget
	  ("Zscale", xmTextWidgetClass, grid,  NULL);
     XtAddCallback(ThisDialog.wZscale, XmNactivateCallback, 
		   setvw_cb, NULL);
     ThisDialog.wResolution = XtVaCreateManagedWidget
	  ("Resolution", xmTextWidgetClass, grid,  NULL);
     XtAddCallback(ThisDialog.wResolution, XmNactivateCallback, 
		   setvw_cb, NULL);
     ThisDialog.wPixelSize = XtVaCreateManagedWidget
	  ("pixsize", xmTextWidgetClass, grid,  NULL);
     XtAddCallback(ThisDialog.wPixelSize, XmNactivateCallback, 
		   setvw_cb, NULL);

     XtManageChild(grid);

     /* Add Time controls. */
     if (ThisDialog.vw->nt){
	  Widget row, button;

	  XtVaCreateManagedWidget
	       ("separator", xmSeparatorWidgetClass, rowcol, NULL);

	  button = XtVaCreateManagedWidget
	  ("Copy Time Data to Next Index", 
	   xmPushButtonWidgetClass, rowcol,  NULL);
	  XtAddCallback(button, XmNactivateCallback, 
			timecopy_cb, 0);
	  
	  row = XtVaCreateWidget
	       ("rowcol", xmRowColumnWidgetClass, rowcol,
		XmNorientation, XmHORIZONTAL,
		NULL);
	  button = XtVaCreateManagedWidget
	       ("Copy Time Data to ", xmPushButtonWidgetClass, row, NULL);
	  XtAddCallback(button, XmNactivateCallback, 
			timecopy_cb, (XtPointer)1);

	  ThisDialog.wTimeCopy = XtVaCreateManagedWidget
	       ("label", xmTextWidgetClass, row, NULL);

	  XtManageChild(row);
     }
     
     XtManageChild(rowcol);
     setwidgets();
     return;
}

/****************************************************************************/


static struct
{
     diaDialog     *dia;
     ImodView      *vw;        /* image data to model                       */
     Ipoint        log;

     /* Widgets used in offset dialog. */
     Widget wXoffset;
     Widget wYoffset;
     Widget wZoffset;


}OffsetDialog = { NULL, 0 };

static void offworkarea_cb(Widget w, XtPointer client, XtPointer call);

static void offquit_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDestroyDialog(OffsetDialog.dia);
     OffsetDialog.dia = NULL;
     return;
}

void imodTransXYZ(Imod *imod, Ipoint trans)
{
     int ob, co, pt;
     Iobj *obj;
     Icont *cont;
     
     for(ob = 0; ob < imod->objsize; ob++){
	  obj = &(imod->obj[ob]);
	  for(co = 0; co < obj->contsize; co++){
	       cont = &(obj->cont[co]);
	       for(pt = 0; pt < cont->psize; pt++){
		    cont->pts[pt].x += trans.x;
		    cont->pts[pt].y += trans.y;
		    cont->pts[pt].z += trans.z;
	       }
	  }
     }
}

static void offapply_cb(Widget w, XtPointer client, XtPointer call)
{
     char *string;
     Ipoint offset;

     string = XmTextGetString(OffsetDialog.wXoffset);
     offset.x = atof(string);
     free(string);
     string = XmTextGetString(OffsetDialog.wYoffset);
     offset.y = atof(string);
     free(string);
     string = XmTextGetString(OffsetDialog.wZoffset);
     offset.z = atof(string);
     free(string);

     imodTransXYZ(OffsetDialog.vw->imod, offset);
     OffsetDialog.log.x += offset.x;
     OffsetDialog.log.y += offset.y;
     OffsetDialog.log.z += offset.z;
     imodDraw(OffsetDialog.vw, IMOD_DRAW_MOD);
}

static void offrevert_cb(Widget w, XtPointer client, XtPointer call)
{
     Ipoint offset;

     offset.x = -OffsetDialog.log.x;
     offset.y = -OffsetDialog.log.y;
     offset.z = -OffsetDialog.log.z;

     imodTransXYZ(OffsetDialog.vw->imod, offset);
     OffsetDialog.log.x += offset.x;
     OffsetDialog.log.y += offset.y;
     OffsetDialog.log.z += offset.z;
     imodDraw(OffsetDialog.vw, IMOD_DRAW_MOD);
}

int openModelOffset(ImodView *vw)
{
     
     if (OffsetDialog.dia){
	  XRaiseWindow(XtDisplay(OffsetDialog.dia->dialog), 
		       XtWindow(OffsetDialog.dia->dialog));
	  return(0);
     }
     
     OffsetDialog.vw = vw;
     OffsetDialog.dia = diaVaCreateDialog
	  ("Imod: Model Offset",
	   App->toplevel, App->context,
	   DiaNcontrolButton, "Apply", offapply_cb, NULL,
	   DiaNcontrolButton, "Revert", offrevert_cb, NULL,
	   DiaNcontrolButton, "Done",  offquit_cb, NULL,
	   DiaNworkAreaFunc, offworkarea_cb, NULL,
	   DiaNwindowQuit, offquit_cb, NULL,
	   0);     
     return 0;
}

static void offworkarea_cb(Widget w, XtPointer client, XtPointer call)
{
     Widget grid;

     grid = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, w,
	   XmNpacking, XmPACK_COLUMN,
	   XmNnumColumns, 2,
	   XmNorientation, XmVERTICAL,
	   NULL);
     
     XtVaCreateManagedWidget
	  ("X Coordinate Offset", xmLabelWidgetClass, grid, NULL);
     XtVaCreateManagedWidget
	  ("Y Coordinate Offset", xmLabelWidgetClass, grid, NULL);
     XtVaCreateManagedWidget
	  ("Z Coordinate Offset", xmLabelWidgetClass, grid, NULL);
     
     OffsetDialog.wXoffset = XtVaCreateManagedWidget
	  ("textbox", xmTextWidgetClass, grid,  NULL);
     OffsetDialog.wYoffset = XtVaCreateManagedWidget
	  ("textbox", xmTextWidgetClass, grid,  NULL);
     OffsetDialog.wZoffset = XtVaCreateManagedWidget
	  ("textbox", xmTextWidgetClass, grid,  NULL);

     XtManageChild(grid);
}
