/*  IMOD VERSION 2.42
 *
 *  pixelview.c -- view numerical values of pixels in an image.
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

/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#include <stdlib.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include "diaP.h"
#include "imod.h"
#include "mrcfiles.h"

#define PV_ROWS 8
#define PV_COLS 8


Widget PixelViewGrid[PV_ROWS][PV_COLS];
Widget PixelViewDialog = 0;
static int ctrl;

static void pview_quit_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodView *vi = (ImodView *)client;
     ivwDeleteControl(vi, ctrl);
}

static void pviewClose_cb(ImodView *vi, void *client, long drawflag)
{
     XtPopdown(PixelViewDialog);
     XtDestroyWidget(PixelViewDialog);
     PixelViewDialog = 0;
}

static void pviewDraw_cb(ImodView *vi, void *client, long drawflag)
{
     if (PixelViewDialog && (drawflag & IMOD_DRAW_XYZ))
	  set_pixelview(vi);
}


int set_pixelview(struct ViewInfo *vi)
{
     int i, j, x, y;
     float pixal;
     char buf[32];
     XmString st;

     if (!PixelViewDialog)
	  return(-1);

     /* DNM 11/24/02: Bring window to the top */
     XRaiseWindow(App->display, XtWindow(PixelViewDialog));

     for (i = 0; i < PV_COLS; i++)
	  for(j = 0; j < PV_ROWS; j++){
	       /* DNM: take floor to avoid duplicating 1 at 0 */
	       x = floor((double)vi->xmouse) + i - (PV_COLS/2);
	       y = floor((double)vi->ymouse) + j - (PV_ROWS/2);
	       if ( (i) && (j)){
		    if ((x < 0) || (y < 0) || (x >= vi->xsize) 
			|| (y >= vi->ysize))
			 sprintf(buf, "         x");
		    else{
			 pixal = ivwGetFileValue(vi, x, y, vi->zmouse);
			 sprintf(buf, "%9g ", pixal);
		    }
		    st = XmStringCreateSimple(buf);
	       }else{
		    if ((!i) && (!j)){
			 sprintf(buf, " Y / X ");
		    }else
		    if (!i){
			 sprintf(buf, " %6d ", y+1);
			 if (j == (PV_COLS / 2))
			      sprintf(buf, " %6d*", y+1);
		    }else {
			 sprintf(buf, " %6d ", x+1);
			 if (i == (PV_ROWS / 2))
			     sprintf(buf, " %6d*", x+1);
		    }

		    st = XmStringCreateSimple(buf);
	       }
	       XtVaSetValues(PixelViewGrid[j][i],
			     XmNlabelString, st, NULL);
	       XmStringFree(st);
	       
	  }
     return(0);
}

void pixelview_cb(Widget w, XtPointer client, XtPointer call)
{
     int pos = (int)client;
     int x,y;

     ivwControlPriority(App->cvi, ctrl);

     y = pos / PV_COLS - PV_COLS / 2;
     x = pos % PV_COLS - PV_ROWS / 2;
     y += 1;
     App->cvi->xmouse += x;
     App->cvi->ymouse -= y;
     ivwBindMouse(App->cvi);
     /*   set_pixelview(App->cvi); */
     imodDraw(App->cvi, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
}

int open_pixelview(struct ViewInfo *vi)
{

     static Widget row, col;
     char *pixtitle = "Pixel View";
     int i, j, x, y;
     float pixal;
     char buf[32];

     Atom wmclose;

     if (PixelViewDialog){
	  set_pixelview(vi);
	  return(-1);
     }

     PixelViewDialog = XtVaCreatePopupShell
	  ("PixelView", topLevelShellWidgetClass, App->toplevel,
	   XtNtitle, pixtitle,
	   XmNvisual, App->visual,
	   NULL);

     row = XtVaCreateManagedWidget
	  ("rowcolumn", xmRowColumnWidgetClass, PixelViewDialog,
	   XmNpacking, XmPACK_COLUMN,
	   XmNnumColumns, PV_COLS,
	   XmNorientation, XmVERTICAL,
	   NULL);

     for (i = 0; i < PV_COLS; i++)
	  for(j = 0; j < PV_ROWS; j++){
	       sprintf(buf, "         x");
	       if ((i) && (j<(PV_ROWS-1))){
		    PixelViewGrid[PV_ROWS - j - 1][i] = XtVaCreateManagedWidget
			 (buf, xmPushButtonWidgetClass, row, NULL);
		    XtAddCallback( PixelViewGrid[PV_ROWS - j - 1][i],
				  XmNactivateCallback, pixelview_cb,
				  (XtPointer)((j * PV_COLS) + i));
	       }else{
		    PixelViewGrid[PV_ROWS - j - 1][i] = XtVaCreateManagedWidget
			 (buf, xmLabelWidgetClass, row, NULL);
	       }
	  }


     
     wmclose = XmInternAtom( XtDisplay(PixelViewDialog),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(PixelViewDialog, wmclose, pview_quit_cb,
			     (caddr_t)vi);

     ctrl = ivwNewControl(vi, pviewDraw_cb, pviewClose_cb, (XtPointer)0);

     XtPopup(PixelViewDialog, XtGrabNone);
     set_pixelview(vi);
     return(0);
}
