/*  IMOD VERSION 2.20
 *
 *  imod_iscale.c -- Reload image with new scaling factors.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowB.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <dia.h>
#include "imod.h"

struct{
     int       init;
     diaDialog *dia;
     ImodView  *vw;
     float     min, max;
     Widget    wMin, wMax;
}imodImageScaleData = {0, 0, 0};

static Widget    fileWidget, mmmWidget;

#define BLACKNEW  32
#define WHITENEW  223

static Widget mkWorkArea(ImodView *vw, Widget top);

static void help_cb()
{
     dia_vasmsg
	  ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
	   "Imod Image Scale \n"
	   "~~~~~~~~~~~~~~~~~~~~~~~~"
	   "\n\n",
	   "The lower and upper limits of pixel values are used to scale",
	   "the image pixel values linearly to the 8-bit display.  ",
	   "When you first open the window, these values are set based upon"
	   " the current position of the contrast sliders, so that if the "
	   "image is reloaded, it will have the same apparent contrast with "
	   "the sliders set to 32 and 223.\n\n",
	   "The Apply button will reload the image with the given limits"
	   " AND set the contrast sliders to 32 and 223.\n\n",
	   "The Calc button will recalculate suggested lower and upper limits"
	   " based upon new settings of the contrast sliders.\n\n",
	   "Thus, you can either type in new lower and upper limits, or use"
	   " the sliders and Calc button to set the limits.\n",
	   NULL);
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     mkWorkArea(imodImageScaleData.vw, w);
     return;
}

static void done_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     diaDestroyDialog(dia);

     imodImageScaleData.dia = NULL;
     return;
}

void ivwShowStatus(char *string);

static void apply_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodView *vw = imodImageScaleData.vw;
     int black = BLACKNEW, white = WHITENEW;
     int k;
     struct MRCheader savehdr;

     char *st = NULL;
     
     st = XmTextGetString(imodImageScaleData.wMin);
     imodImageScaleData.min = atof(st);
     st = XmTextGetString(imodImageScaleData.wMax);
     imodImageScaleData.max = atof(st);

/* DNM: the min and max will take care of all the scaling needs, so no longer
  need the li-black and li->white to be anything but 0 and 255 */

     vw->black = black;
     vw->li->black = 0;
     vw->white = white;
     vw->li->white = 255;
     vw->li->smin = imodImageScaleData.min;
     vw->li->smax = imodImageScaleData.max;
     
     iiSetMM(vw->image, (double)vw->li->smin, (double)vw->li->smax);
     ivwSetScale(vw);

     if (vw->vmSize){
	  /* flush the image cache. */
	  ivwFlushCache(vw);
     }else{
	  /* flipped data will crash */
	  int reflip = 0;
	  if (vw->li->axis == 2){
	       ivwFlip(vw);
	       reflip = 1;
	  }
	  

	  if (vw->li->contig){
	       free(vw->idata[0]);
	  }else{
	       for (k = 0; k < vw->zsize; k++)
		    free(vw->idata[k]);
	  }
	  free(vw->idata);

/* DNM: got to reread header since it gets screwed up at end of read */
	  if (vw->image->file == IIFILE_MRC){
	      if (!vw->image->fp)
	         iiReopen(vw->image);
	      if (!vw->image->fp) return;
	      mrc_head_read(vw->image->fp, 
			    (struct MRCheader *)vw->image->header);
	    }
	  vw->idata = imod_io_image_load
	      (vw->image, vw->li, imod_imgcnt);

	  if (!vw->idata){
	       fprintf(stderr, "IMOD: Fatal Error. Image LOST!\n");
	       exit(-1);
	  }
	  
	  if (App->depth == 8)
	       ivwScale(vw);
	  if (reflip)
	       ivwFlip(vw);
     }

     /* DNM: clear any information for floating windows for this time */
     imod_info_float_clear(-vw->zsize, vw->ct);
     imod_info_setbw(black, white);     
     xcramp_setlevels(vw->cramp,black,white);

     imodDraw(vw, IMOD_DRAW_IMAGE);
     return;
}

/* Computes default scale limits from current scale and slider settings */

static void ComputeScale(ImodView *vw)
{
     float slidecur, rangecur, slidenew, rangenew;
     slidecur = vw->white - vw->black;
     rangecur = vw->li->smax - vw->li->smin;
     slidenew = WHITENEW - BLACKNEW;
     rangenew = slidecur * rangecur / slidenew;
     imodImageScaleData.min = (slidenew * rangenew / 255.0f) *
          ( (255.0f * vw->li->smin / (slidecur * rangecur)) +
	   vw->black/slidecur - BLACKNEW/slidenew );
     imodImageScaleData.max = imodImageScaleData.min + rangenew;
}

static void compute_cb(Widget w, XtPointer client, XtPointer call)
{
     char label[32];
     ComputeScale(imodImageScaleData.vw);
     sprintf(label, "%g", imodImageScaleData.min);
     XtVaSetValues(imodImageScaleData.wMin, XmNvalue, label, NULL);
     sprintf(label, "%g", imodImageScaleData.max);
     XtVaSetValues(imodImageScaleData.wMax, XmNvalue, label, NULL);
}

void imodImageScaleDialog(ImodView *vw)
{
     XtPointer cbd = (XtPointer)vw;
     
     if (imodImageScaleData.dia){
	  XRaiseWindow(App->display, 
		       XtWindow(imodImageScaleData.dia->dialog));
	  return;
     }

     imodImageScaleData.vw = vw;
     if ((!vw->li->smin) && (!vw->li->smax)){
	  vw->li->smin = vw->hdr->amin;
	  vw->li->smax = vw->hdr->amax;
     }

     ComputeScale(vw);

     imodImageScaleData.dia = diaVaCreateDialog
	  ("Imod: Image Scale", App->toplevel, App->context,
	   DiaNcontrolButton, "Done", done_cb,   cbd,
	   DiaNcontrolButton, "Apply", apply_cb, cbd,
	   DiaNcontrolButton, "Calc", compute_cb, cbd,
	   DiaNcontrolButton, "Help", help_cb,   cbd,
	   DiaNworkAreaFunc,  workarea_cb,       cbd,
	   DiaNwindowQuit,    done_cb,           cbd,
	   0);
     return;
}

/****************************************************************************/
/*  Dialog controls.                                                 */

static void show_file_and_mmm(ImodView *vw)
{
     char label[120];
     XmString xstring;

     sprintf(label, "File: %s", vw->image->filename);
     xstring = XmStringCreateSimple(label);
     XtVaSetValues(fileWidget, XmNlabelString, xstring, NULL);
     XmStringFree(xstring);

     sprintf(label, "Min: %g     Max: %g     Mean: %g",  vw->hdr->amin,
	     vw->image->amax, vw->image->amean);
     xstring = XmStringCreateSimple(label);
     XtVaSetValues(mmmWidget, XmNlabelString, xstring, NULL);
     XmStringFree(xstring);
}

static Widget mkWorkArea(ImodView *vw, Widget top)
{
     Widget frame, col;

     frame = XtVaCreateWidget
	  ("frame", xmFrameWidgetClass, top, NULL);

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, frame,
	   NULL);
     {
	  Widget row;
	  char label[32];
	  label[0] = 0x00;

	  fileWidget = XtVaCreateManagedWidget
	       (label, xmLabelWidgetClass, col, NULL);

	  mmmWidget = XtVaCreateManagedWidget
	       (label, xmLabelWidgetClass, col, NULL);

	  show_file_and_mmm(vw);

	  XtVaCreateManagedWidget
	       ("sep", xmSeparatorWidgetClass, col, NULL);
	  XtVaCreateManagedWidget
	       ("Limits for Linear Scaling:", xmLabelWidgetClass, col, NULL);

	  row = XtVaCreateWidget
	       ("rowcol", xmRowColumnWidgetClass, col,
		XmNorientation, XmHORIZONTAL,
		NULL);
	  XtVaCreateManagedWidget
	       ("Lower Limit", xmLabelWidgetClass, row, NULL);
	  sprintf(label, "%g", imodImageScaleData.min);
	  imodImageScaleData.wMin = XtVaCreateManagedWidget
	       ("MinScale", xmTextWidgetClass, row,
		XmNcolumns, 12,
		XmNvalue, label,
		NULL);
	  XtAddCallback(imodImageScaleData.wMin, XmNactivateCallback,
			apply_cb, (XtPointer)imodImageScaleData.vw);
	  XtManageChild(row);

	  row = XtVaCreateWidget
	       ("rowcol", xmRowColumnWidgetClass, col,
		XmNorientation, XmHORIZONTAL,
		NULL);
	  XtVaCreateManagedWidget
	       ("Upper Limit", xmLabelWidgetClass, row, NULL);
	  sprintf(label, "%g", imodImageScaleData.max);
	  imodImageScaleData.wMax = XtVaCreateManagedWidget
	       ("MaxScale", xmTextWidgetClass, row,
		XmNcolumns, 12,
		XmNvalue, label,
		NULL);
	  XtAddCallback(imodImageScaleData.wMax, XmNactivateCallback,
			apply_cb, (XtPointer)imodImageScaleData.vw);
	  XtManageChild(row);
	  
     }
     XtManageChild(col);
     XtManageChild(frame);
     return(frame);
}     

void imodImageScaleUpdate(ImodView *vw)
{
     if (imodImageScaleData.dia)
	  show_file_and_mmm(vw);
}
