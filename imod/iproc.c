/*  IMOD VERSION 2.50
 *
 *  iproc.c -- image processing for imod.
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

#include <Xm/Xm.h>
#include <Xm/ScrolledW.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/Scale.h>
#include <Xm/List.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>

#include <mrcc.h>
#include "diaP.h"
#include "imod.h"
#include "iproc.h"
#include "sliceproc.h"

static ImodIProc proc = {0};

static void edge_cb(Widget w, XtPointer client, XtPointer call);
static void mkedge_cb(Widget w, XtPointer client, XtPointer call);
static void thresh_cb(Widget w, XtPointer client, XtPointer call);
static void threshv_cb(Widget w, XtPointer client, XtPointer call);
static void mkthresh_cb(Widget w, XtPointer client, XtPointer call);
static void apply_cb(Widget w, XtPointer client, XtPointer call);
static void mkgen_cb(Widget w, XtPointer client, XtPointer call);
static void smooth_cb(Widget w, XtPointer client, XtPointer call);
static void sharpen_cb(Widget w, XtPointer client, XtPointer call);
static void grow_cb(Widget w, XtPointer client, XtPointer call);
static void shrink_cb(Widget w, XtPointer client, XtPointer call);


ImodIProcData proc_data[] = {
     {"edge", edge_cb, mkedge_cb, NULL, NULL},
     {"threshold", thresh_cb, mkthresh_cb, NULL, NULL},
     {"smooth", smooth_cb, mkgen_cb, (XtPointer)"Smooth Image:", NULL},
     {"sharpen", sharpen_cb, mkgen_cb, 
	   (XtPointer)"Sharpen Edges.", NULL},
     {"dilation", grow_cb, mkgen_cb, 
	   (XtPointer)"Grow Threshold Area.", NULL},
     {"erosion", shrink_cb, mkgen_cb, 
	   (XtPointer)"Shrink Threshold Area.", NULL},
     NULL,
};

static void edgetype_cb(Widget w, XtPointer client, XtPointer call)
{
     proc.edge = (int)client;
}
static void mkedge_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProcData *ipd = (ImodIProcData *)call;
     Widget rowcol, opt;
     XmString filter = XmStringCreateSimple("Filter");
     XmString sobel = XmStringCreateSimple("Sobel");
     XmString prew  = XmStringCreateSimple("Prewitt");
     XmString lapl  = XmStringCreateSimple("Laplacian");
     XmString grm   = XmStringCreateSimple("Graham");

     ipd->control =  XtVaCreateWidget
	  ("edge_control", xmFormWidgetClass, w, 
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,	   
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
     rowcol = XtVaCreateWidget
	  ("edge_rowcol",  xmRowColumnWidgetClass, ipd->control,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,	   
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
     XtVaCreateManagedWidget
	  ("Edge Enhancement Type:", xmLabelWidgetClass, rowcol, 
	   NULL);

     opt = XmVaCreateSimpleOptionMenu
	  (rowcol, "option_menu", 
	   filter, 'F', 0, edgetype_cb,
	   XmVaPUSHBUTTON, sobel, 'S', NULL, NULL,
	   XmVaPUSHBUTTON, prew, 'P', NULL, NULL,
	   XmVaPUSHBUTTON, lapl, 'L', NULL, NULL,
	   XmVaPUSHBUTTON, grm,  'G', NULL, NULL,
	   XmNvisual,   App->visual,
	   NULL);
     XtManageChild(opt);
     XmStringFree(filter);
     XmStringFree(sobel);
     XmStringFree(prew);
     XmStringFree(lapl);
     XmStringFree(grm);


     XtManageChild(rowcol);
     XtManageChild(ipd->control);
}

static void mkgen_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProcData *ipd = (ImodIProcData *)call;
     Widget rowcol;

     ipd->control =  XtVaCreateWidget
	  ("edge_control", xmFormWidgetClass, w, 
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,	   
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
     rowcol = XtVaCreateWidget
	  ("rowcol",  xmRowColumnWidgetClass, ipd->control,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,	   
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
     XtVaCreateManagedWidget
	  ((char *)client, xmLabelWidgetClass, rowcol, 
	   NULL);
     XtManageChild(rowcol);
     XtManageChild(ipd->control);
}



static void cpdslice(Islice *sl, ImodIProc *ip)
{
     register unsigned char *from, *to, *last;
     int rampbase = ip->vw->rampbase;
     from = sl->data.b;
     to = ivwGetZSectionTime(ip->vw, ip->idatasec, ip->idatatime);
     if (!to) return;
/*     to = ip->vw->idata[ip->idatasec]; */
     last = to + (ip->vw->xsize * ip->vw->ysize);
     if (App->depth > 8){
	  do{
	       *to++ = *from++;
	  }while (to !=  last);
     }else{
	  do{
	       *to++ = *from++ + rampbase;
	  }while (to !=  last);
     }
     sliceFree(sl);
}
static void edge_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)call;
     unsigned char *image = ivwGetCurrentZSection(ip->vw);
     Islice s;
     Islice *gs;
     int cz;

     if (!image) return;


     switch (ip->edge){
	case 0:
	  sliceInit(&s, ip->vw->xsize, ip->vw->ysize, 0, image);
	  sliceByteEdgeSobel(&s);
	  if(App->depth == 8)
	       sliceByteAdd(&s, ip->vw->rampbase);
	  break;

	case 1:
	  sliceInit(&s, ip->vw->xsize, ip->vw->ysize, 0, image);
	  sliceByteEdgePrewitt(&s);
	  if(App->depth == 8)
	       sliceByteAdd(&s, ip->vw->rampbase);
	  break;

	case 2:
	  sliceInit(&s, ip->vw->xsize, ip->vw->ysize, 0, image);
	  sliceByteEdgeLaplacian(&s);
	  break;
	  
	case 3:
	  sliceInit(&s, ip->vw->xsize, ip->vw->ysize, 0, image);
	  sliceByteGraham(&s);
	  break;

	case 5:
	  gs = sliceGradient(&s);
	  if (!gs) return;
	  cpdslice(gs, ip);
	  break;
	default:
	  break;
     }
     imodDraw(ip->vw, IMOD_DRAW_IMAGE);
     return;
}
static void smooth_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)call;
     Islice s;
     unsigned char *image = ivwGetCurrentZSection(ip->vw);
     if (!image) return;
     sliceInit(&s, ip->vw->xsize, ip->vw->ysize, 0, image);
     sliceByteSmooth(&s);
     imodDraw(ip->vw, IMOD_DRAW_IMAGE);
}
static void sharpen_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)call;
     Islice s;
     unsigned char *image = ivwGetCurrentZSection(ip->vw);
     if (!image) return;
     sliceInit(&s, ip->vw->xsize, ip->vw->ysize, 0, image);
     sliceByteSharpen(&s);
     imodDraw(ip->vw, IMOD_DRAW_IMAGE);
}

static void grow_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)call;
     Islice s;

     unsigned char *image = ivwGetCurrentZSection(ip->vw);
     if (!image) return;

     sliceInit(&s, ip->vw->xsize, ip->vw->ysize, 0, image);

     if (App->depth == 8){
	  s.min = ip->vw->rampbase;
	  s.max = ip->vw->rampsize + s.min;
     }else{
	  s.min = 0; s.max = 255;
     }

     sliceByteGrow(&s,  (int)s.max);
     imodDraw(ip->vw, IMOD_DRAW_IMAGE);
}
static void shrink_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)call;
     Islice s;
     unsigned char *image = ivwGetCurrentZSection(ip->vw);
     if (!image) return;

     sliceInit(&s, ip->vw->xsize, ip->vw->ysize, 0, image);
     if (App->depth == 8){
	  s.min = ip->vw->rampbase;
	  s.max = ip->vw->rampsize + s.min;
     }else{
	  s.min = 0; s.max = 255;
     }
     sliceByteShrink(&s,  (int)s.max);

     imodDraw(ip->vw, IMOD_DRAW_IMAGE);
}

static void mkthresh_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProcData *ipd = (ImodIProcData *)call;

     Widget rowcol, slider;

     ipd->control =  XtVaCreateWidget
	  ("thresh_control", xmFormWidgetClass, w, 
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,	   
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
     rowcol = XtVaCreateWidget
	  ("thresh_rowcol",  xmRowColumnWidgetClass, ipd->control,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,	   
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
     XtVaCreateManagedWidget
	  ("Threshold Filter Value", xmLabelWidgetClass, rowcol, 
	   NULL);
     slider = XtVaCreateManagedWidget
	  ("Threshold", xmScaleWidgetClass, rowcol,
	   XmNshowValue, True,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 254,
	   XmNvalue, proc.threshold,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(slider, XmNvalueChangedCallback, threshv_cb, &proc);
     XtAddCallback(slider, XmNdragCallback, threshv_cb, &proc);
     XtManageChild(rowcol);
     XtManageChild(ipd->control);
}

static void thresh_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)call;
     int cz, xysize, thresh, minv, maxv;
     unsigned char *idat, *last;
     
     thresh = ip->threshold;
     
     if (App->depth == 8){
	  thresh = (int)
	       ((((float)ip->vw->rampsize/256.0f)*thresh) + ip->vw->rampbase);
	  minv = ip->vw->rampbase;
	  maxv = ip->vw->rampsize + minv;
     }else{
	  minv = 0; maxv = 255;
     }

     xysize = ip->vw->xsize * ip->vw->ysize;
     idat = ivwGetCurrentZSection(ip->vw);
     if (!idat) return;
     for(last = idat + xysize; idat != last; idat++){
	  if (*idat > thresh)
	       *idat = maxv;
	  else
	       *idat = minv;
     }
     imodDraw(ip->vw, IMOD_DRAW_IMAGE);
     return;
}

static void threshv_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     ImodIProc *ip = (ImodIProc *)client;
     
     ip->threshold = cbs->value;
     return;
}

static void listproc_cb(Widget w, XtPointer client, XtPointer call)
{
     XmListCallbackStruct *cbs = (XmListCallbackStruct *)call;
     ImodIProc *ip = (ImodIProc *)client;
     ip->procnum = cbs->item_position - 1;
     

     if (ip->curcont)
	  XtUnmanageChild(ip->curcont);

     if (proc_data[proc.procnum].control){
	  XtManageChild(proc_data[proc.procnum].control);
	  XRaiseWindow(App->display, 
		       XtWindow(proc_data[proc.procnum].control));
     }
     ip->curcont = proc_data[proc.procnum].control;

     if (cbs->reason == XmCR_DEFAULT_ACTION)
	  apply_cb(w, client, (XtPointer)&proc);
}

static Widget mklist(Widget parent)
{
     Widget form, list, col;


     /*     XtManageChild(form);*/
     return(parent);
}

static void help_cb()
{
     dia_vasmsg
	  ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
	   "Imod Image Processing \n"
	   "~~~~~~~~~~~~~~~~~~~~~~~~"
	   "\n\n",
	   "Various kinds of simple filters can be applied with these "
	   "controls.  The filter will always be applied to the current "
	   "section.\n\n",
	   "Single-click in the list of filters to select the current filter "
	   "to be applied to the data; in some cases there will be further "
	   "parameters to select.\n\n"
	   "Selecting the [Apply] button will apply the current filter to the "
	   "ORIGINAL image data.  Double-clicking in the filter list is the "
	   "same as selecting the [Apply] button.\n\n"
	   "Selecting the [More] button will apply the filter to the CURRENT "
	   "image data, as modified by previous filter operations.\n\n"
	   "Selecting the [Reset] button, applying a filter to a different "
	   "section, closing the window with [Done], or flipping the data "
	   "volume will all restore the original image data for a section, "
	   "unless you select the [Save] button.  [Save] will permanently "
	   "replace the image data in memory with the processed data.\n\n",
	   NULL);
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     ImodIProc *ip = (ImodIProc *)client;
     Widget form, list;
     XmString str;
     Widget sw, row, col, button, slider;
     int i;

     form = XtVaCreateWidget
	  ("form", xmFormWidgetClass, w, NULL);

     list = XmCreateScrolledList(form, "proc_list", NULL, 0);
     for(i = 0; (proc_data[i].name); i++){
	  str = XmStringCreateSimple(proc_data[i].name);
	  XmListAddItem(list, str, i+1);
	  XmStringFree(str);
     }
     XtVaSetValues(list,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNtopAttachment, XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_FORM,
		   XmNvisibleItemCount, 5, 
		   XmNvalue, 1,
		   XmNselectionPolicy, XmSINGLE_SELECT,
		   NULL);
     XtAddCallback(list, XmNsingleSelectionCallback, listproc_cb, client);
     XtAddCallback(list, XmNdefaultActionCallback, listproc_cb, client); 
     XtManageChild(list);

     proc.frame = XtVaCreateManagedWidget
	  ("frame", xmFormWidgetClass, form,
	   XmNleftAttachment, XmATTACH_WIDGET,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNleftWidget, list,
	   NULL);

     /* make unmanaged widgets */
     for(i = 0; (proc_data[i].name); i++){
	  if (proc_data[i].mkwidget)
	       proc_data[i].mkwidget
		    (proc.frame, proc_data[i].client, &proc_data[i]);
	  if (proc_data[i].control){
	       XtSetMappedWhenManaged( proc_data[i].control, True);
	       XtUnmanageChild( proc_data[i].control);
	  }
     }
     if (proc_data[proc.procnum].control)
	  XtManageChild( proc_data[proc.procnum].control);
     ip->curcont = proc_data[proc.procnum].control;
/*
     if (proc_data[proc.procnum].control)
	  XRaiseWindow(App->display, 
		       XtWindow(proc_data[proc.procnum].control));
*/

     XtManageChild(form);

}

static void apply_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)client;
     diaDialog *dia = (diaDialog *)call;
     int cz =  (ip->vw->zmouse + 0.5f);

     /* Unconditionally restore data if modified */
     clearsec(ip);

     /* If this is a new section, save the data */
     if (cz != ip->idatasec || ip->vw->ct != ip->idatatime) {
          ip->idatasec = cz;
	  ip->idatatime = ip->vw->ct;
          savesec(ip);
     }

     /* Operate on the original data */
     if ( proc_data[ip->procnum].cb) {
          imod_info_float_clear(cz, ip->vw->ct);
	  proc_data[ip->procnum].cb(w, proc_data[ip->procnum].client, client);
	  ip->modified = 1;
     }
}
static void reapply_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)client;
     diaDialog *dia = (diaDialog *)call;
     int cz =  (ip->vw->zmouse + 0.5f);
 
     /* If this is not the same section, treat it as an Apply */
     if (cz != ip->idatasec || ip->vw->ct != ip->idatatime)
          apply_cb(w, client, call);

     /* Otherwise operate on the current data without restoring it */
     if ( proc_data[ip->procnum].cb) {
          imod_info_float_clear(cz, ip->vw->ct);
	  proc_data[ip->procnum].cb(w, proc_data[ip->procnum].client, client);
	  ip->modified = 1;
     }
}

static void save_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)client;
     diaDialog *dia = (diaDialog *)call;
     ip->modified = 0;
     ip->idatasec = -1;
}

static void clear_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)client;
     diaDialog *dia = (diaDialog *)call;
     clearsec(ip);
     imodDraw(ip->vw, IMOD_DRAW_IMAGE);
}

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodIProc *ip = (ImodIProc *)client;
     diaDialog *dia = (diaDialog *)call;
     
     diaDestroyDialog(dia);
     ip->dia = NULL;
     clearsec(ip);
     imodDraw(ip->vw, IMOD_DRAW_IMAGE);
     free(ip->idata);
     return;
}


int iprocRethink(struct ViewInfo *vw)
{
     if (proc.dia){
       if (proc.idata) {
	       clearsec(&proc);
	       proc.idatasec = -1;
	       free(proc.idata);
       }
       proc.idata = (unsigned char *)malloc(vw->xsize * vw->ysize);
     }
     return 0;
}

int inputIProcOpen(struct ViewInfo *vw)
{
     if (!proc.dia){
	  proc.vw = vw;
	  proc.idatasec = -1;
	  proc.idatatime = 0;
	  proc.modified = 0;
	  proc.idata = (unsigned char *)malloc(vw->xsize * vw->ysize);
	  proc.procnum = 0;

	  proc.threshold = 128;
	  proc.edge = 0;

	  if (!proc.idata)
	       return(-1);
	  proc.dia = diaVaCreateDialog
	       ("Imod: Image Processing",
		App->toplevel, App->context,
		 DiaNcontrolButton, "Apply",
		apply_cb, (XtPointer)&proc,
		 DiaNcontrolButton, "More",
		reapply_cb, (XtPointer)&proc,
		 DiaNcontrolButton, "Reset",
		clear_cb, (XtPointer)&proc,
		 DiaNcontrolButton, "Save",
		save_cb, (XtPointer)&proc,
		DiaNcontrolButton, "Done", 
		quit_cb, (XtPointer)&proc,
		DiaNcontrolButton, "Help", 
		help_cb, (XtPointer)&proc,
		DiaNworkAreaFunc, workarea_cb, (XtPointer)&proc,
		DiaNwindowQuit, quit_cb, (XtPointer)&proc,
		0);     
     }else{
	  XRaiseWindow(App->display, XtWindow(proc.dia->dialog));
     }
     return(0);
}

/* clear the section back to original data. */
static void clearsec(ImodIProc *ip)
{
     register unsigned char *from, *to, *last;
     
     if (ip->idatasec < 0 || !ip->modified)
	  return;

     from = ip->idata;
     to = ivwGetZSectionTime(ip->vw, ip->idatasec, ip->idatatime);
     if (!to) return;
     last = to + (ip->vw->xsize * ip->vw->ysize);
     do{
	  *to++ = *from++;
     }while (to != last);
     ip->modified = 0;
     imod_info_float_clear(ip->idatasec, ip->idatatime);
     return;
}

/* save the processing image to buffer. */
static void savesec(ImodIProc *ip)
{
     register unsigned char *from, *to, *last;
     
     if (ip->idatasec < 0)
	  return;


     to   = ip->idata;
     from = ivwGetZSectionTime(ip->vw, ip->idatasec, ip->idatatime);
     if (!from) return;
     last = to + (ip->vw->xsize * ip->vw->ysize);
     do{
	  *to++ = *from++;
     }while (to != last);
}
