/*  IMOD VERSION 2.50
 *
 *  imod_info_cb.c -- Callback functions for the Information Window.
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
    Revision 3.1  2002/01/29 03:10:00  mast
    Call imodDraw instead of xyz_draw after changing model/movie mode

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <Xm/Xm.h>
#include <Xm/ToggleB.h>

#include <dia.h>
#include "imod.h"
#include "imod_info.h"
#include "imodel.h"
#include "mrcfiles.h"
#include "options.h"
#include "xzap.h"

int sampleMeanSD(unsigned char *image, int type, int nx, int ny, float sample, 
		 float matt, float *mean, float *sd);

int Imod_obj_cnum = -1;

/* DNM 2/12/01: let the object and contour buttons cycle around at either 
   end */
void imod_nextobj_cb(Widget w, XtPointer client, XtPointer call)
{
     Imod *imod = App->cvi->imod;
     /* if there are at least two objects and we're at the end, set to 1 and
	use inputPrevObject to get to 0 */
     if ((imod->cindex.object == imod->objsize - 1) && imod->objsize > 1) {
	  imod->cindex.object = 1;
	  inputPrevObject(App->cvi);
     } else
	  inputNextObject(App->cvi);
     imod_setxyzmouse();
     return;
}

void imod_prevobj_cb(Widget w, XtPointer client, XtPointer call)
{
     Imod *imod = App->cvi->imod;
     /* If there are at least two objects and we're at the first one, set to 
	the next to last one and us inputNextObject to get to the last one */
     if (!imod->cindex.object && imod->objsize > 1) {
	  imod->cindex.object = imod->objsize - 2;
	  inputNextObject(App->cvi);
     } else
	  inputPrevObject(App->cvi);
     imod_setxyzmouse();
     return;
}

void imod_nextcont_cb(Widget w, XtPointer client, XtPointer call)
{
     Imod *imod = App->cvi->imod;
     Iobj *obj = imodObjectGet(imod);
     if (obj) {
	  if ((imod->cindex.contour == obj->contsize - 1) && 
	      obj->contsize > 1) {
	       imod->cindex.contour = 1;
	       inputPrevContour(App->cvi);
	       imod_setxyzmouse(); 
	       return;
	  }
     }
     inputNextContour(App->cvi);
     imod_setxyzmouse(); 
     return;
}

void imod_prevcont_cb(Widget w, XtPointer client, XtPointer call)
{
     Imod *imod = App->cvi->imod;
     Iobj *obj = imodObjectGet(imod);
     if (obj) {
	  if (!imod->cindex.contour && obj->contsize > 1) {
	       imod->cindex.contour = obj->contsize - 2;
	       inputNextContour(App->cvi);
	       imod_setxyzmouse(); 
	       return;
	  }
     }
     inputPrevContour(App->cvi);
     imod_setxyzmouse(); 
     return;
}

void imod_nextpoint_cb(Widget w, XtPointer client, XtPointer call)
{
     Imod *imod = App->cvi->imod;
     Icont *cont = imodContourGet(imod);
     if (cont) {
	  if ((imod->cindex.point == cont->psize - 1) && cont->psize > 1) {
	       imod->cindex.point = 1;
	       PrevPoint(Model);
	       imod_setxyzmouse(); 
	       return;
	  }
     }
     NextPoint(Model);
     imod_setxyzmouse(); 
     return;
}

void imod_prevpoint_cb(Widget w, XtPointer client, XtPointer call)
{
     Imod *imod = App->cvi->imod;
     Icont *cont = imodContourGet(imod);
     if (cont) {
	  if (!imod->cindex.point && cont->psize > 1) {
	       imod->cindex.point = cont->psize - 2;
	       NextPoint(Model);
	       imod_setxyzmouse(); 
	       return;
	  }
     }
     PrevPoint(Model);
     imod_setxyzmouse();
}

void imod_nextx_cb(Widget w, XtPointer client, XtPointer call)
{
     inputNextx(App->cvi);
     return;
}

void imod_prevx_cb(Widget w, XtPointer client, XtPointer call)
{
     inputPrevx(App->cvi);
     return;
} 

void imod_nexty_cb(Widget w, XtPointer client, XtPointer call)
{
     inputNexty(App->cvi);
     return;
}

void imod_prevy_cb(Widget w, XtPointer client, XtPointer call)
{
     inputPrevy(App->cvi);
     return;
} 

void imod_nextz_cb(Widget w, XtPointer client, XtPointer call)
{
     inputNextz(App->cvi);
     return;
}

void imod_prevz_cb(Widget w, XtPointer client, XtPointer call)
{
     inputPrevz(App->cvi);
     return;
} 

void imod_obj_select_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     printf("imod_obj_select_cb: %d selected\n", cbs->value);
     return;
}

/* DNM: make this function to simplify updating slider numbers from several 
   places */
static void display_bwslider_value(Widget w, int white)
{
     char val[8];
     XmString str;
     sprintf(val, "%03d", white);
     str = XmStringCreateSimple(val);
     XtVaSetValues(w, XmNlabelString, str, NULL);
     XmStringFree(str);
     return;
}

static int float_on = 0;

void imod_blacklevel_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     int white, black;
     int float_save = float_on;

     if (ImodForbidLevel)
	  return;

     black = cbs->value;
     white = App->cvi->white;

     /* DNM: If TrueColor and slider is being dragged, just display number */
     if (App->rgba && (cbs->reason == XmCR_DRAG)) {
	  display_bwslider_value(Imod_widget_blackval, black);
	  return;
     }

     if (black == 255)
	  black = 254;
     if (black > white)
	  white = black + 1;

     xcramp_setlevels(App->cvi->cramp,black,white);
     App->cvi->black = black;
     App->cvi->white = white;

     /* Set the float flag to false to prevent this change from being 
	undone in a redraw */
     float_on = FALSE;
     imod_info_setbw(black, white);
     float_on = float_save;
     return;
}

void imod_whitelevel_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     int black, white;
     int float_save = float_on;

     if (ImodForbidLevel)
	  return;

     white = cbs->value;
     black = App->cvi->black;

     /* DNM: If TrueColor and slider is being dragged, just display number */
     if (App->rgba && (cbs->reason == XmCR_DRAG)) {
	  display_bwslider_value(Imod_widget_whiteval, white);
	  return;
     }

     if (!white)
	  white = 1;
     if (white < black)
	  black = white - 1;

     xcramp_setlevels(App->cvi->cramp,black,white);

     App->cvi->black = black;
     App->cvi->white = white;

     /* Set the float flag to false to prevent this change from being 
	undone in a redraw */
     float_on = FALSE;
     imod_info_setbw(black, white);
     float_on = float_save;
     return;
}


void imod_float_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     if (cbs->set) {
          float_on = 1;
	  imod_info_setbw(App->cvi->black, App->cvi->white);
     } else
          float_on = 0;
     return;
}

/* DNM 6/8/01: fixed bug in getting mode, changed to pass mode to function */
void imod_mmode_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     int mode = (int)client;

     /* DNM 6/9/01: workaround for PC now has callbacks only on the disarm
	of the button that was pushed, so skip this test and unconditionally
	set the mode buttons */
     /* if (!cbs->set)
	return; */

     imod_set_mmode(mode);

     /* DNM 1/28/02: change from drawing xyz to general drawing */
     imodDraw(App->cvi, IMOD_DRAW_MOD);
     return;
}

void imod_info_quit(Widget w, XtPointer client, XtPointer call)
{
     Imod_info_quit = TRUE;
     imod_quit();
     return;
}


/****************************************************************************/
/* call back support functions.                                             */
/****************************************************************************/

void imod_info_setobjcolor(void)
{
     unsigned short red, green, blue;
     int clev;
     struct Mod_Object *obj;
     Status status;
     XColor color;
     Colormap cmap;

     obj = imodel_object_get(Model);
     if (!obj){
	  red = green = blue = 128;
     }else{
	  red   = 255 * obj->red;
	  green = 255 * obj->green;
	  blue  = 255 * obj->blue;
     }
     clev = ((red * 30) + (green * 59) + (blue * 11)) / 100;

     color.flags = DoRed | DoBlue | DoGreen;
     color.red   = (red   << 8);
     color.green = (green << 8);
     color.blue  = (blue  << 8);
     color.pixel = App->curobj;

     if (App->rgba){
	  /* DNM: the color is already there */
	  if (obj)
	       color.pixel = obj->fgcolor;
	  /*	 XAllocColor(App->display, App->cmap, &color); */
     }else{
	 XStoreColors(XtDisplay(App->toplevel), App->cmap, &color, 1);
	 if (App->cmap != App->cmapGL)
	      XStoreColors(XtDisplay(App->toplevel), App->cmapGL, &color, 1);
     }
     XtVaSetValues( Imod_widget_object, XmNbackground, 
		   color.pixel, NULL);

     if (clev > 127)
	  XtVaSetValues( Imod_widget_object, XtVaTypedArg,  
			XmNforeground, XmRString, "Black", 6, NULL);
     else
	  XtVaSetValues( Imod_widget_object, XtVaTypedArg,  
			XmNforeground, XmRString, "White", 6, NULL);
     return;
}

void imod_info_setocp(void)
{
    Imod *imod = Model;
     XmString str;
     char val[32];
 
     static int ob, co, pt;
     static int mob=0, mco=0, mpt=0;
     struct Mod_Object *obj;
     struct Mod_Contour *cont;
     
     obj = imodel_object_get(imod);
     cont = imodContourGet(imod);
     
     if ((imod->cindex.object + 1 != ob) || (imod->objsize != mob)){
	  ob  = imod->cindex.object + 1;
	  mob = imod->objsize;
	  co  = -2;
	  pt  = -2;
	  if (obj)
	       sprintf(val, "Object  %3d/%3d", imod->cindex.object + 1, 
		       imod->objsize);
	  else
	       sprintf(val, "Object  -x-/%3d", imod->objsize);
	  
	  str = XmStringCreateSimple(val);

	  XtVaSetValues(Imod_widget_object, XmNlabelString, str, NULL);
	  XmStringFree(str);
	  imod_object_edit_draw();
     }

    
    if (!obj){
	sprintf(val, "Contour");
	str = XmStringCreateSimple(val);
	XtVaSetValues(Imod_widget_contour, XmNlabelString, str, NULL);
	XmStringFree(str);
    }else{
	if (((imod->cindex.contour + 1) != co) || (obj->contsize != mco)){
	    co  = imod->cindex.contour + 1;
	    mco = obj->contsize;
	    pt  = -2;
	    if (cont)
		sprintf(val, "Contour %3d/%3d", imod->cindex.contour + 1,
			obj->contsize);
	    else
		sprintf(val, "Contour -x-/%3d", obj->contsize);
	    str = XmStringCreateSimple(val);
	    XtVaSetValues(Imod_widget_contour, XmNlabelString, str, NULL);
	    XmStringFree(str);
	}
    }
    
    if (!cont){
	 sprintf(val, "Point   ");
	 str = XmStringCreateSimple(val);
	 XtVaSetValues(Imod_widget_point, XmNlabelString, str, NULL);
	 XmStringFree(str);
    }else{

	if ((imod->cindex.point + 1 != pt) || (cont->psize != mpt)){
	    pt  = imod->cindex.point + 1;
	    mpt = cont->psize;
	    if (imod->cindex.point == -1)
		sprintf(val, "Point   -x-/%3d", cont->psize);
	    else{
		if ((cont) && (cont->psize)){
		    sprintf(val, "Point   %3d/%3d", imod->cindex.point + 1,
			    cont->psize);
		}else{
		    sprintf(val, "Point     0/  0");
		}
	    }
	    str = XmStringCreateSimple(val);
	    XtVaSetValues(Imod_widget_point, XmNlabelString, str, NULL);
	    XmStringFree(str);
	}
    }


     if ((Imod_obj_cnum != imod->cindex.object)
	 && (imod->cindex.object != -1)){

	  Imod_obj_cnum = imod->cindex.object;
	  imod_info_setobjcolor();

     }
     contSurfShow();
     inputContourMoveDialogUpdate();
     return;
}

void imod_info_setxyz(void)
{
     XmString str;
     char val[32];
     int x,xs,y,ys,z,zs;
     static int lx,ly,lz,lys,lzs;

     ivwBindMouse(App->cvi);
     x = App->cvi->xmouse + 1;
     y = App->cvi->ymouse + 1;
     z = App->cvi->zmouse + 1;
     xs = App->cvi->xsize;
     ys = App->cvi->ysize;
     zs = App->cvi->zsize;

     if (lx != x){
	  lx = x;
	  sprintf(val, "X = %3d / %3d", x, xs);
	  str = XmStringCreateSimple(val);
	  XtVaSetValues(Imod_widget_x, XmNlabelString, str, NULL);
	  XmStringFree(str);
     }

     if (ly != y || lys != ys){
	  ly = y;
	  lys = ys;
	  sprintf(val, "Y = %3d / %3d", y, ys);
	  str = XmStringCreateSimple(val);
	  XtVaSetValues(Imod_widget_y, XmNlabelString, str, NULL);
	  XmStringFree(str);
     }

     if (lz != z || lzs != zs){
	  lz = z;
	  lzs = zs;
	  sprintf(val, "Z = %3d / %3d", z, zs);
	  str = XmStringCreateSimple(val);
	  XtVaSetValues(Imod_widget_z, XmNlabelString, str, NULL);
	  XmStringFree(str);
     }
     set_pixelview(App->cvi);
     return;
}

static int ref_section;
static int ref_black = 0;
static int ref_white = 255;
static int last_section = 0;
static int ref_time;
static int last_time = 0;

void imod_info_setbw(int black, int white)
{
     static int oblack = 0;
     static int owhite = 255;
     int remute = FALSE;

     if (oblack != black){
	  oblack = black;
	  XtVaSetValues( Imod_widget_blacklevel, XmNvalue, black, NULL);
	  display_bwslider_value(Imod_widget_blackval, black);
	  remute = TRUE;
     }

     if (owhite != white){
	  owhite = white;
	  XtVaSetValues( Imod_widget_whitelevel, XmNvalue, white, NULL);
	  display_bwslider_value(Imod_widget_whiteval, white);
	  remute = TRUE;
     }

     /* if we are using a colormap that isn't
      * mutable then we need to redraw all image data.
      * use the IMOD_DRAW_IMAGE flag to redraw all image
      * data and clear all image caches, and IMOD_DRAW_NOSYNC to prevent
      * panning the zap window to the current model point
      */
     if ((remute) && (App->rgba)){
	  imodDraw(App->cvi, IMOD_DRAW_IMAGE | IMOD_DRAW_NOSYNC);
     }

     /* DNM: set this information as values for a new reference section for
	floated intensities */
     ref_section = last_section;
     ref_time = last_time;
     ref_black = black;
     ref_white = white;
     return;
}


/* Implements floating; i.e. adjusting of sliders according to changes in the
   mean and SD between images 
   Returns 0 if nothing was changed, or 1 if black/white levels changed */
static float *sec_mean = NULL;
static float *sec_sd = NULL;
static int table_size = 0;
static int tdim = 0;
int imod_info_bwfloat(ImodView *vw, int section, int time)
{
     float sample, matt, pctLo, pctHi, mean, sd;
     int i, newwhite, newblack, err1;
     int save_ref_sec,save_ref_black, save_ref_white, save_ref_time;
     int needsize, iref, isec;
     float sloperatio;
     unsigned char *image;
     int retval = 0;

     if (float_on) {

          /* Make sure table exists and is the right size */
	  tdim = ivwGetMaxTime(vw) + 1;
	  needsize = vw->zsize * tdim;
          if (table_size == 0) {
	       sec_mean = (float *)malloc(needsize * sizeof(float));
	       sec_sd = (float *)malloc(needsize * sizeof(float));
          } else if (table_size != needsize) {
	       sec_mean = (float *)realloc(sec_mean, 
					   needsize * sizeof(float));
	       sec_sd = (float *)realloc(sec_sd, needsize * sizeof(float));
	  }

	  if (!sec_mean || !sec_sd) {
	       imod_info_float_clear(-1, -1);
	       return 0;
	  }
	  
	  /* Clear out any new entries */
          if (table_size < needsize)
	       for (i = table_size; i < needsize; i++)
		    sec_mean[i] = sec_sd[i] = -1;
	  table_size = needsize;

	  if ((ref_section + 1) * (ref_time + 1) > table_size ||
	      (section + 1) * (time + 1) > table_size)
	       return 0;

	  /* Get information about reference and 
	     current sections if necessary */

	  if (time > 0 && ref_time == 0)
	       ref_time = 1;

	  matt = 0.05;
	  sample = 10000.0/(vw->xsize*vw->ysize);
	  if (sample > 1.0)
	       sample = 1.0;
	  
	  err1 = 0;
	  iref = tdim * ref_section + ref_time;
	  isec = tdim * section + time;
	  if (sec_sd[iref] < 0 ) {
	       image = ivwGetZSectionTime(vw, ref_section, ref_time);
	       err1 = sampleMeanSD(image, 0, vw->xsize, vw->ysize, sample,
				   matt, &sec_mean[iref], &sec_sd[iref]);
	  }
	       
	  if (!err1 && sec_sd[isec] < 0 ) {
	       image = ivwGetZSectionTime(vw, section, time);
	       err1 = sampleMeanSD(image, 0, vw->xsize, vw->ysize, sample,
				   matt, &sec_mean[isec], &sec_sd[isec]);
	  }
	       
	  if (!err1) {
	    
	       /* Compute new black and white sliders */
	       sloperatio = sec_sd[isec] / sec_sd[iref];

	       newblack = sec_mean[isec] - 
		 (sec_mean[iref] - ref_black) * sloperatio + 0.5;
	       newwhite = newblack + sloperatio * (ref_white - ref_black)
		    + 0.5;
		    
	       if (newblack < 0)
		    newblack = 0;
	       if (newwhite > 255)
		    newwhite = 255;

	       /* Set the sliders and the ramp; save and restore
		  reference section information */
	       if (newwhite != vw->white || newblack != vw->black) {
		    vw->black = newblack;
		    vw->white = newwhite;
		    xcramp_setlevels(vw->cramp, vw->black, vw->white);
		    save_ref_sec = ref_section;
		    save_ref_time = ref_time;
		    save_ref_black = ref_black;
		    save_ref_white = ref_white;
		    imod_info_setbw(vw->black, vw->white);
		    ref_section = save_ref_sec;
		    ref_time = save_ref_time;
		    ref_black = save_ref_black;
		    ref_white = save_ref_white;
		    retval = 1;
	       }
	  }
     }

     last_section = section;
     last_time = time;
     return retval;
}

/* Clear the information for floating sections - for one section or all 
   section < 0 and time < 0: clear entire table
   section = - number of sections and time >=0; clear all at this time
   section >= 0, time >= 0; just clear this section
*/
void imod_info_float_clear(int section, int time)
{
     int i;

     if (section < 0 && time < 0) {
          if (sec_mean)
	       free(sec_mean);
	  if (sec_sd)
	       free(sec_sd);
	  sec_mean = NULL;
	  sec_sd = NULL;
	  table_size = 0;
     } else if (section < 0) {
	  if ((-section) * tdim + time >= table_size)
	       return;
	  for (i = 0; i < -section; i++) {
	       sec_mean[i * tdim + time] = -1;
	       sec_sd[i * tdim + time] = -1;
	  }
     } else if (section * tdim + time < table_size) {
          sec_mean[section * tdim + time] = -1;
          sec_sd[section * tdim + time] = -1;
     }
     return;
}

/****************************************************************************/
/*  Imod link functions */

int imod_open(FILE *mfin)
{
     if (mfin == NULL)
	  /* new model */
	  {
	       Model = imodNew();
	       imodNewObject(Model);
	  }
     else
	  {
	       Model = (struct Mod_Model *)LoadModel(mfin, NULL);
	       if (Model == NULL){
		    return(-1);
	       }
	  }
     return(0);
}

void show_status(char *info)
{
     XmString str;

     if (!info)
	  return;

     imod_info_msg(info, " ");
     return;
}

void imod_show_info(char *info, int line)
{
     if (!info)
	  return;
     
     if (line == 1)
	  imod_info_msg(info, NULL);
     if (line == 2)
	  imod_info_msg(NULL, info);
     return;
}

void imod_info_msg(char *top, char *bot)
{
     XmString tstr = NULL;
     XmString bstr = NULL;

     if (top){
	  wprint("%s\n", top);
	  tstr = XmStringCreateSimple(top);
     }

     if (bot){
	  wprint("%s\n", bot);
	  bstr = XmStringCreateSimple(bot);
     }
     
     if (tstr)
	  XmStringFree(tstr);
     if (bstr)
	  XmStringFree(bstr);
     return;
}

void imod_info_forbid(void)
{
     ImodForbidLevel++;
     return;
}

void imod_info_enable(void)
{
     ImodForbidLevel--;
     if (ImodForbidLevel < 0)
	  ImodForbidLevel = 0;
     return;
}

/* DNM 6/8/01: changed so that it gets called with the actual mode to be
   changed to, or with a different value to toggle the mode */
void imod_set_mmode(int mode)
{
     if (Model){
	  if (mode == IMOD_MM_TOGGLE) {
	       if (Model->mousemode == IMOD_MMOVIE)
		    mode = IMOD_MMODEL;
	       else
		    mode = IMOD_MMOVIE;
	  }

	  if (mode == IMOD_MMOVIE){
	       Model->mousemode = IMOD_MMOVIE;
	       XmToggleButtonSetState(Imod_widget_movie, True, False);
	       XmToggleButtonSetState(Imod_widget_model, False, False);  
	  }
	  else{
	       App->cvi->xmovie = App->cvi->ymovie = App->cvi->zmovie = 
		    App->cvi->tmovie = 0;
	       Model->mousemode = IMOD_MMODEL;
	       XmToggleButtonSetState(Imod_widget_model, True, False); 
	       XmToggleButtonSetState(Imod_widget_movie, False, False);  
	  }
     }
     imodDraw(App->cvi, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC);
     return;
}

void imod_draw_window(void)
{
     if (Model){
	  imod_info_setocp();
     }
     if (App->cvi){
	  imod_info_setxyz();
     }
     return;
}

void oimod_imgcnt(char *string){imod_info_msg(Statstring, string);return;}

void imod_imgcnt(char *string)
{
     wprint("%s\n%s\r", Statstring, string);
     imod_info_input();
     return;
}
