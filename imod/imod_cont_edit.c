/*  IMOD VERSION 2.50
 *
 *  imod_cont_edit.c -- Edit contour data in imod.
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

#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/ToggleB.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <Xm/ArrowB.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <math.h>
#include <dia.h>
#include "imod.h"

#define contour_bits_width  16
#define contour_bits_height 16

static Pixmap horseshoePix = 0;
static Pixmap closedPix    = 0;
static int    horseshoe     = 0;
static unsigned char closedBits[] = {
     0xff, 0xff, 0x01, 0x80, 0xe1, 0x87, 0x31, 0x8c, 0x19, 0x98, 0x0d, 0x90,
     0x05, 0x90, 0x05, 0x90, 0x05, 0x90, 0x05, 0x90, 0x0d, 0x90, 0x19, 0x98,
     0x31, 0x8c, 0xe1, 0x87, 0x01, 0x80, 0xff, 0xff};
static unsigned char horseshoeBits[] = {
     0xff, 0xff, 0x01, 0x80, 0xe1, 0x87, 0xf1, 0x8f, 0x39, 0x8c, 0x1d, 0x88,
     0x0d, 0x80, 0x0d, 0x80, 0x0d, 0x80, 0x0d, 0x80, 0x1d, 0x88, 0x39, 0x8c,
     0xf1, 0x8f, 0xe1, 0x87, 0x01, 0x80, 0xff, 0xff};

static void set_replace_button(void);

struct contour_edit_struct{
     ImodView  *vw;
     diaDialog *dia;
     Iindex    i1, i2;
     Widget    w1, w2, w3, w4, w5, w6, w7, w2d, w8, w9, w10, w11, w12, w13;
     char      *prompt;
     int       store;
     int       movetosurf;
     int       surf_moveto;
     int       replace;
     int       enabled;
     int       keepsize;
};

static void setlabel(Widget w, Iindex *ind)
{
     char label[44];
     XmString str;

     if ((ind->object < 0)||(ind->contour < 0)||(ind->point < 0)){
	  sprintf(label, "Object None, Contour None, Point None ");
     }else{
	  sprintf(label, "Object %d, Contour %d, Point %d",
		  ind->object+1, ind->contour+1, ind->point+1);
     }
     str = XmStringCreateSimple(label);
     XtVaSetValues(w, XmNlabelString, str, NULL);
     XmStringFree(str);
     return;
}

static void button1_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     struct contour_edit_struct *ced = (struct contour_edit_struct *)client;

     ced->i1.object = ced->vw->imod->cindex.object;
     ced->i1.contour = ced->vw->imod->cindex.contour;
     ced->i1.point = ced->vw->imod->cindex.point;
     setlabel(ced->w1, &(ced->i1));
     return;
}

static void button2_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     struct contour_edit_struct *ced = (struct contour_edit_struct *)client;

     ced->i2.object = ced->vw->imod->cindex.object;
     ced->i2.contour = ced->vw->imod->cindex.contour;
     ced->i2.point = ced->vw->imod->cindex.point;
     setlabel(ced->w2, &(ced->i2));
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     Widget row, col, button;
     diaDialog *dia = (diaDialog *)call;
     struct contour_edit_struct *ced = (struct contour_edit_struct *)client;
     char *deflabel = "None                              ";

     row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, w,
	   NULL);
     XtVaCreateManagedWidget
	  (ced->prompt, xmLabelWidgetClass, row, NULL);

     col = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     button = XtVaCreateManagedWidget
	  ("Set 1", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(button, XmNactivateCallback, button1_cb, client);
     ced->w1 = XtVaCreateManagedWidget
	  (deflabel, xmLabelWidgetClass, col, 
	   XmNrecomputeSize, True,
	   XmNmarginRight, 10,
	   NULL);
     setlabel(ced->w1, &(ced->i1));
     col = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     button = XtVaCreateManagedWidget
	  ("Set 2", xmPushButtonWidgetClass, col, 
	   NULL);
     XtAddCallback(button, XmNactivateCallback, button2_cb, client);
     ced->w2 = XtVaCreateManagedWidget
	  (deflabel, xmLabelWidgetClass, col, 
	   XmNrecomputeSize, True,
	   XmNmarginRight, 10,
	   NULL);
     setlabel(ced->w2, &(ced->i2));
     return;
}

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     struct contour_edit_struct *ced = (struct contour_edit_struct *)client;

     diaDestroyDialog(dia);
     ced->dia = NULL;
     return;
}

/****************************************************************************/

static void break_help_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg("Contour Break Help:\n\n"
		"Break a single contour into two contours.  "
		"The break will occur at one or two set points.  "
		"To set a point, first select the current point then "
		"select the [Set 1] or [Set 2] button.\n\n",
		"If only one set point is set the original contour will "
		"contain all the points before the set point and a new "
		"contour will contain the set point and all the points "
		"after the set point.\n\n",
		"If both points are set the second contour will contain "
		"points between and including the two set points "
		" and the first contour "
		"will contain the remaining points.",
		NULL);
}

static void break_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     struct contour_edit_struct *ced = (struct contour_edit_struct *)client;
     Icont *nc, *cont1, *cont2, *cont;
     Icont *scont1, *scont2;
     Iobj *obj;
     Ipoint *p1, *p2;
     int ob, co, pt;
     int i, ni, pt1, pt2;
     int breakPoints = 1;

     /*
      * Check that at least the first break point is set.
      */
     if (ced->i1.point < 0){
	 wprint("\a\nContour Break Error:\n"
		"\tFirst break point not set.\n");
	 return;
     }

     if ((ced->i2.object >= 0) && 
	 (ced->i2.contour >= 0) && 
	 (ced->i2.point >= 0 )){
	  
	  if ( ( ced->i2.object != ced->i1.object) ||
	      (ced->i2.contour != ced->i1.contour)){
	       wprint("\a\nContour Break Error:\n"
		      "\tBoth break points must be on the same contour.\n");
	       return;
	  }
	  pt1 = ced->i1.point;
	  pt2 = ced->i2.point;
	  breakPoints = 2;
     }else{
	  pt1 = 0;
	  pt2 = ced->i1.point;
     }
     
     if ((pt1<0) || (pt2 < 0)){
	 wprint("\a\nContour Break Error:\n"
		"\tInvalid break points set.\n");
	 return;
     }
     if (pt1 == pt2){
	 wprint("\a\nContour Break Error:\n"
		"\tBreak points can't be the same.\n");
	 return;
     }

     /* DNM: make sure object number is valid before accessing it */
     if (ced->i1.object >= ced->vw->imod->objsize) {
	 wprint("\a\nContour Break Error:\n"
		"\tObject number no longer valid.\n");
	 return;
     }

     /* DNM: now set up to save and restore current location upon error */
     imodGetIndex(ced->vw->imod, &ob, &co, &pt);
     imodSetIndex(ced->vw->imod,
		  ced->i1.object, ced->i1.contour, ced->i1.point);
     obj = imodObjectGet(ced->vw->imod);

     /* DNM 2/12/01: need to test that the contour number is still legal */
     if (ced->i1.contour >= obj->contsize){
	 wprint("\a\nContour Break Error:\n"
		"\tContour number is no longer valid.\n");
	 imodSetIndex(ced->vw->imod, ob, co, pt);
	 return;
     }

     cont1 = imodContourGet(ced->vw->imod);

     if ((!obj) || (!cont1)){
	 wprint("\a\nContour Break Error:\n"
		"\tInvalid Model data or no break point selected.\n");
	 imodSetIndex(ced->vw->imod, ob, co, pt);
	 return;
     }

     if (!cont1->psize){
	 wprint("\a\nContour Break Error:\n"
		"\tCurrent contour has no points.\n");
	 imodSetIndex(ced->vw->imod, ob, co, pt);
	 return;
     }

     if ((pt1 >= cont1->psize) || (pt2 >= cont1->psize)){
	 wprint("\a\nContour Break Error:\n"
		"\tInvalid break points set.\n");
	 imodSetIndex(ced->vw->imod, ob, co, pt);
	 return;
     }


     cont = imodContourDup(cont1);     
     cont2 = cont;

     if (pt1 > pt2){
	  int tpt;
	  tpt = pt1;
	  pt1 = pt2;
	  pt2 = tpt;
     }
     
     /* DNM: handle sizes properly below */
     /* Build up our new contour. */
     ni = 0;
     if (breakPoints == 1){
	  for(i = pt2; i < cont1->psize; i++, ni++) {
	       cont2->pts[ni] = cont1->pts[i];
	       if (cont1->sizes)
		    cont2->sizes[ni] = cont1->sizes[i];
	  }
	  cont2->psize = ni;
     }else{
	  for(i = pt1; i <= pt2; i++, ni++) {
	       cont2->pts[ni] = cont1->pts[i];
	       if (cont1->sizes)
		    cont2->sizes[ni] = cont1->sizes[i];
	  }
	  cont2->psize = ni;
     }


     /* Resize our old contour. */
     if (breakPoints == 1){
	 cont1->psize = pt2;
     }else{
	  ni = pt1;
	  for(i = pt2+1; i < cont1->psize; i++, ni++) {
	       cont1->pts[ni] = cont1->pts[i];
	       if (cont1->sizes)
		    cont1->sizes[ni] = cont1->sizes[i];
	  }
	  cont1->psize -= pt2-pt1+1;
     }

     ced->i1.point = ced->i2.point = -1;

     setlabel(ced->w1, &(ced->i1));
     setlabel(ced->w2, &(ced->i2));

     /* Set wild flag for each resulting contour */
     imodel_contour_check_wild(cont1);
     imodel_contour_check_wild(cont2);

     imodObjectAddContour(obj, cont);
     imodDraw(ced->vw, IMOD_DRAW_MOD);
     return;
}

void inputContourBreak(ImodView *vw)
{
     Iobj  *obj;
     Icont *cont;
     Icont *nc;
     static struct contour_edit_struct ced;
     static int first = 1;

     if (first){
	  ced.dia = NULL;
	  first = 0;
     }
     if (ced.dia){
	  wprint("Error: Contour Break already open.\n");
	  return;
     }
     ced.vw = vw;
     ced.i1.object = vw->imod->cindex.object;
     ced.i1.contour = vw->imod->cindex.contour;
     ced.i1.point = vw->imod->cindex.point;
     ced.i2.object = -1;
     ced.i2.contour = -1;
     ced.i2.point = -1;
     ced.prompt = "Select break point(s)";

     ced.dia = diaVaCreateDialog
	  ("Imod: Break Contours",
	   App->toplevel, App->context,
	   DiaNcontrolButton, "Apply",
	   break_cb, (XtPointer)&ced,
	   DiaNcontrolButton, "Done", 
	   quit_cb, (XtPointer)&ced,
	   DiaNcontrolButton, "Help",
	   break_help_cb, (XtPointer)&ced,
	   DiaNworkAreaFunc, workarea_cb, (XtPointer)&ced,
	   DiaNwindowQuit, quit_cb, (XtPointer)&ced,
	   0);     
     return;
}

/****************************************************************************/

static void join_help_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg("Contour Join Help:\n\n"
		"Make a single contour from two separate contours and "
		"remove the old contours.\n",

		"To select contours to join, first select a model point "
		"within the contour and then "
		"select the [Set 1] or [Set 2] button.  "
		"Both set points must be set.\n\n",
		
		"For open type objects, the new contour will contain "
		"points from the first contour up to [Set 1] and will "
		"contain points from the second contour starting at "
		"[Set 2] through the end of the contour.  The exception to "
		"this is when [Set 1] is at the START of one contour and "
		"[Set 2] is at the END of the other contour.  In this case "
		"the new contour will contain all of the points from the "
		"original two contours, rather "
		"than just two points.\n\n",

		"For closed type objects, the contours will be joined at "
		"the closest points.\n\n",
		"For scattered type objects, ALL of the points from the second"
		" contour will be appended to the first contour, regardless of"
		" which points are selected.  No points will be discarded.\n",

		NULL);
}

static void join_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     struct contour_edit_struct *ced = (struct contour_edit_struct *)client;
     int ob, co, pt;
     Icont *cont1, *cont2, *jcont;
     Iobj *obj;
     double d1,d2;

     /* DNM 2/12/01: put in initial test for a "None" selection */
     if (ced->i1.object < 0 || ced->i1.contour < 0 || ced->i1.point < 0 ||
	 ced->i2.object < 0 || ced->i2.contour < 0 || ced->i2.point < 0) {
	 wprint("\a\nContour Join Error: No contour selected.\n");
	 return;
     }

     /*  DNM: separate objects are not allowed because the way joining is
	 treated depends on object type.  Move this test up to top */
     if (ced->i2.object != ced->i1.object){
	 wprint("\a\nContour Join Error:\n"
		"\tJoin contours must belong to the same object.\n");
	 return;
     }

     /* DNM 2/12/01: test for not being the same contour */
     if (ced->i2.contour == ced->i1.contour){
	 wprint("\a\nContour Join Error:\n"
		"\tSet points must be in different contours.\n");
	 return;
     }

     /* DNM 2/12/01: test for object still valid */
     if (ced->i2.object >= ced->vw->imod->objsize){
	 wprint("\a\nContour Join Error:\n"
		"\tObject number no longer valid.\n");
	 return;
     }

     imodGetIndex(ced->vw->imod, &ob, &co, &pt);
     imodSetIndex(ced->vw->imod, 
		  ced->i1.object, ced->i1.contour, ced->i1.point);

     obj   = imodObjectGet(ced->vw->imod);

     /* DNM 2/12/01: need to test that the contour numbers are still legal */
     if (ced->i1.contour >= obj->contsize || ced->i2.contour >= obj->contsize){
	 wprint("\a\nContour Join Error:\n"
		"\tContour number is no longer valid.\n");
	 imodSetIndex(ced->vw->imod, ob, co, pt);
	 return;
     }

     cont1 = imodContourGet(ced->vw->imod);
     imodSetIndex(ced->vw->imod, 
		  ced->i2.object, ced->i2.contour, ced->i2.point);
     cont2 = imodContourGet(ced->vw->imod);

     /* DNM: consolidate the identical tests on cont1 and cont2 */
     if (!cont1 || !cont2){
	 wprint("\a\nContour Join Error:\n"
		"\tInvalid Model data or no contour selected.\n");
	 imodSetIndex(ced->vw->imod, ob, co, pt);
	 return;
      }

     /* DNM: make sure the set points are still legal */
     if (ced->i1.point >= cont1->psize || ced->i2.point >= cont2->psize){
	 wprint("\a\nContour Join Error:\n"
		"\tContour has changed since point was set.\n");
	 imodSetIndex(ced->vw->imod, ob, co, pt);
	 return;
     }
     
     if (cont1->label || cont2->label)
	  if (dia_choice("Joining will destroy point labels; do you really "
			 "want to join?", "Yes", "No", NULL) != 1) {
	       imodSetIndex(ced->vw->imod, ob, co, pt);
	       return;
	  }

     pt = ced->i1.point;
     if (iobjScat(obj->flags)){
	  jcont = imodContourSplice(cont1, cont2, cont1->psize - 1, 0);
	  
     } else if (iobjOpen(obj->flags)){
         if (ced->i2.point > 0 && ced->i2.point == cont2->psize - 1 &&
	     ced->i1.point == 0 && cont1->psize > 1) {
	      jcont = imodContourSplice(cont2, cont1, ced->i2.point, 
					ced->i1.point);
	      pt = ced->i2.point;
	 } else
	      jcont = imodContourSplice(cont1, cont2, ced->i1.point,
					ced->i2.point);
     }else{
	 jcont = imodContourJoin(cont1, cont2, -1, -1, FALSE, 0);
     }

     if (cont1->psize)
	  free(cont1->pts);
     if (cont1->sizes)
	  free(cont1->sizes);
	  
     /* copy the structure elements into place in the contour array */
     imodContourCopy(jcont, cont1);

     /* DNM: set index past point in first contour, adjust contour # if 
	needed */
     co = ced->i1.contour;
     if (ced->i1.object == ced->i2.object && ced->i1.contour > ced->i2.contour)
          co--;
     if (pt + 1 < cont1->psize)
          pt++;

     /* check wild flag of new resulting contour */
     imodel_contour_check_wild(cont1);

     /* delete the second contour, which may shift cont1 down */
     imodDeleteContour(ced->vw->imod);
     
     imodSetIndex(ced->vw->imod, 
		  ced->i1.object, co, pt);

     imodDraw(ced->vw, IMOD_DRAW_MOD);
     return;
}

void inputContourJoin(ImodView *vw, int x, int y)
{
     static struct contour_edit_struct join;
     static int first = 1;

     if (first){
	  join.dia = NULL;
	  first = 0;
     }
     if (join.dia){
	  wprint("Error: Contour Join already open.\n");
	  return;
     }
     join.vw = vw;
     join.i1.object = vw->imod->cindex.object;
     join.i1.contour = vw->imod->cindex.contour;
     join.i1.point = vw->imod->cindex.point;
     join.i2.object = -1;
     join.i2.contour = -1;
     join.i2.point = -1;

     join.dia = diaVaCreateDialog
	  ("Imod: Join Contours",
	   App->toplevel, App->context,
	   DiaNcontrolButton, "Apply", join_cb, (XtPointer)&join,
	   DiaNcontrolButton, "Done",  quit_cb, (XtPointer)&join,
	   DiaNcontrolButton, "Help",  join_help_cb, (XtPointer)&join,
	   DiaNworkAreaFunc, workarea_cb, (XtPointer)&join,
	   DiaNwindowQuit, quit_cb, (XtPointer)&join,
	   0);
     return;
}

/*****************************************************************************/
/*   Contour Surface Edit                                                    */
/*   Also add time edit. */

static struct contour_edit_struct surf;
static int surfirst = 1;
static int size_scale_max = 500;
#define SIZE_SCALE_LIMIT  10000

static void set_size_text(float size)
{
     char vals[20];

     if (!surf.w8)
	  return;
     if (size >= 0.0) {
	  sprintf(vals, "%g", size);
	  XtVaSetValues(surf.w8, XmNvalue, vals, NULL);
     } else
	  XtVaSetValues(surf.w8, XmNvalue, "Default", NULL);
	  
}

static void set_size_scale(float size)
{
     int val;

     if (!surf.w9)
	  return;
     val = (int)(10. * size + 0.5);
     if (val < 0)
	  val = 0;
     if (val > size_scale_max) {
	  if (val > SIZE_SCALE_LIMIT)
	       val = SIZE_SCALE_LIMIT;
	  size_scale_max = val;
	  XtVaSetValues(surf.w9, XmNmaximum, size_scale_max, NULL);
     }
     XtVaSetValues(surf.w9, XmNvalue, val, NULL);
}

void contSurfShow(void)
{
     Icont *cont;
     Iobj  *obj;
     static char label[32];
     XmString str;
     int min,max,val;
     float size;
     int pt;
     Boolean state;

     if (surfirst)
	    return;

     if (!surf.dia)
	  return;

     obj  = imodObjectGet(surf.vw->imod);
     cont = imodContourGet(surf.vw->imod);

     if (!cont)
	  sprintf(label, "Surface -x-/ %d", obj->surfsize);
     else
	  sprintf(label, "Surface %d/ %d", cont->surf, obj->surfsize);

     str = XmStringCreateSimple(label);
     XtVaSetValues(surf.w1, XmNlabelString, str, NULL);
     XmStringFree(str);

     min = 0; 
     if (obj)  max = obj->surfsize; else max = 1;  /* was surfsize + 1 */
     if (cont) val = cont->surf;        else val = 0;
     if (!max)
	  max = 1;
     XtVaSetValues(surf.w2,
		   XmNminimum, min, 
		   XmNmaximum, max, 
		   XmNvalue, val,
		   NULL);

     /* Show state of ghost drawing */
     state = (surf.vw->ghostmode  & IMOD_GHOST_NEXTSEC) != 0;
     XmToggleButtonSetState (surf.w11, state, False);

     state = (surf.vw->ghostmode  & IMOD_GHOST_PREVSEC) != 0;
     XmToggleButtonSetState (surf.w12, state, False);


     /* show closed/horseshoe surface flag */
     if (cont){
	  if (cont->flags & ICONT_OPEN){
	       if (!horseshoe){
		    XtVaSetValues(surf.w4, XmNlabelPixmap, horseshoePix, NULL);
		    horseshoe = 1;
	       }
	  }else{
	       if (horseshoe){
		    XtVaSetValues(surf.w4, XmNlabelPixmap, closedPix, NULL);
		    horseshoe = 0;
	       }
	  }
     }

     /* Show time data. */
     if (surf.w5){
	  if (iobjTime(obj->flags)){
	       if (cont){
		    static char label[32];
		    sprintf(label, "%3d", cont->type);
		    XmTextSetString(surf.w5, label);
	       }
	       XtSetSensitive(surf.w5, True);
	  }else{
	       XmTextSetString(surf.w5, "No Time");
	       XtSetSensitive(surf.w5, False);
	  }
     }

     if (surf.w2d){
	 if ((cont)&&(cont->psize)){
	     static char label[32];
	     sprintf(label, "%3g", cont->pts->z);
	     XmTextSetString(surf.w2d, label);
	     XtSetSensitive(surf.w2d, True);   
	 }else{
	     XmTextSetString(surf.w2d, "No Contour");
	     XtSetSensitive(surf.w2d, False);  
	 }
     }
     
     if (surf.w6){
	  if (cont){
/*	       imodLabelPrint(cont->label, stdout); */
	       if ((cont->label) && (cont->label->name))
		    XmTextSetString(surf.w6, cont->label->name);
	       else
		    XmTextSetString(surf.w6, "");
	       
	       XtSetSensitive(surf.w6, True);
	       
	       if (surf.w7){
		    if (surf.vw->imod->cindex.point == -1){
			 XmTextSetString(surf.w7, "No Point");
			 XtSetSensitive(surf.w7, False);
		    }else{
			 char *plabel = NULL;
			 if (cont->label)
			      plabel = imodLabelItemGet
				   (cont->label, surf.vw->imod->cindex.point);
			 
			 if (plabel)
			      XmTextSetString(surf.w7, plabel);
			 else
			      XmTextSetString(surf.w7, "");
			 
			 XtSetSensitive(surf.w7,True);
		    }
	       }
	       
	  }else{
	       XmTextSetString(surf.w6, "No Contour");
	       XtSetSensitive(surf.w6, False);
	       if (surf.w7){
		    XmTextSetString(surf.w7, "No Point");
		    XtSetSensitive(surf.w7, False);
	       }
	  }

     }


     /* Display size as 'default' in the text box but actual size in slider */
     pt = surf.vw->imod->cindex.point;
     size = -1.0;
     if (cont && (pt != -1) && cont->sizes) 
	  size = cont->sizes[pt];
     set_size_text(size);

     if (cont && (pt != -1)) 
	  size = imodPointGetSize(obj, cont, pt);
     else
	  size = obj->pdrawsize;
     set_size_scale(size);
	       
     return;
}

static void surftime_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     Icont *cont;
     Iobj  *obj;
     
     obj = imodObjectGet(surf.vw->imod);
     if ((obj) &&  (iobjTime(obj->flags))){
	  cont = imodContourGet(surf.vw->imod);
	  if (cont){
	       char *st = XmTextGetString(w);
	       cont->type = atoi(st);
	       cont->flags |= ICONT_TYPEISTIME;
	       XtFree(st);
	  }
     }	  
     contSurfShow();
     imodDraw(surf.vw, IMOD_DRAW_MOD);
}

static void z_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     Icont *cont = imodContourGet(surf.vw->imod);
     float z;
     char *st;
     int pt;
     if (!cont) return;

     st = XmTextGetString(w);
     if (!st) return;
     z = atof(st);

     for(pt = 0; pt < cont->psize; pt++)
	 cont->pts[pt].z = z;
     XtFree(st);
}

static void clabel_cb(Widget w, XtPointer client, XtPointer call)
{
     char *st;
     Icont *cont = imodContourGet(surf.vw->imod);
     if (!cont) return;
     
     st = XmTextGetString(w);
     if (!cont->label)
	  cont->label = imodLabelNew();
     imodLabelName(cont->label, st);
     XtFree(st);
     return;
}

static void plabel_cb(Widget w, XtPointer client, XtPointer call)
{
     char *st;
     Icont *cont = imodContourGet(surf.vw->imod);
     if (!cont) return;
     
     st = XmTextGetString(w);
     if (!cont->label)
	  cont->label = imodLabelNew();
     
     imodLabelItemAdd(cont->label, st, surf.vw->imod->cindex.point);
     XtFree(st);
}

static void surfnew_cb(Widget w, XtPointer client, XtPointer call)
{
     inputNewSurface(surf.vw);
}

static void surfgoto_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     Icont *cont;
     Iobj  *obj;
     int distmin = 1000000;
     int co, closest, dist;
     int target = cbs->value;
     
     obj = imodObjectGet(surf.vw->imod);

     if (!obj || !obj->contsize)
	  return;

     /* if target is next or previous contour, use the AdjacentSurface call */
     cont = imodContourGet(surf.vw->imod);
     if (cont)
	  if (cont->surf == target + 1 || cont->surf == target - 1) {
	       inputAdjacentSurface(surf.vw, target - cont->surf);
	       return;
	  }
     
     /* find the first contour with the closest surface number */
     for (co = 0; co < obj->contsize; co++) {
	  dist = obj->cont[co].surf - target;
	  if (dist < 0)
	       dist = -dist;
	  if (dist < distmin) {
	       distmin = dist;
	       closest = co;
	       if (!dist)
		    break;
	  }
     }
     
     surf.vw->imod->cindex.contour = closest;

     /* if point index is too high or low, change it. */
     if (surf.vw->imod->cindex.point >= obj->cont[closest].psize)
	  surf.vw->imod->cindex.point = obj->cont[closest].psize - 1;
     if (surf.vw->imod->cindex.point < 0 && obj->cont[closest].psize > 0)
	  surf.vw->imod->cindex.point = 0;

     imod_setxyzmouse();

     contSurfShow();
     return;
}

static void surfghost_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     Iobj *obj   = imodObjectGet(surf.vw->imod);
     Icont *cont = imodContourGet(surf.vw->imod);
     int flag = (int)client;

     if (!cbs->set)
	  surf.vw->ghostmode &= ~flag;
     else
	  surf.vw->ghostmode |= flag;

     /* Save the state of the mode for section ghost, unless it's all turned
	off */
     if ((flag & IMOD_GHOST_SECTION) && 
	 (surf.vw->ghostmode & IMOD_GHOST_SECTION))
	  surf.vw->ghostlast = surf.vw->ghostmode;
     
     imodDraw(App->cvi, IMOD_DRAW_MOD);

     return;
}

static void surfshoe_cb(Widget w, XtPointer client, XtPointer call)
{
     Icont *cont = imodContourGet(surf.vw->imod);
     
     if (cont){
	  if (cont->flags & ICONT_OPEN){
	       horseshoe = 0;
	       XtVaSetValues(surf.w4, XmNlabelPixmap, closedPix, NULL);
	       cont->flags &= ~ICONT_OPEN;
	  }else{
	       horseshoe = 1;
	       XtVaSetValues(surf.w4, XmNlabelPixmap, horseshoePix, NULL);
	       cont->flags |= ICONT_OPEN;
	  }
     }
     return;
}

static void size_scale_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     Icont *cont;
     float size;
     int pt = surf.vw->imod->cindex.point;

     size = cbs->value / 10.;

     set_size_text(size);

     cont = imodContourGet(surf.vw->imod);
     if (!cont || (pt == -1))
	  return;

     imodPointSetSize(cont, pt, size);
     imodDraw(surf.vw, IMOD_DRAW_MOD);
}

static void size_text_cb(Widget w, XtPointer client, XtPointer call)
{
     char *st = NULL;
     Icont *cont;
     float size;
     int pt = surf.vw->imod->cindex.point;

     st = XmTextGetString(w);
     size = atof(st);
     XtFree(st);
     if (size < 0.0) {
	  size = 0.0;
	  set_size_text(size);
     }

     set_size_scale(size);

     cont = imodContourGet(surf.vw->imod);
     if (!cont || (pt == -1))
	  return;

     imodPointSetSize(cont, pt, size);
     imodDraw(surf.vw, IMOD_DRAW_MOD);
}

static void continsurf_cb(Widget w, XtPointer client, XtPointer call)
{
     int direction = (int)client;
     inputAdjacentContInSurf(surf.vw, direction);
}

static void surfinobj_cb(Widget w, XtPointer client, XtPointer call)
{
     int direction = (int)client;
     inputAdjacentSurface(surf.vw, direction);
}

static void surfworkarea_cb(Widget w, XtPointer client, XtPointer call)
{
     Widget row, col, button, arrow, row2, row3;
     diaDialog *dia = (diaDialog *)call;
     Pixel  fg, bg;
     int    depth;
     Boolean state;

     row = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, w,
	   NULL);

     surf.w2 = XtVaCreateManagedWidget
	  ("scale", xmScaleWidgetClass, row,
	   XmNminimum, 0, 
	   XmNmaximum, 1,
	   XmNvalue, 0,
	   XmNshowValue, True,
	   XmNorientation,   XmHORIZONTAL,
	   XmNscaleMultiple, 1,
	   XtVaTypedArg, XmNtitleString,
	   XmRString, "Go To Surface", 14,
	   NULL);
     XtAddCallback(surf.w2, XmNvalueChangedCallback, 
		   surfgoto_cb, (XtPointer)&surf);

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   NULL);

     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, col, 
	   XmNarrowDirection, XmARROW_UP, NULL);
     XtAddCallback(arrow, XmNarmCallback, 
		   (XtCallbackProc)surfinobj_cb, (XtPointer)(1));

     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, col, 
	   XmNarrowDirection, XmARROW_DOWN, NULL);
     XtAddCallback(arrow, XmNarmCallback, 
		   (XtCallbackProc)surfinobj_cb, (XtPointer)(-1));

     surf.w1 = XtVaCreateManagedWidget
	  ("Surface 9999/9999", xmLabelWidgetClass, col, NULL);

     XtManageChild(col);

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   NULL);

     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, col, 
	   XmNarrowDirection, XmARROW_UP, NULL);
     XtAddCallback(arrow, XmNarmCallback, 
		   (XtCallbackProc)continsurf_cb, (XtPointer)(1));

     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, col, 
	   XmNarrowDirection, XmARROW_DOWN, NULL);
     XtAddCallback(arrow, XmNarmCallback, 
		   (XtCallbackProc)continsurf_cb, (XtPointer)(-1));

     XtVaCreateManagedWidget
	  ("Contour in surface",
	   xmLabelWidgetClass, col, NULL);
     XtManageChild(col);


     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   NULL);

     button = XtVaCreateManagedWidget
	  ("New Surf", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   surfnew_cb, NULL);

     surf.w3 = XtVaCreateManagedWidget
	  ("Surface ghost",  xmToggleButtonWidgetClass, col, NULL);
     XtAddCallback(surf.w3, XmNvalueChangedCallback, surfghost_cb, 
		   (XtPointer)IMOD_GHOST_SURFACE);
     XmToggleButtonSetState
	  (surf.w3, (surf.vw->ghostmode  & IMOD_GHOST_SURFACE), False);

     XtManageChild(col);

     /* col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNpacking, XmPACK_TIGHT,
	   NULL); */
     col = XtVaCreateWidget("form", xmFormWidgetClass, row, NULL);

     row2 = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col,
	   XmNtopAttachment,    XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNleftAttachment,   XmATTACH_FORM,
	   NULL);

     XtVaCreateManagedWidget
	  ("Section ghost:", xmLabelWidgetClass, row2, 
	 XmNmarginHeight, 0, NULL);
     row3 = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, row2,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     surf.w11 = XtVaCreateManagedWidget
	  ("Up",  xmToggleButtonWidgetClass, row3, 
	 XmNmarginHeight, 0, NULL);
     XtAddCallback(surf.w11, XmNvalueChangedCallback, surfghost_cb, 
		   (XtPointer)IMOD_GHOST_NEXTSEC);
     state = (surf.vw->ghostmode  & IMOD_GHOST_NEXTSEC) != 0;
     XmToggleButtonSetState (surf.w11, state, False);

     surf.w12 = XtVaCreateManagedWidget
	  ("Down",  xmToggleButtonWidgetClass, row3,
	   XmNmarginHeight, 0, NULL);
     XtAddCallback(surf.w12, XmNvalueChangedCallback, surfghost_cb, 
		   (XtPointer)IMOD_GHOST_PREVSEC);
     state = (surf.vw->ghostmode  & IMOD_GHOST_PREVSEC) != 0;
     XmToggleButtonSetState (surf.w11, state, False);
     XtManageChild(row3);
     XtManageChild(row2);

     XtVaGetValues(surf.w3,
		   XmNforeground, &fg,
		   XmNbackground, &bg,
		   XmNdepth, &depth,
		   NULL);

     if (!horseshoePix){
	  horseshoePix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel),
		(char *)horseshoeBits,
		contour_bits_width, contour_bits_height,
		fg, bg, depth);
     }
     if (!closedPix){
	  closedPix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel),
		(char *)closedBits,
		contour_bits_width, contour_bits_height,
		fg, bg, depth);
     }
     surf.w4 = XtVaCreateManagedWidget
	  ("closed", xmPushButtonWidgetClass, col, 
	   XmNlabelType, XmPIXMAP,
	   XmNindicatorOn, False,
	   XmNlabelPixmap, closedPix,
	   XmNtopAttachment,    XmATTACH_FORM,
	   XmNrightAttachment,   XmATTACH_FORM,
	   NULL);
     XtAddCallback(surf.w4, XmNactivateCallback, surfshoe_cb, NULL);
     horseshoe = 0;

     XtManageChild(col);

     XtVaCreateManagedWidget
	  ("Time Index", xmLabelWidgetClass, row, NULL);
     surf.w5 = XtVaCreateManagedWidget
	  ("Time Index", xmTextWidgetClass, row,
	   NULL);
     XtAddCallback(surf.w5, XmNactivateCallback, surftime_cb, NULL);

     XtVaCreateManagedWidget
	  ("Contour Label", xmLabelWidgetClass, row, NULL);
     surf.w6 = XtVaCreateManagedWidget
	  ("label", xmTextWidgetClass, row, NULL);
     XtAddCallback(surf.w6, XmNactivateCallback, clabel_cb, NULL);


     XtVaCreateManagedWidget
	  ("Point Label", xmLabelWidgetClass, row, NULL);
     surf.w7 = XtVaCreateManagedWidget
	  ("label", xmTextWidgetClass, row, NULL);
     XtAddCallback(surf.w7, XmNactivateCallback, plabel_cb, NULL);

     if (!(App->cvi->dim & 4)){
	 XtVaCreateManagedWidget
	     ("Z value", xmLabelWidgetClass, row, NULL);
	 surf.w2d = XtVaCreateManagedWidget
	     ("Z value", xmTextWidgetClass, row,
	      NULL);
	 XtAddCallback(surf.w2d, XmNactivateCallback, z_cb, NULL);
     }else
	 surf.w2d = 0;

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     XtVaCreateManagedWidget
	  ("Point Size", xmLabelWidgetClass, col, NULL);
     surf.w8 = XtVaCreateManagedWidget
	  ("psize", xmTextWidgetClass, col,
	   XmNcolumns, 10,
	   NULL);
     XtAddCallback(surf.w8, XmNactivateCallback, size_text_cb, NULL);

     XtManageChild(col);

     surf.w9 = XtVaCreateManagedWidget
	  ("scale", xmScaleWidgetClass, row,
	   XmNminimum, 0, 
	   XmNmaximum, size_scale_max,
	   XmNvalue, 0,
	   XmNshowValue, True,
	   XmNorientation,   XmHORIZONTAL,
	   XmNscaleMultiple, 5,
	   XmNdecimalPoints, 1,
	   NULL);
     XtAddCallback(surf.w9, XmNvalueChangedCallback, size_scale_cb, NULL);

     XtManageChild(row);
     return;
}

static void surfhelp_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg("Contour Type Help\n"
		"---------------------------\n\n",
		
		"Surfaces:\n",
		"Each contour has its own surface number.  "
		"The surface number can be used for subgrouping contours "
		"without having to create new objects.  The surface number "
		"of the current contour is displayed to the right of the "
		"surface up and down arrows (\"Surface #/#\").  The first "
		"number is the current surface, the second number is the "
		"maximum surface number in the current object.\n\n",
		
		"The \"Go To Surface\" slider can be used to switch to the "
		"first contour in the selected surface.  Its value will be "
		"maintained at the current surface number when you move "
		"between contours, but when you click or drag the slider, it "
		"will change surfaces.\n\n"

		"The Surface Up and Down arrows can be used to move to the "
		"first contour in the next or previous surface, respectively. "
		"Non-existent surfaces are skipped.\n\n"

		"The \"Contour in Surface\" Up and Down arrows can be used to "
		"step to the next and previous contours within the current "
		"surface.\n\n"

		"The \"New Surf\" button will start a new surface with the "
		"next free number and "
		"create an empty contour with that surface number.\n\n"

		"The \"Surface ghost\" toggle button may be used during model "
		"editing to highlight the current contour, along with all "
		"other contours with the same surface number. \n\n",

		"The \"Section ghost\" toggle buttons may be used to control "
		"the section-to-section ghost display mode.  If the \"Up\" "
		"button is on, then contours will be displayed as ghosts on "
		"the next section; the \"Down\" button will display contours "
		"as ghosts on the previous section.  The \"g\" hotkey will "
		"toggle "
		"the combination selected by these buttons on and off.\n\n",

		"Next to the ghost checkboxes is the open/closed gadget.  "
		"The button toggles between closed and open contours in an "
		"object defined as having closed contours.  "
		"The default is a closed contour where the last point is "
		"connected to the first.  ",
		"An open contour will not be connected between the "
		"last and first points, thus allowing a partially cut surface "
		"to be represented by an object containing both closed and "
		"open contours.  The button has no effect for an object "
		"defined as having open contours.",

		"\n\nTime Index:\n",
		"The time index for datasets that vary over time can "
		"be edited with the Time Index text box.  "
		"The new time won't take effect unless Enter is pressed "
		"after typing in the new time index.  This box is active only "
		"when the Time Data toggle is selected for the particular "
		"object in the Edit-Object dialog."

		"\n\nLabels:\n",
		"It is possible to have labels for each contour and point.  "
		"Press Enter after editing a new label.",
		"\n\nPoint Size:\n",
		"It is also possible to assign a size for each point.  "
		"If the object type is scattered points, this number will "
		"determine the size at which the particular point will be "
		"displayed.  Points without a size will still be displayed at "
		"the size specified for the object as a whole.  You can "
		"change a size either by typing a number into the text box "
		"(followed by Enter) or by using the slider.  If a point has "
		"no size, the text box displays 'Default' and the slider is "
		"set to the global point size for the object.\n",
		NULL);

     return;
}

void inputContourSurf(ImodView *vw)
{
     if (surfirst){
	  surf.dia = NULL;
	  surfirst = 0;
     }
     if (surf.dia){
	  wprint("Error: Contour Surface already open.\n");
	  XRaiseWindow(App->display, XtWindow(surf.dia->dialog));
	  return;
     }
     surf.vw = vw;
     
     surf.dia = diaVaCreateDialog
	  ("Imod: Contour Type",
	   App->toplevel, App->context,
	   DiaNcontrolButton, "Done",  quit_cb, (XtPointer)&surf,
	   DiaNcontrolButton, "Help",  surfhelp_cb, (XtPointer)&surf,
	   DiaNworkAreaFunc, surfworkarea_cb, (XtPointer)&surf,
	   DiaNwindowQuit, quit_cb, (XtPointer)&surf,
	   0);
     contSurfShow();
     return;
}



/*****************************************************************************/
/*  Contour Move                                                             */
/*****************************************************************************/
/*
 * Move a contour to a different object.
 *
 * Todo: Add copy or move ability.
 *       Move/copy a contour to a new section or time point.
 */
static struct contour_edit_struct comv;
static int movefirst = 1;

/* The current object to move the contour to.  This allows hotkeys
 * to be used to move the contour to the current 'moveto' object.
 */
extern int Imod_obj_moveto;

static void movehelp_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg("Contour Move Help\n",
		"---------------------------\n",
		"This dialog is used to move contours to a different object "
		"or to a different surface.  ",
		"Use the slider to select the object or surface # to move "
		"the current contour to.  ",
		"IMOD remembers the object #, so the hot key 'M' in the ",
		"Zap window can be used to move contours quickly.\n\n",
		"When the first toggle button is selected, all contours with ",
		"the same surface number as the current contour will also ",
		"be moved to the selected object.  If no surfaces have been "
		"assigned in the object, then all contours in the object will"
		" be moved.\n\n",
		"When the second toggle button is selected, contours are "
		"assigned to a different surface in the same object rather "
		"than moved to a new object.\n\n",
		"When the type of the selected target object is scattered "
		"points and the current object is not scattered points, "
		"the third toggle button can be used to convert the current "
		"contour into a single point centered on the contour and with "
		"a size that corresponds to the area of the contour.  The new "
		"point will be added to the end of the last contour in the "
		"target object.  The "
		"current contour will be deleted.\n\n",
		"When the current object consists of scattered points, the "
		"fourth button can be used to specify whether the sizes of "
		"the points will be completely preserved when they are "
		"transferred to the new object.  If the button is selected, "
		"points that do not have their own individual sizes will be "
		"assigned the default size of the current object before being "
		"moved to the new object.  If the button is not depressed, "
		"such points will not be assigned individual sizes, and they "
		"will acquire the default size of the object they are moved "
		"to.  In either case, points that do have individual sizes "
		"will retain these sizes after the transfer.\n\n",
		"When moving contours to a different object, the surface "
		"number that they have in their new home depends on whether "
		"the first toggle button is selected.  If it is not (i.e., "
		"you are moving one contour at a time), then each "
		"contour moved will have the same surface number as before, "
		"and no surfaces will be created if there are none already.\n"
		"If the button is selected (i.e., you are moving all contours "
		"with the same surface number), then the set of contours that "
		"get moved in one operation will be assigned to a new surface "
		"with the next free number in the new object.  If you want to "
		"move all of the contours in an object with no surfaces into "
		"a different object and have them occupy a separate surface "
		"in that object, select the option to move all contours with "
		"the same surface number.\n",

		NULL);
     return;
}

static void moveapply_cb(Widget w, XtPointer client, XtPointer call)
{
     if (comv.vw->obj_moveto > comv.vw->imod->objsize)
	  comv.vw->obj_moveto = comv.vw->imod->objsize;
     if (comv.vw->obj_moveto < 1)
	  comv.vw->obj_moveto = 1;

     inputContourMove();

     imod_setxyzmouse();
     return;
}

/* Manage the three check box sensitivities to reflect the mutual exclusivity 
   of the two options */
static void set_replace_button()
{
     Iobj *obj;
     unsigned int scat = 0;
     obj = imodObjectGet(comv.vw->imod);
     if (obj)
	  scat = iobjScat(obj->flags);
     if(iobjScat(comv.vw->imod->obj[comv.vw->obj_moveto - 1].flags) &&
	!comv.movetosurf && !scat) {
	  XmToggleButtonSetState(comv.w4, comv.replace, False);
	  XtSetSensitive(comv.w4, True);
	  if (comv.replace) {
	       XtSetSensitive(comv.w3, False);
	       XmToggleButtonSetState(comv.w3, False, False);
	       XtSetSensitive(comv.w10, False);
	       XmToggleButtonSetState(comv.w10, False, False);
	  } else {
	       XtSetSensitive(comv.w3, True);
	       XmToggleButtonSetState(comv.w3, comv.store, False);
	       XtSetSensitive(comv.w10, True);
	       XmToggleButtonSetState(comv.w10, comv.movetosurf, False);
	  }
     } else {
	  XmToggleButtonSetState(comv.w4, False, False);
	  XtSetSensitive(comv.w4, False);
	  XtSetSensitive(comv.w3, True);
	  XmToggleButtonSetState(comv.w3, comv.store, False);
	  XtSetSensitive(comv.w10, True);
	  XmToggleButtonSetState(comv.w10, comv.movetosurf, False);
     }

     if (scat && !comv.movetosurf)
	  XtSetSensitive(comv.w11, True);
     else
	  XtSetSensitive(comv.w11, False);
}

static void movesurf_cb(Widget w, XtPointer client, XtPointer call)
{
     if (comv.store)
	  comv.store = 0;
     else 
	  comv.store = 1;
     return;
}

static void movetosurf_cb(Widget w, XtPointer client, XtPointer call)
{
     XmString str;

     if (comv.movetosurf) {
	  comv.movetosurf = 0;
	  str = XmStringCreateSimple
	       ("Move Current Contour to Selected Object.  ");
     } else {
	  comv.movetosurf = 1;
	  str = XmStringCreateSimple
	       ("Move Current Contour to Selected Surface.");
     }
     XtVaSetValues(comv.w1, XmNlabelString, str, NULL);
     XmStringFree(str);
     inputContourMoveDialogUpdate();

     set_replace_button();
     return;
}

static void movereplace_cb(Widget w, XtPointer client, XtPointer call)
{
     if (comv.replace)
	  comv.replace = 0;
     else
	  comv.replace = 1;
     set_replace_button();
     return;
}

static void movenew_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     if (comv.movetosurf)
	  comv.surf_moveto = cbs->value;
     else {
	  App->cvi->obj_moveto = cbs->value;
	  set_replace_button();
     }
     return;
}

static void movekeepsize_cb(Widget w, XtPointer client, XtPointer call)
{
     if (comv.keepsize)
	  comv.keepsize = 0;
     else
	  comv.keepsize = 1;
     return;
}

static void moveworkarea_cb(Widget w, XtPointer client, XtPointer call)
{
     Widget row, col, button;
     diaDialog *dia = (diaDialog *)call;
	  
     row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, w,
	   NULL);

     comv.w1 = XtVaCreateManagedWidget
	  ("Move Current Contour to Selected Object.", 
	   xmLabelWidgetClass, row, NULL);

     /* DNM: add XmNscaleMultiple to get left button click to work */
     comv.w2 = XtVaCreateManagedWidget
	  ("scale", xmScaleWidgetClass, row,
	   XmNminimum, 1, 
	   XmNmaximum, 2,
	   XmNvalue, 1,
	   XmNshowValue, True,
	   XmNorientation,   XmHORIZONTAL,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(comv.w2, XmNvalueChangedCallback, 
		   movenew_cb, (XtPointer)&comv);

     comv.w3 = XtVaCreateManagedWidget
	  ("Move all contours with same surface #.",  
	   xmToggleButtonWidgetClass, row, NULL);
     XtAddCallback(comv.w3, XmNvalueChangedCallback, movesurf_cb, NULL);

     XmToggleButtonSetState(comv.w3, comv.store, False);

     comv.w10 = XtVaCreateManagedWidget
	  ("Move contour to different surface, not object.",  
	   xmToggleButtonWidgetClass, row, NULL);
     XtAddCallback(comv.w10, XmNvalueChangedCallback, movetosurf_cb, NULL);

     XmToggleButtonSetState(comv.w10, comv.movetosurf, False);

     comv.w4 = XtVaCreateManagedWidget
	  ("Replace contour by single point of same size.",  
	   xmToggleButtonWidgetClass, row, NULL);
     XtAddCallback(comv.w4, XmNvalueChangedCallback, movereplace_cb, NULL);

     comv.w11 = XtVaCreateManagedWidget
	  ("Preserve sizes of points with default size.",  
	   xmToggleButtonWidgetClass, row, NULL);
     XtAddCallback(comv.w11, XmNvalueChangedCallback, movekeepsize_cb, NULL);
     XmToggleButtonSetState(comv.w10, comv.keepsize, False);

     set_replace_button();
     return;
}

/* move the current contour in the model to the selected moveto object. */
void inputContourMove(void)
{
     Iobj *obj, *tobj;
     Icont *cont;
     int surf, ob, co, pt;
     int nsurf;
     float firstz, size;
     double weight;
     Ipoint ccent;
     int conew, ptnew;

     /* Check that user has set up the move operation. */
     if (movefirst){
	  wprint("\aError: Select Edit->Contour->Move to setup move.\n");
	  return;
     }

     if (!comv.enabled) {
	  wprint("\aError: Must have move than one object or surface to "
		 "be able to move contours.\n");
	  return;
     }

     imodGetIndex(App->cvi->imod, &ob, &co, &pt);
     /* object to move current contour to. */
     tobj = &(App->cvi->imod->obj[App->cvi->obj_moveto - 1]);

     /* Get current object and contour. */
     obj = imodObjectGet(App->cvi->imod);
     if (!obj)  return;
     cont = imodContourGet(App->cvi->imod);
     if (!cont)  return;

     /* DNM 3/29/01: set up values for new contour and point */
     co = App->cvi->imod->cindex.contour;
     conew = co;
     ptnew = pt;

     /* Option to replace contour by point supercedes other options */

     if (iobjScat(tobj->flags) && comv.replace && !comv.movetosurf && 
	 !iobjScat(obj->flags)) {
	  if (cont->psize < 3) {
	       wprint("\aError: Contour must have at least 3 points.\n");
	       return;
	  }
	  firstz = cont->pts[0].z;
	  for (pt = 1; pt < cont->psize; pt++)
	       if (cont->pts[pt].z != firstz) {
		    wprint("\aError: Contour not all in one plane.\n");
		    return;
	       }

	  /* Get the centroid and the area, use area to set the size */

	  imodel_contour_centroid(cont, &ccent, &weight);
	  ccent.x /= weight;
	  ccent.y /= weight;
	  ccent.z = firstz;
	  size = sqrt((double)(imodContourArea(cont)/3.14159));
	  if(!tobj->contsize) {
	       cont = imodContourNew();
	       imodObjectAddContour(tobj, cont);
	  }
	  cont = &(tobj->cont[tobj->contsize - 1]);
	  imodPointAppend(cont, &ccent);
	  imodPointSetSize(cont, cont->psize - 1, size);
	  imodObjectRemoveContour(obj, co);

	  /* DNM 3/29/01: drop back to previous contour and set no
	     current point */
	  conew = co - 1;
	  ptnew = -1;

     } else if (comv.movetosurf) {
	  /* move contours to different surface */
	  if (comv.store){
	       /* Move all contours with the same surface number. */
	       surf = cont->surf;
	       for(co = 0; co < obj->contsize; co++){
		    if (obj->cont[co].surf == surf)
			 obj->cont[co].surf = comv.surf_moveto;
	       }
	  } else
	       cont->surf = comv.surf_moveto;

	  if (obj->surfsize < comv.surf_moveto)
	       obj->surfsize = comv.surf_moveto;
	  inputContourMoveDialogUpdate();

     } else {
	  /* move contours to other object: require a different object */
	  if (tobj == obj) {
	       wprint("\aError: Trying to move contour to object it"
		      " is already in.\n");
	       return;
	  }
	       
	  if (comv.store){
	       /* Move all contours with the same surface number. */
	       /* Assign them the first free surface # in destination object */
	       surf = cont->surf;
	       nsurf = imodel_unused_surface(tobj);
	       if (tobj->surfsize < nsurf)
		    tobj->surfsize = nsurf;

	       for(co = 0; co < obj->contsize; co++){
		    if (obj->cont[co].surf == surf){
			 cont = &(obj->cont[co]);
			 cont->surf = nsurf;

			 /* Set all the sizes before moving, if it is a
			    scattered object and button is set for this */
			 if (iobjScat(obj->flags) && comv.keepsize)
			      for (pt = 0; pt < cont->psize; pt++)
				   imodPointSetSize(cont, pt, imodPointGetSize
						    (obj, cont, pt));
			 imodObjectAddContour(tobj, cont);
			 if (!imodObjectRemoveContour(obj, co))
			      co--;

			 /* DNM 3/29/01: if move any contours, better set
			    contour number undefined */
			 conew = -1;
			 ptnew = -1;
		    }
	       }
	  
	  }else{
	       /* Moving one contour, just keep the surface number as is and
		  Adjust surface # limit if necessary; fix point sizes if
		  scattered points and that option selected */
	       if (cont->surf > tobj->surfsize)
		    tobj->surfsize = cont->surf;
	       if (iobjScat(obj->flags) && comv.keepsize)
		    for (pt = 0; pt < cont->psize; pt++)
			 imodPointSetSize(cont, pt, 
					  imodPointGetSize(obj, cont, pt));
	       imodObjectAddContour(tobj, cont);
	       imodObjectRemoveContour(obj, co);

	       /* DNM 3/29/01: drop back to previous contour and set no 
		  current point */
	       conew = co - 1;
	       ptnew = -1;
	  }
     }
     
     /* DNM 3/29/01: manage the new contour and point numbers appropriately
	given possible change of contour number.  Switch from using 
	imodSetIndex because that function re-attaches to a point. */
     if (conew >= obj->contsize)
	  conew = obj->contsize - 1;
     if (conew < 0 && obj->contsize > 0)
	  conew = 1;
     if (conew > -1) {
	  if (ptnew >= (int)obj->cont[conew].psize)
	       ptnew = obj->cont[conew].psize - 1;
     } else {
	  ptnew = -1;
     }
     App->cvi->imod->cindex.contour = conew;
     App->cvi->imod->cindex.point = ptnew;
     return;
}

void inputContourMoveDialogUpdate(void)
{
     int min, max, val;
     Iobj *obj;

     if (movefirst != 0 || comv.dia == NULL)
	  return;

     obj = imodObjectGet(App->cvi->imod);

     if (comv.movetosurf) {
	  min = 0; 
	  if (obj)  
	       max = obj->surfsize; 
	  else
	       max = 0;
	  if (comv.surf_moveto > max)
	       comv.surf_moveto = max;
	  val = comv.surf_moveto;
     } else {
	  min = 1;
	  max = App->cvi->imod->objsize;
	  if (App->cvi->obj_moveto > max)
	       App->cvi->obj_moveto = max;
	  val = App->cvi->obj_moveto;
     }
     if (max <= min) {
	  val = min;
	  max = min + 1;
	  comv.enabled = 0;
     } else {
	  comv.enabled = 1;
     }
     XtSetSensitive(comv.w2, comv.enabled);

     XtVaSetValues(comv.w2,
		   XmNminimum, min, 
		   XmNmaximum, max, 
		   XmNvalue, val,
		   NULL);
     set_replace_button();
     return;
}

void inputContourMoveDialog(ImodView *vw)
{
     if (movefirst){
	  comv.dia = NULL;
	  comv.store = 0;
	  comv.movetosurf = 0;
	  comv.surf_moveto = 0;
	  comv.replace = 0;
	  comv.enabled = 1;
	  comv.keepsize = 0;
	  movefirst = 0;
     }

     if (comv.dia){
	  XRaiseWindow(App->display, XtWindow(comv.dia->dialog));
	  return;
     }

     comv.vw = vw;

     comv.dia = diaVaCreateDialog
	  ("Imod: Contour Move",
	   App->toplevel, App->context,
	   DiaNcontrolButton, "Done",  quit_cb, (XtPointer)&comv,
	   DiaNcontrolButton, "Apply", moveapply_cb, (XtPointer)&comv,
	   DiaNcontrolButton, "Help",  movehelp_cb, (XtPointer)&comv,
	   DiaNworkAreaFunc, moveworkarea_cb, (XtPointer)&comv,
	   DiaNwindowQuit, quit_cb, (XtPointer)&comv,
	   0);
     inputContourMoveDialogUpdate();
     return;
}
