/*  IMOD VERSION 2.42
 *
 *  imod_cont_copy.c -- Contour copy dialog.
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
Revision 3.2  2002/09/26 21:28:55  rickg
Removed empty string sprintf formats and unused variables.

Revision 3.1  2002/09/13 20:56:01  mast
Changed include of libgen.h to be on sun only

*/

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/Separator.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <stdio.h>
#include <unistd.h>

/* 9/10/02: This include is not known to be unneeded on sun but is generally
   unavailable and unneeded on SGI - so leave it just for sun */
#ifdef __sun
#include <libgen.h>
#endif
#include <math.h>

#include "diaP.h"
#include "imod.h"

#define COPY_TO_OBJECT     0
#define COPY_TO_CURRENT    1
#define COPY_TO_SECTION    2
#define COPY_TO_TIME       3

#define COPY_TO_NEXT_SECTION 18
#define COPY_TO_PREV_SECTION 17
#define COPY_TO_NEXT_TIME    19

static struct
{
  diaDialog     *dia;
  ImodView      *vw;        /* image data to model                       */

  /* Flags */
  int doSurface;    
  int doLabel;
  int doPointLabel;
  int doAll;

  /* Copy to information. */
  int copyOperation;
  int surfaceNumber;
  int objectNumber;
  int sectionNumber;
  int timeIndex;

  /* Copy from information. */
  int   currentTime;
  int   currentSection;    /* DNM 11/30/02: change from float to int */


  /* Widgets used in copy dialog. */
  Widget wCopyOption;
  Widget wToNumber;
  Widget wAllFlag;

  Widget wSurface;
  Widget wSurfaceFlag;

  Widget wLabel;
  Widget wLabelFlag;

  Widget wPointLabel;
  Widget wPointLabelFlag;

  Widget wTimeCopy;

}

ThisDialog = 
  { 
    NULL, NULL , 
    0, 0, 0, 0, 
    0, -1, 0, -1, 0,
  };

static void workarea_cb(Widget w, XtPointer client, XtPointer call);

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
  diaDestroyDialog(ThisDialog.dia);
  ThisDialog.dia = NULL;
  return;
}

static void help_cb(Widget w, XtPointer client, XtPointer call)
{

  dia_vasmsg
    ("Imod Contour Copy Help\n"
     "---------------------------------------------------------------\n",
     "\n",
     "This dialog allows one to copy the current contour, or "
     "selected contours in the current object to a selected section, "
     "object or time index.\n",
     "If copying to a section or time index, only contours matching "
     "the time or section number of the current contour will be copied.\n\n",

     "The Filter settings can be used to filter out contours "
     "if copying all contours in an object and/or points.  "
     "The surface number is used to select contours of a given "
     "surface only.  See the menu item Edit->Contour->Type.  "
     "The label matching strings can contain special characters "
     "for pattern matching.  These characters are '?', '*', and "
     "'\\'.  The '?' charactor is a wildcard character that matches "
     "a single character.  The '*' character is a wildcard character "
     "that matches any number of characters.  The '\\' character is "
     "used to override the special meaning of characters,"
     "ie \\? \\* \\\\\n\n",
     "Select Apply to execute the copy operation using the current "
     "settings.\n",
     "Select Done to close the dialog without doing any "
     "copy operation.\n\n",
     NULL);
  return;
}

/* These two functions support 4D scope data. */
static int contCompare(Icont *c1, Icont *c2)
{
  int dif = 0;
  int pt;

  if ((!c1) || (!c2)) return -1;
  if (c1->type != c2->type) dif++;
  if (c1->surf != c2->surf) dif++;
  if (c1->flags != c2->flags) dif++;
  if (c1->psize != c2->psize) dif++;
  if (dif) return dif;
  dif = 7;
  for(pt = 0; pt < c1->psize; pt++){
	if (c1->pts[pt].x != c2->pts[pt].x) dif++;
	if (c1->pts[pt].y != c2->pts[pt].y) dif++;
	if (c1->pts[pt].z != c2->pts[pt].z) dif++;
  }
  return dif;
}
/* rm points in c2 that are in c1 */
static int contRmDup(Icont *c1, Icont *c2)
{
  int delpts = 0;
  int pt1,pt2;
  if ((!c1)||(!c2)||(!c1->psize)||(!c2->psize)) return delpts;
  for(pt1 = 0; pt1 < c1->psize; pt1++)
	for(pt2 = 0; pt2 < c2->psize; pt2++){
      if ((c1->pts[pt1].x == c2->pts[pt2].x) &&
          (c1->pts[pt1].y == c2->pts[pt2].y) &&
          (c1->pts[pt1].z == c2->pts[pt2].z)){
		imodPointDelete(c2, pt2); 
		delpts++;
		pt2--;
      }
	}
  return delpts;
}

/*
 * Copy a single contour, cont, to the place it needs to go.
 *
 */
static int copyContour(Icont *cont)
{
  Iobj *toObj;
  int co,pt, section;

  if (!cont) return(-1);
  if (!cont->psize) return(-1);

  switch(ThisDialog.copyOperation){

  case COPY_TO_OBJECT:
    toObj = &ThisDialog.vw->imod->obj[ThisDialog.objectNumber - 1];

    /* Don't copy if duplicate contour already exists. */
    for(co = 0; co < toObj->contsize; co++){
      if (contCompare(&toObj->cont[co], cont) == 0)
        return(0);
    }
    /* Remove duplicate points */
    if (iobjScat(toObj->flags)){
      for(co = 0; co < toObj->contsize; co++){
        contRmDup(&toObj->cont[co], cont);
      }
    }
    imodObjectAddContour(toObj, cont);
    break;

  case COPY_TO_CURRENT:
    toObj = imodObjectGet(ThisDialog.vw->imod);
    imodObjectAddContour(toObj, cont);
    break;

  case COPY_TO_SECTION:
  case COPY_TO_NEXT_SECTION:
  case COPY_TO_PREV_SECTION:
    toObj   = imodObjectGet(ThisDialog.vw->imod);
    section = ThisDialog.sectionNumber-1; 
    for(pt = 0; pt < cont->psize; pt++){
      cont->pts[pt].z = section;
    }
    imodObjectAddContour(toObj, cont);
    break;

  case COPY_TO_TIME:
  case COPY_TO_NEXT_TIME:
    toObj   = imodObjectGet(ThisDialog.vw->imod);
    cont->type = ThisDialog.timeIndex;
    imodObjectAddContour(toObj, cont);
    break;
  }
  return(0);
}

static void apply_cb(Widget w, XtPointer client, XtPointer call)
{
  char *badCopy = "\nCopy operation cancelled.\n";
  /*     char *badObjectErrorMsg = 
         "\nCopy operation cancelled.\n"
         "Object out of range or invalid\n.";
  */
  char *tstr = NULL;

  Iobj *obj   = imodObjectGet(ThisDialog.vw->imod);
  Icont *cont = imodContourGet(ThisDialog.vw->imod);
  Icont *ncont;
  int co, pt, errcode;
  char *coval;
  if (!obj){
    wprint("\n%sBad input object.\n",badCopy);
    return;
  }

  /* DNM: check validity of current contour: here test on all the
     conditions where a current contour is not needed */
  if (!(ThisDialog.doAll && (ThisDialog.copyOperation == COPY_TO_OBJECT ||
                             ThisDialog.copyOperation == COPY_TO_CURRENT))){
    if ((!cont) || (cont->psize <= 0)){
      wprint("\n%sBad input contour.\n", badCopy);
      return;
    }
  }

  /* check copy to place is valid. */
  switch(ThisDialog.copyOperation){
  case COPY_TO_OBJECT:
    coval = XmTextGetString(ThisDialog.wToNumber);
    if (coval){
      ThisDialog.objectNumber = atoi(coval);
      free(coval);
    }else{
      ThisDialog.objectNumber = 0;
    }
    if ((ThisDialog.objectNumber < 1) ||
        (ThisDialog.objectNumber > ThisDialog.vw->imod->objsize)){
      wprint("%sBad destination object.\n", badCopy);
      return;
    }
	  
    break;

  case COPY_TO_CURRENT:
    break;

  case COPY_TO_SECTION:
    /* get section number to copy from.*/
    ThisDialog.currentSection = (int)floor(cont->pts->z + 0.5);

    /* get section number to copy to. */
    coval = XmTextGetString(ThisDialog.wToNumber);
    if (coval){
      ThisDialog.sectionNumber = atoi(coval);
      free(coval);
    }else{
      ThisDialog.sectionNumber = 0;
    }
    if ((ThisDialog.sectionNumber <= 0) || 
        ( ThisDialog.sectionNumber > ThisDialog.vw->zsize )){
      wprint("%sBad destination section.\n", badCopy);
      return;
    }
    break;

  case COPY_TO_TIME:
    ThisDialog.currentTime = cont->type;
    coval = XmTextGetString(ThisDialog.wToNumber);
    if (coval){
      ThisDialog.timeIndex = atoi(coval);
      free(coval);
    }else{
      ThisDialog.timeIndex = -1;
    }
    if ((ThisDialog.timeIndex > ThisDialog.vw->nt) ||
        ( ThisDialog.timeIndex < 0))
      {
        wprint("%sBad destination time index.\n", badCopy);
        return;
      }
    break;

    /* DNM 2/16/01: made these work relative to section of current
       contour */
  case COPY_TO_NEXT_SECTION:
    ThisDialog.currentSection = (int)floor(cont->pts->z + 0.5);
    if (ThisDialog.currentSection == (ThisDialog.vw->zsize - 1)){
      wprint("%sNext section invalid.\n", badCopy);
      return;
    }
    ThisDialog.sectionNumber = ThisDialog.currentSection + 2;
    break;

  case COPY_TO_PREV_SECTION:
    ThisDialog.currentSection = (int)floor(cont->pts->z + 0.5);
    if (!ThisDialog.currentSection){
      wprint("%sPrevious section invalid.\n", badCopy);
      return;
    }
    ThisDialog.sectionNumber = ThisDialog.currentSection;
    break;

  }

  if (!ThisDialog.doAll){
    ncont = imodContourDup(cont);
    copyContour(ncont);
  }else{
    int maxcont = obj->contsize;
    /* copy all contours in current object */
    for(co = 0; co < maxcont; co++){
      cont = &obj->cont[co];
      if (ThisDialog.copyOperation == COPY_TO_SECTION ||
          ThisDialog.copyOperation == COPY_TO_NEXT_SECTION ||
          ThisDialog.copyOperation == COPY_TO_PREV_SECTION){
        if (!cont->psize) continue;
        if (floor(cont->pts->z + 0.5) != ThisDialog.currentSection)
          continue;
      }
      if ((ThisDialog.copyOperation == COPY_TO_TIME) &&
          (cont->type != ThisDialog.currentTime))
        continue;
      if (ThisDialog.doSurface){
        if (cont->surf != ThisDialog.surfaceNumber)
          continue;
      }
      if (ThisDialog.doLabel){
        /* check for match */
        if (!cont->label) continue;
        tstr = XmTextGetString(ThisDialog.wPointLabel);
        if (!tstr) break;
        if (!imodLabelMatch(cont->label, tstr)){
          free(tstr);
          continue;
        }
        free(tstr);
      }
	       
      if (ThisDialog.doPointLabel){
        if (!cont->label) continue;
        tstr = XmTextGetString(ThisDialog.wPointLabel);
        if (tstr){
          int maxp = cont->psize;
          char *cplabel;

          /* copy points that match only. */
          ncont = imodContourNew();
          *ncont = *cont;
          ncont->psize = 0;
          ncont->label = NULL;
          ncont->pts   = NULL;

          for(pt = 0; pt < maxp; pt++){
            cplabel = imodLabelItemGet(cont->label, pt);
            if (!cplabel) continue;
			      
            if (ilabelMatchReg(tstr, cplabel)){
              imodPointAppend(ncont, &cont->pts[pt]);
              if (!ncont->label)
                ncont->label = imodLabelNew();
              imodLabelItemAdd(ncont->label, 
                               cplabel,
                               ncont->psize - 1);
            }
          }
          copyContour(ncont);
          free(tstr);
          continue;
        }
        continue;
      }
	       
      /* copy the entire contour */
      if (cont->psize){
        ncont  = imodContourDup(cont);
        errcode = copyContour(ncont);
      }
    }
  }
  wprint("Copy operation completed\n");
  imodDraw(ThisDialog.vw, IMOD_DRAW_MOD);
  imod_setxyzmouse();
}


int openContourCopyDialog(ImodView *vw)
{
     
  if (ThisDialog.dia){
    XRaiseWindow(XtDisplay(ThisDialog.dia->dialog), 
                 XtWindow(ThisDialog.dia->dialog));
    return(0);
  }
     
  ThisDialog.vw = vw;
  ThisDialog.dia = diaVaCreateDialog
    ("Imod: Copy Contour Data Dialog",
     App->toplevel, App->context,

     DiaNcontrolButton, "Done",
     quit_cb, (XtPointer)&ThisDialog,

     DiaNcontrolButton, "Apply",
     apply_cb, (XtPointer)&ThisDialog,

     DiaNcontrolButton, "Help",
     help_cb, (XtPointer)&ThisDialog,quit_cb, (XtPointer)&ThisDialog,

     DiaNworkAreaFunc, workarea_cb, (XtPointer)&ThisDialog,
     DiaNwindowQuit, quit_cb, (XtPointer)&ThisDialog,
     0);

  if (!ThisDialog.dia) return(1);
  return(0);
}

/****************************************************************************/

static void setwidgets(void)
{
  char string[32];

  switch(ThisDialog.copyOperation){
  case COPY_TO_OBJECT:
    if (ThisDialog.objectNumber > 0)
      sprintf(string, "%d", ThisDialog.objectNumber);
    else
      string[0] = '\0';
    XtSetSensitive(ThisDialog.wToNumber, True);
    break;
  case COPY_TO_CURRENT:
    sprintf(string, "Current");
    XtSetSensitive(ThisDialog.wToNumber, False);
    break;
  case COPY_TO_SECTION:
    if (ThisDialog.sectionNumber > 0)
      sprintf(string, "%d", ThisDialog.sectionNumber);
    else
      string[0] = '\0';
    XtSetSensitive(ThisDialog.wToNumber, True);
    break;
  case COPY_TO_TIME:
    if (ThisDialog.timeIndex)
      sprintf(string, "%d", ThisDialog.timeIndex);
    else
      string[0] = '\0';
    XtSetSensitive(ThisDialog.wToNumber, True);
    break;
  case COPY_TO_NEXT_SECTION:
  case COPY_TO_PREV_SECTION:
  default:
    string[0] = '\0';
    XtSetSensitive(ThisDialog.wToNumber, False);
    break;
  }
  XmTextSetString(ThisDialog.wToNumber, string);

  XmToggleButtonSetState
    (ThisDialog.wAllFlag,
     (ThisDialog.doAll) ? True : False,
     False);

  XmToggleButtonSetState
    (ThisDialog.wSurfaceFlag,
     (ThisDialog.doSurface) ? True : False,
     False);

  XmToggleButtonSetState
    (ThisDialog.wLabelFlag,
     (ThisDialog.doLabel) ? True : False,
     False);

  XmToggleButtonSetState
    (ThisDialog.wPointLabelFlag,
     (ThisDialog.doPointLabel) ? True : False,
     False);

  if (ThisDialog.surfaceNumber >= 0){
    sprintf(string, "%d", ThisDialog.surfaceNumber);
    XmTextSetString(ThisDialog.wSurface, string);
  }

  if (ThisDialog.doAll){
    XtSetSensitive(ThisDialog.wSurface, True);
    XtSetSensitive(ThisDialog.wSurfaceFlag, True);
    XtSetSensitive(ThisDialog.wLabel, True);
    XtSetSensitive(ThisDialog.wLabelFlag, True);
    XtSetSensitive(ThisDialog.wPointLabel, True);
    XtSetSensitive(ThisDialog.wPointLabelFlag, True);
  }else{
    XtSetSensitive(ThisDialog.wSurface, False);
    XtSetSensitive(ThisDialog.wSurfaceFlag, False);
    XtSetSensitive(ThisDialog.wLabel, False);
    XtSetSensitive(ThisDialog.wLabelFlag, False);
    XtSetSensitive(ThisDialog.wPointLabel, False);
    XtSetSensitive(ThisDialog.wPointLabelFlag, False);
  }
     
}


static void flag_cb(Widget w, XtPointer client, XtPointer call)
{
  int *flag;

  if (client){
    flag = (int *)client;
    if (*flag)
      *flag = False;
    else
      *flag = True;
  }
  setwidgets();
}

static void objnum_cb(Widget w, XtPointer client, XtPointer call)
{
  char *string;
     
  string = XmTextGetString(ThisDialog.wToNumber);

  if (string){
    switch(ThisDialog.copyOperation){
    case COPY_TO_OBJECT:
      ThisDialog.objectNumber = atoi(string);
      break;
    case COPY_TO_SECTION:
      ThisDialog.sectionNumber = atoi(string);
      break;
    case COPY_TO_TIME:
      ThisDialog.timeIndex = atoi(string);
      break;
    }
    free(string);
    if (!client)
      setwidgets();
  }
  return;
}  

static void surface_cb(Widget w, XtPointer client, XtPointer call)
{
  char *string;

  string = XmTextGetString(ThisDialog.wSurface);
  if (string)
    if (strlen(string) > 0){
      ThisDialog.surfaceNumber = atoi(string);
      free(string);
      setwidgets();
      return;
    }
  ThisDialog.surfaceNumber = -1;
  return;
}

static void setOption_cb(Widget w, XtPointer client, XtPointer call)
{
  ThisDialog.copyOperation = (int) client;
  setwidgets();
}
      

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
  Widget row, rowcol, grid;
  Widget pulldown, pb, pb1;
  Arg             args[4];
  Cardinal        n = 0;
     
  rowcol = XtVaCreateWidget
    ("rowcol", xmRowColumnWidgetClass, w, NULL);

  row = XtVaCreateWidget
    ("rowcol", xmRowColumnWidgetClass, rowcol, 
     XmNorientation, XmHORIZONTAL,
     NULL);

  /*
   *  Make pull down menu for destination.
   */
  XtSetArg(args[n], XmNvisual, App->visual); n++;
  pulldown = XmCreatePulldownMenu
    (row, "option", args, n);
  pb1 = XtVaCreateManagedWidget
    ("Copy to Object #", xmPushButtonWidgetClass, pulldown, NULL);
  XtAddCallback(pb1, XmNactivateCallback,
                setOption_cb, COPY_TO_OBJECT);
  pb = XtVaCreateManagedWidget
    ("Copy to Section #", xmPushButtonWidgetClass, pulldown, NULL);
  XtAddCallback(pb, XmNactivateCallback,
                setOption_cb, (XtPointer)COPY_TO_SECTION);

  pb = XtVaCreateManagedWidget
    ("Copy to Next Section", xmPushButtonWidgetClass, pulldown, NULL);
  XtAddCallback(pb, XmNactivateCallback,
                setOption_cb, (XtPointer)COPY_TO_NEXT_SECTION);

  pb = XtVaCreateManagedWidget
    ("Copy to Prev Section", xmPushButtonWidgetClass, pulldown, NULL);
  XtAddCallback(pb, XmNactivateCallback,
                setOption_cb, (XtPointer)COPY_TO_PREV_SECTION);

  pb = XtVaCreateManagedWidget
    ("Duplicate", xmPushButtonWidgetClass, pulldown, NULL);
  XtAddCallback(pb, XmNactivateCallback,
                setOption_cb, (XtPointer)COPY_TO_CURRENT);
  pb = XtVaCreateManagedWidget
    ("Copy to Time Index #", xmPushButtonWidgetClass, pulldown, NULL);
  XtAddCallback(pb, XmNactivateCallback,
                setOption_cb, (XtPointer)COPY_TO_TIME);
  if (!ThisDialog.vw->nt){
    XtSetSensitive(pb, False);
  }
  XtSetArg(args[n], XmNsubMenuId, pulldown); n++;
  XtSetArg(args[n], XmNmenuHistory, pb1);    n++;
  ThisDialog.wCopyOption =  XmCreateOptionMenu(row, "option", args, n);
  XtManageChild(ThisDialog.wCopyOption);

  ThisDialog.wToNumber = XtVaCreateManagedWidget
    ("text", xmTextWidgetClass, row, NULL);
  XtAddCallback(ThisDialog.wToNumber, XmNactivateCallback,
                objnum_cb, NULL);
  XtAddCallback(ThisDialog.wToNumber, XmNlosePrimaryCallback,
                objnum_cb, (XtPointer)1);

  XtManageChild(row);


  /*  
   *  Make copy all contour in current object button.
   */
  ThisDialog.wAllFlag = XtVaCreateManagedWidget
    ("Apply to all Contours in current Object",  
     xmToggleButtonWidgetClass, rowcol, NULL);
  XtAddCallback(ThisDialog.wAllFlag, XmNvalueChangedCallback,
                flag_cb, (XtPointer)&ThisDialog.doAll);


  /*
   *  Make contour filter gadgets.
   */
  XtVaCreateManagedWidget
    ("separator", xmSeparatorWidgetClass, rowcol, NULL);
  XtVaCreateManagedWidget
    ("Filters:", xmLabelWidgetClass, rowcol, NULL);

  grid = XtVaCreateWidget
    ("rowcol", xmRowColumnWidgetClass, rowcol, 
     XmNpacking, XmPACK_COLUMN,
     XmNnumColumns, 2,
     XmNorientation, XmVERTICAL,
     NULL);
  ThisDialog.wSurfaceFlag = XtVaCreateManagedWidget
    ("Surface #",  xmToggleButtonWidgetClass, grid, NULL);
  XtAddCallback(ThisDialog.wSurfaceFlag, XmNvalueChangedCallback,
                flag_cb, (XtPointer)&ThisDialog.doSurface);
  ThisDialog.wLabelFlag = XtVaCreateManagedWidget
    ("Match Contour Label",  xmToggleButtonWidgetClass, grid, NULL);
  XtAddCallback(ThisDialog.wLabelFlag, XmNvalueChangedCallback,
                flag_cb, (XtPointer)&ThisDialog.doLabel);
  ThisDialog.wPointLabelFlag = XtVaCreateManagedWidget
    ("Match Point Label",  xmToggleButtonWidgetClass, grid, NULL);
  XtAddCallback(ThisDialog.wPointLabelFlag, XmNvalueChangedCallback,
                flag_cb, (XtPointer)&ThisDialog.doPointLabel);

  ThisDialog.wSurface = XtVaCreateManagedWidget
    ("text", xmTextWidgetClass, grid,  NULL);
  XtAddCallback(ThisDialog.wSurface, XmNactivateCallback, 
                surface_cb, NULL);
  ThisDialog.wLabel = XtVaCreateManagedWidget
    ("text", xmTextWidgetClass, grid,  NULL);
  ThisDialog.wPointLabel = XtVaCreateManagedWidget
    ("text", xmTextWidgetClass, grid,  NULL);
  XtManageChild(grid);


  /* all done making widgets.*/
  XtManageChild(rowcol);
  setwidgets();
  return;
}
