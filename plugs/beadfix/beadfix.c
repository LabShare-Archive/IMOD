/*
 *  beadfix.c -- Special plugin for fixing fiducial models
 *
 */

/*****************************************************************************
 *   Copyright (C) 1997-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

/* include basic X-Window, Motif and OpenGL headers.
 */
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>

#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <imodel.h>
#include <dia.h>
#include "imodplug.h"

#define MAXLINE 100

typedef struct {
     int obj;
     int cont;
     int view;
} residpt;

/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
     ImodView    *view;
     Widget       window;
     Widget       reread;
     Widget       nextres;
     Widget       nextlocal;
     Widget       movepoint;
     Widget       undomove;
     Widget       clearlist;

     char   *filename;
     FILE   *fp;
     int    ifdidgap;
     int    residok;
     int    xsize, ysize, zsize;
     int    lastob, lastco, lastpt, lastbefore;
     int    objcont;
     float  xresid, yresid;                /* last residual looked at */
     int    objlook, contlook, ptlook;     /* obj, cont, pt of that residual */
     int    curmoved;                      /* flag whether it has been moved */
     int    objmoved, contmoved, ptmoved;  /* obj, cont, pt of moved point */
     int    didmove;                       /* flag that a point was moved */
     Ipoint oldpt, newpt;                  /* old and new positions */
     int    lookonce;
     residpt *lookedlist;                  /* List of points examined */
     int    listsize;                      /* Number of items on list */
     int    listmax;                       /* Size allocated for list */
     
}PlugData;


static void nextgap_cb(Widget w, XtPointer client, XtPointer call);

static void open_cb(Widget w, XtPointer client, XtPointer call);

static void reread_cb(Widget w, XtPointer client, XtPointer call);

static void nextres_cb(Widget w, XtPointer client, XtPointer call);

static void movepoint_cb(Widget w, XtPointer client, XtPointer call);

static void undomove_cb(Widget w, XtPointer client, XtPointer call);

static PlugData thisPlug = { 0, 0 };

static void makeWorkArea(Widget parent);

/*
 * Called by the imod plugin load function. 
 */
char *imodPlugInfo(int *type)
{
    if (type)
	*type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS;
    return("Bead Fixer");
}

static void help_cb(Widget w, XtPointer client, XtPointer call)
{
    dia_vasmsg("Bead Fixer Plugin Help\n\n",
	       "This Plugin makes it easier to fix tilt series fiducial "
	       "models in two ways.  It has a function for finding the next "
	       "gap or untracked place in the model and making that be the "
	       "current point.  It also will read the log file from running "
	       "Tiltalign and move to points with high residuals.\n\n"
	       "Select the Go to Next Gap button or hit the space bar key to "
	       "move to the next gap or incomplete place in the model.  If "
	       "there is a gap, the point BEFORE the gap will be displayed.  "
	       "If a contour does not start on the first section, the first "
	       "point in the contour (which is AFTER the gap) will be "
	       "displayed and a message will be printed.  The program "
	       "searches forward from the current point except the first time "
	       "when the function is selected.  To begin the search at the "
	       "beginning of the model again, detach from the current point "
	       "by clicking the left mouse button far from any model point, "
	       "then select the Next Gap function.\n\n"
	       "Select the Open Tiltalign Log File button to open a log "
	       "file.\n\n"
	       "Select the Go to Next Big Residual button or hit the "
	       "apostrophe key to move to the next point in the log file "
	       "with a big residual.\n\n"
	       "The X and Y displacements implied by the residual are printed "
	       "in the Info window.  If these displacements seems to match "
	       "the amounts that the point should be moved in X and Y to "
	       "match the bead in the image, then select the Move Point by "
	       "Residual button or push the semicolon key to move the point "
	       "by those amounts in X and Y.\n\n"
	       "After moving a point by its residual, select the Undo Move "
	       "button or push the U key to undo the move.\n\n"
	       "Select the Reread Log File button to reread the file, or a "
	       "new version of it after rerunning Tiltalign.\n\n",
	       "Select the Go to Next Local Set button if you have a log file "
	       "with a series of local alignments and want to skip to the "
	       "residuals from the next local alignment.\n\n",
	       "The program keeps a list of each point whose residual has "
	       "been examined.  If the Examine Points Once toggle is "
	       "selected, then any point already on the list is skipped over "
	       "when you Go to Next Big Residual.  A point is added to the "
	       "list regardless of whether Examine Points Once is selected."
	       "\n\n"
	       "Select the Clear Examined List button to empty the list of "
	       "points that have been examined.  The list is not cleared when "
	       "Examine Points Once is switched on or off.  To see some "
	       "points for the second time, turn off Examine Points Once; to "
	       "see all points again, push Clear Examined List.\n",
	       NULL);
}

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = (PlugData *)client;

     plug->view = NULL;
     XtPopdown(plug->window);
     XtDestroyWidget(plug->window);
     plug->window = 0;
     if (plug->listmax)
	  free(plug->lookedlist);
     plug->listmax = 0;
}

/*
 *  Grab hotkey input. return 1 if we handle the key.
 */
int imodPlugKeys(ImodView *vw, XKeyEvent *event)
{
    PlugData *plug = &thisPlug;
    KeySym keysym;
    int    keyhandled = 0;
    int    ctrl;
    int    shift;

    /*
     * Don't grab keys if plug window isn't open.
     */
    if (!plug->view)
	return keyhandled;
    
    /* The keysym values are XK_a...XK_z and 
     * XK_space, XK_comma
     */
    keysym = XLookupKeysym(event, 0);

    /*
     * Modifier key mask.  Set ctrl and shift to true
     * if the coresponding key is pressed.
     */
    ctrl   = event->state & ControlMask;
    shift  = event->state & ShiftMask;
    
    
    switch(keysym){
      case XK_apostrophe: 
        nextres_cb(NULL, NULL, NULL);
	keyhandled = 1;
	break;
      case XK_space:
        nextgap_cb(NULL, NULL, NULL);
	keyhandled = 1;
	break;
       case XK_semicolon:
	 movepoint_cb(NULL, NULL, NULL);
	 keyhandled = 1;
	 break;
       case XK_u:
	 undomove_cb(NULL, NULL, NULL);
	 keyhandled = 1;
	 break;
    }
    return keyhandled;
}

/*
 *  Execute any function or program you wish with this function.
 *  Here we open up a window for user interaction.
 *  see imodplug.h for a list of support functions.
 */

void imodPlugExecute(ImodView *inImodView)
{
     Atom     wmclose;
     Widget   form;
     PlugData *plug;

     plug = &thisPlug;

     if (plug->view){
	 wprint("Bead Fixer already open.\n");
	 /* 
	  * Bring the window to the front if already open.
	  */
	 if (plug->window) XtPopup(plug->window, XtGrabNone);
	 return;
     }

     plug->view = inImodView;
     ivwGetImageSize(inImodView, &plug->xsize, &plug->ysize, &plug->zsize);

     /* 
      * Initialize data. 
      */
     plug->filename = NULL;
     plug->fp = NULL;
     plug->ifdidgap = 0;
     plug->residok = 0;
     plug->lastob = -1;
     plug->curmoved = 0;
     plug->objlook = -1;
     plug->didmove = 0;
     plug->lookonce = 0;
     plug->listmax = 0;
     plug->listsize = 0;

     /*
      * This creates the plug window.
      */
     plug->window  = XtVaCreatePopupShell
	  ("Bead Fixer", topLevelShellWidgetClass, imodTopLevel(),
	   XmNvisual, imodVisual(),
	   XtNtitle, imodPlugInfo(0),
	   NULL);

     /* Make window controls. */
     makeWorkArea(plug->window);

     /* Set up the quit function for the window. */
     wmclose = XmInternAtom( imodDisplay(),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(plug->window, wmclose, quit_cb,
			     (caddr_t)plug);

     /* Open up the window. */
     XtPopup(plug->window, XtGrabNone);
}


/* Open a tiltalign log file to find points with big residuals */

static void open_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = &thisPlug;

    if(plug->filename != NULL) free(plug->filename);
      
    plug->filename = dia_filename("Enter name of tiltalign log file");

    if(plug->filename != NULL) reread_cb(NULL, (XtPointer)0, NULL);
    else {
      wprint("Unusable file name\n");
      return;
    }

    if(plug->fp != NULL)  XtSetSensitive(plug->reread, True);    
    if(plug->fp != NULL)  XtSetSensitive(plug->nextlocal, True);    

    return;
}


/* Read or reread the tiltalign log file whose name was already obtained */

static void reread_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = &thisPlug;

    char line[MAXLINE];
    char *arealine;
    int skipopen = (int)client;
    int newstyle, oldstyle = 0;
    int found = 0;

    if(plug->filename == NULL) return;
    if (!skipopen) {
	 if(plug->fp != NULL) fclose(plug->fp);
	 plug->fp = fopen(plug->filename, "r");
	 wprint("Rereading file.\n");
    } else {
	 while(!found && fgets(line, MAXLINE, plug->fp) != NULL) {
	      arealine = strstr(line,"Doing local area");
	      if (arealine) {
		   arealine[22]=0x00;
		   wprint("Skipping to next local set, area%s.\n",
			  &arealine[16]);
		   found = 1;
	      }
	 }
	 if (!found) {
	      wprint("No more local sets.\n");
	      return;
	 }
    }

    if(plug->fp == NULL) {
	 wprint("Error opening file!\n");
	 return;
    }
    
    while(fgets(line, MAXLINE, plug->fp) != NULL) {
	 newstyle = strstr(line,"   #     #     #      X         Y        X")
	      != NULL;
	 if (!newstyle)
	      oldstyle = strstr(line,"   #     #      X         Y        X")
		   != NULL;
	 if (newstyle || oldstyle) {
	      plug->objcont = newstyle;
	      XtSetSensitive(plug->nextres, True);    
	      XtSetSensitive(plug->nextlocal, True);    
	      plug->residok = 1;
	      return;
	 }
    }

    wprint("Residual data not found\n");
    XtSetSensitive(plug->nextres, False);    
    XtSetSensitive(plug->nextlocal, False);    
    plug->residok = 0;
    return;
}


/* Jump to the next point with a big residual */

static void nextres_cb(Widget w, XtPointer client, XtPointer call)
{
    char line[MAXLINE];
    char *getres;
    int inobj, incont, inpt, inview, curpt, obj, nobj, cont, ncont, ipt, npnt;
    int obsav, cosav, ptsav, i;
    int found = 0;
    float xc, yc, xr, yr, sd, resval;
    Iobj *ob;
    Icont *con;
    Ipoint *pts;

    PlugData *plug = &thisPlug;
    Imod *theModel = ivwGetModel(plug->view);
    ivwControlActive(plug->view, 0);

    if(plug->fp == NULL || plug->residok == 0) return;

    do {
	 getres = fgets(line, MAXLINE, plug->fp);
	 if(getres == NULL || strlen(line) <3) {
	      wprint("No more residuals in this list\n");
	      XtSetSensitive(plug->nextres, False);    
	      plug->residok=0;
	      return;
	 }
	 if (plug->objcont)
	      sscanf(line, "%d %d %d %f %f %f %f %f", 
		     &inobj, &incont, &inview, &xc, &yc, &xr, &yr, &sd);

	 else {
	      sscanf(line, "%d %d %f %f %f %f %f", 
		     &inpt, &inview, &xc, &yc, &xr, &yr, &sd);
	      inobj = 1;
	      incont = inpt;
	 }

	 /* See if point is on list */
	 found = 0;
	 for (i = 0; i < plug->listsize && !found; i++)
	      if (inobj == plug->lookedlist[i].obj && 
		  incont == plug->lookedlist[i].cont
		  && inview == plug->lookedlist[i].view)
		   found = 1;

	 /* Continue with next point if looking once and this point was found
	    on the list */
    } while (plug->lookonce && found);

    /* Add point to list if it wasn't found */
    if (!found) {
	 if (plug->listsize >= plug->listmax) {
	      if (plug->listmax)
		   plug->lookedlist = (residpt *)realloc
			(plug->lookedlist, (plug->listmax + 100) * 
			 sizeof(residpt));
	      else
		   plug->lookedlist = (residpt *)malloc(100 * sizeof(residpt));
	      plug->listmax += 100;
	 }
	 plug->lookedlist[plug->listsize].obj = inobj;
	 plug->lookedlist[plug->listsize].cont = incont;
	 plug->lookedlist[plug->listsize++].view = inview;
    }

    found = 0;
    curpt=0;
    nobj = imodGetMaxObject(theModel); 
    imodGetIndex(theModel, &obsav, &cosav, &ptsav);

    if (plug->objcont) {

	 /* New case of direct object-contour listing */
	 if (inobj > nobj) {
	      wprint("\aObject not found!\n");
	      return;
	 }
	 obj = inobj - 1;
	 cont = incont - 1;
	 imodSetIndex(theModel, obj, cont, 0);
	 ob = imodObjectGet(theModel);
	 ncont = imodObjectGetMaxContour(ob);
	 if (incont <= ncont) {
	      found = 1;
	      con = imodContourGet(theModel);
	      npnt = imodContourGetMaxPoint(con);
	 }
    } else {

	 /* Old case of "point #", need to count through valid contours */
	 ob = imodObjectGetFirst(theModel);

	 for (obj=0; obj < nobj ; obj++) {
	      ncont = imodObjectGetMaxContour(ob);
	      con = imodContourGetFirst(theModel);
	      for (cont = 0; cont < ncont; cont++)  {
		   npnt = imodContourGetMaxPoint(con);
		   if (npnt > 1) curpt++;
		   if(curpt == inpt) {
			found = 1;
			break;
		   }
		   con = imodContourGetNext(theModel);
	      }
	      if (found)
		   break;
	      ob = imodObjectGetNext(theModel);
	 }
    }

    if (!found || !con) {
	 wprint("\aContour not found!\n");
	 imodSetIndex(theModel, obsav, cosav, ptsav);
	 return;
    }
    pts = imodContourGetPoints(con);
    for (ipt = 0; ipt < npnt; ipt++) {
	 if(floor((double)(pts++->z + 1.5f)) == inview) {
	      imodSetIndex(theModel, obj, cont, ipt);
	      ivwRedraw(plug->view);
	      resval = sqrt((double)(xr*xr + yr*yr));
	      sprintf(line, "Residual =%6.2f (%5.1f,%5.1f),%5.2f SDs\n",
		      resval, xr, yr, sd);
	      wprint(line);
	      plug->xresid = xr;
	      plug->yresid = yr;
	      plug->objlook = obj;
	      plug->contlook = cont;
	      plug->ptlook = ipt;
	      plug->curmoved = 0;
	      XtSetSensitive(plug->movepoint, True);
	      return;
	 }
    }
    wprint("\aPoint not found in contour!\n");
    imodSetIndex(theModel, obsav, cosav, ptsav);
    return;
}

static void lookonce_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     PlugData *plug = &thisPlug;
     plug->lookonce = cbs->set;
}

static void clearlist_cb(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = &thisPlug;
     plug->listsize = 0;
}

static void movepoint_cb(Widget w, XtPointer client, XtPointer call)
{
     int obj, cont, pt;
     PlugData *plug = &thisPlug;
     Imod *theModel = ivwGetModel(plug->view);
     ivwControlActive(plug->view, 0);
     
     if(plug->fp == NULL || plug->curmoved  || plug->objlook < 0) 
	  return;
    imodGetIndex(theModel, &obj, &cont, &pt);
    if (obj != plug->objlook || cont != plug->contlook || pt != plug->ptlook) {
	 wprint("\aThe current point is not the same as the point with the "
		"last residual examined!\n");
	 return;
    }

    /* move the point */
    plug->oldpt = theModel->obj[obj].cont[cont].pts[pt];
    plug->newpt = plug->oldpt;
    plug->newpt.x += plug->xresid;
    plug->newpt.y += plug->yresid;
    theModel->obj[obj].cont[cont].pts[pt] = plug->newpt;
    plug->objmoved = plug->objlook;
    plug->contmoved = plug->contlook;
    plug->ptmoved = plug->ptlook;

    /* set flags and buttons */
    plug->curmoved = 1;
    plug->didmove = 1;
    XtSetSensitive(plug->movepoint, False);
    XtSetSensitive(plug->undomove, True);
    ivwRedraw(plug->view);
}

static void undomove_cb(Widget w, XtPointer client, XtPointer call)
{
     int obsav, cosav, ptsav;
     int obj, cont, pt;
     int nobj, ncont;
     Iobj *ob;
     Icont *con;
     float dx, dy, distsq;
     PlugData *plug = &thisPlug;
     Imod *theModel = ivwGetModel(plug->view);
     ivwControlActive(plug->view, 0);
     
     if(plug->fp == NULL || !plug->didmove) 
	  return;
    imodGetIndex(theModel, &obsav, &cosav, &ptsav);

    nobj = imodGetMaxObject(theModel); 

    if (plug->objmoved < nobj) {
	 imodSetIndex(theModel, plug->objmoved, plug->contmoved, 
		      plug->ptmoved);
	 ob = imodObjectGet(theModel);
	 ncont = imodObjectGetMaxContour(ob);
	 if (plug->contmoved < ncont) {
	      con = imodContourGet(theModel);
	      if (plug->ptmoved < con->psize) {

		   /* Check that point is within 10 pixels of where it was */
		   dx = con->pts[plug->ptmoved].x - plug->newpt.x;
		   dy = con->pts[plug->ptmoved].y - plug->newpt.y;
		   distsq = dx * dx + dy * dy;
		   if (distsq < 100. && con->pts[plug->ptmoved].z ==
		       plug->newpt.z) {
			con->pts[plug->ptmoved] = plug->oldpt;
			plug->didmove = 0;
			plug->curmoved = 0;
			XtSetSensitive(plug->undomove, False);
			XtSetSensitive(plug->movepoint, True);
			ivwRedraw(plug->view);
			return;
		   }	
	      }
	 }
    }
    
    wprint("\aMoved point no longer exists or is not close enough "
	   "to where it was moved to!\n");
    imodSetIndex(theModel, obsav, cosav, ptsav);
    XtSetSensitive(plug->undomove, False);
}

static int foundgap(int obj, int cont, int ipt, int before)
{
    PlugData *plug = &thisPlug;
    Imod *theModel = ivwGetModel(plug->view);

    if(plug->lastob == obj && plug->lastco == cont && plug->lastpt == ipt
       && plug->lastbefore == before)
      return 1;

    plug->lastob = obj;
    plug->lastco = cont;
    plug->lastpt = ipt;
    plug->lastbefore = before;
    imodSetIndex(theModel, obj, cont, ipt);
    ivwRedraw(plug->view);
    return 0;
  }


/* Jump to next gap in the model, or place where it is not tracked to first
   or last section */

static void nextgap_cb(Widget w, XtPointer client, XtPointer call)
{
    int  obj, nobj, cont, ncont, ipt, npnt;
    int obsav, cosav, ptsav, curob, curco, curpt, lookback;
    int iptmin, iptmax, iztst, ipt2, foundnext;
    float zcur, zmin, zmax;
    Iobj *ob;
    Icont *con;
    Ipoint *pts;
    char line[MAXLINE];

    PlugData *plug = &thisPlug;
    Imod *theModel = ivwGetModel(plug->view);

    /* This is needed to make button press behave just like hotkey in syncing
       the image */
    ivwControlActive(plug->view, 0);

    con = imodContourGet(theModel);
    imodGetIndex(theModel, &obsav, &cosav, &ptsav);

    curob = obsav;
    curco = cosav;
    curpt = ptsav;
    lookback = 0;

    if(!con || plug->ifdidgap == 0) {
      curob = curco = curpt = 0;
      plug->lastob = -1;
      plug->lastbefore = 0;
      lookback = 1;
    }

    plug->ifdidgap = 1;

    /* If last one was at start of track, go back to first point of contour */
    if (plug->lastbefore)
	 curpt = 0;

    imodSetIndex(theModel, curob, curco, curpt);
    nobj = imodGetMaxObject(theModel); 

    ob = imodObjectGet(theModel);
    con = imodContourGet(theModel);

    for (obj=curob; obj < nobj ; obj++) {
      ncont = imodObjectGetMaxContour(ob);
      for (cont = curco; cont < ncont; cont++)  {
	npnt = imodContourGetMaxPoint(con);
	if(npnt > 0) {
	  pts = imodContourGetPoints(con);

	  /* find min and max z in contour */
	  zmin = pts->z;
	  iptmin = 0;
	  zmax = zmin;
	  iptmax = 0;
	  for (ipt = 0; ipt < npnt; ipt++) {
	       if (zmin > pts[ipt].z) {
		    zmin = pts[ipt].z;
		    iptmin = ipt;
	       }
	       if (zmax < pts[ipt].z) {
		    zmax = pts[ipt].z;
		    iptmax = ipt;
	       }
	  }

	  /* If looking back, check zmin, set it as gap before if not 0 */
	  if(lookback == 1 && zmin > 0.5) {
	      if(foundgap(obj,cont,iptmin, 1) == 0) {
		sprintf(line, "\aContour %d is incomplete at start of stack\n",
			cont+1);
		wprint(line);
		return;
	      }
	  }

	  /* from current point forward, check for existence of a point at 
	     next z value; if none, it's a gap */
	  for (ipt = curpt; ipt < npnt; ipt++) {
	    if (ipt != iptmax) {
		 zcur = pts[ipt].z;
		 iztst = zcur + 1.5;
		 foundnext = 0;
		 for (ipt2 = 0; ipt2 < npnt; ipt2++) {
		      if (iztst == (int)(pts[ipt2].z + 0.5)) {
			   foundnext = 1;
			   break;
		      }
		 }
		 if (!foundnext)
		      if(foundgap(obj, cont, ipt, 0) == 0) return;
	    }
	  }

	  /* If get to end of contour, check zmax against z of file */
	  if(zmax + 1.1f < plug->zsize) {
	    if(foundgap(obj, cont, iptmax, 0) == 0) return;
	  }
	}
	con = imodContourGetNext(theModel);
	lookback = 1;
	curpt = 0;
      }
      ob = imodObjectGetNext(theModel);
      curco = 0;
    }
    wprint("\aNo more gaps found!\n");
    imodSetIndex(theModel, obsav, cosav, ptsav);
    return;
}


static void makeWorkArea(Widget parent)
     /* was type Widget but nothing is returned . . . */
{
    PlugData *plug = &thisPlug;

    Widget container, row;
    Widget button;
    Boolean state;
    
    container = XtVaCreateWidget
	("container", xmRowColumnWidgetClass, parent, 
	 XmNentryAlignment, XmALIGNMENT_CENTER, NULL);
    
    row = XtVaCreateWidget
	 ("rowcol", xmRowColumnWidgetClass, container,
	  XmNorientation, XmHORIZONTAL, NULL);

    button = XtVaCreateManagedWidget
	("Go to Next Gap", xmPushButtonWidgetClass, container, NULL); 
    XtAddCallback(button, XmNactivateCallback, nextgap_cb, (XtPointer)1);

    button = XtVaCreateManagedWidget
	("Open Tiltalign Log File", xmPushButtonWidgetClass, container, NULL);
    XtAddCallback(button, XmNactivateCallback, open_cb, plug);

    plug->reread = XtVaCreateManagedWidget
	("Reread Log File", xmPushButtonWidgetClass, container, NULL);
    XtAddCallback(plug->reread, XmNactivateCallback, reread_cb, 
		  (XtPointer)0);
    XtSetSensitive(plug->reread, False);

    plug->nextlocal = XtVaCreateManagedWidget
        ("Go to Next Local Set", xmPushButtonWidgetClass, container, NULL);
    XtAddCallback(plug->nextlocal, XmNactivateCallback, reread_cb, 
		  (XtPointer)1);
    XtSetSensitive(plug->nextlocal, False);

    plug->nextres = XtVaCreateManagedWidget
        ("Go to Next Big Residual", xmPushButtonWidgetClass, container, NULL);
    XtAddCallback(plug->nextres, XmNactivateCallback, nextres_cb, plug);
    XtSetSensitive(plug->nextres, False);

    plug->movepoint = XtVaCreateManagedWidget
        ("Move Point by Residual", xmPushButtonWidgetClass, container, NULL);
    XtAddCallback(plug->movepoint, XmNactivateCallback, movepoint_cb, plug);
    XtSetSensitive(plug->movepoint, False);

    plug->undomove = XtVaCreateManagedWidget
        ("Undo Move", xmPushButtonWidgetClass, container, NULL);
    XtAddCallback(plug->undomove, XmNactivateCallback, undomove_cb, plug);
    XtSetSensitive(plug->undomove, False);

    row = XtVaCreateWidget
	 ("row", xmRowColumnWidgetClass, container, NULL);

    button = XtVaCreateManagedWidget
        ("Examine Points Once", xmToggleButtonWidgetClass, row, 
	 XmNmarginHeight, 0, NULL);
    XtAddCallback(button, XmNvalueChangedCallback, lookonce_cb, plug);
    state = plug->lookonce != 0;
    XmToggleButtonSetState(button, state, False);
    XtManageChild(row);

    plug->clearlist = XtVaCreateManagedWidget
        ("Clear Examined List", xmPushButtonWidgetClass, container, NULL);
    XtAddCallback(plug->clearlist, XmNactivateCallback, clearlist_cb, plug);

    row = XtVaCreateWidget
	 ("form", xmFormWidgetClass, container,
	  XmNfractionBase,    7,  NULL);
    
    button = XtVaCreateManagedWidget
        ("Help", xmPushButtonWidgetClass, row, 
	 XmNtopAttachment,    XmATTACH_FORM,
	 XmNbottomAttachment, XmATTACH_FORM,
	 XmNleftAttachment,   XmATTACH_POSITION,
	 XmNleftPosition,     1,
	 XmNrightAttachment,  XmATTACH_POSITION,
	 XmNrightPosition,    3,  
	 NULL);
    XtAddCallback(button, XmNactivateCallback, help_cb, plug);
    
    button = XtVaCreateManagedWidget
	("Close", xmPushButtonWidgetClass, row,
	 XmNtopAttachment,    XmATTACH_FORM,
	 XmNbottomAttachment, XmATTACH_FORM,
	 XmNleftAttachment,   XmATTACH_POSITION,
	 XmNleftPosition,     4,
	 XmNrightAttachment,  XmATTACH_POSITION,
	 XmNrightPosition,    6,  
	 NULL);
    XtAddCallback(button, XmNactivateCallback, quit_cb, plug);

    XtManageChild(row);

    XtManageChild(container);
}


