/*  IN TESTING VERSION 0.1
 *  4D Scope Lineage plugin
 *
 *  label.c - IMOD plugin to control point labels.
 */

/*****************************************************************************
 *   Copyright (C) 1997 by Boulder Laboratory for 3-Dimensional Fine         *
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
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/Scale.h>
#include <Xm/ArrowB.h>
#include <Xm/Separator.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <string.h>

#include <stdio.h>
#include <ctype.h>
#include <imodel.h>
#include <dia.h>
#include "imodplug.h"

/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
     ImodView    *view;
     Widget       window;
     int          control;

     /***** user data ****/
     int    ctime;
     int    listPos;
     int    listSize;
     int    refresh;
     Widget text;       
     Widget list;

}PlugData;

static PlugData thisPlug = { 0, 0, 0, 0};

static Widget makeWorkArea(Widget parent);
static void setWidgets(void);
static void split(PlugData *inPlug, char lc, char rc);
void plugDraw_cb(ImodView *inImodView, void *client, int drawflag);
void plugClose_cb(ImodView *vi, void *client, int reason);

/*
 * Called by the imod plugin load function. 
 */
char *imodPlugInfo(int *type)
{
    if (type)
	*type = IMOD_PLUG_MENU+IMOD_PLUG_KEYS;
     return("4D Cell Label");
}

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = (PlugData *)client;
     ivwDeleteControl(plug->view, plug->control);
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

	 /* 
	  * Bring the window to the front if already open.
	  */
	 if (plug->window) XtPopup(plug->window, XtGrabNone);
	 
	 wprint("%s: already open.\n", imodPlugInfo(0));
	 return;
     }

     plug->view = inImodView;
     /* 
      * Initialize user data. 
      */
     plug->listSize = 0;
     plug->refresh  = 0;
     plug->control  = ivwNewControl
	 (plug->view, plugDraw_cb, plugClose_cb, (XtPointer)plug);

     /*
      * This creates the plug window.
      */
     plug->window  = XtVaCreatePopupShell
	  (imodPlugInfo(0), topLevelShellWidgetClass, imodTopLevel(),
	   XmNvisual, imodVisual(),
	   XtNtitle, imodPlugInfo(0),
	   NULL);

     /* Make window conrols. */
     makeWorkArea(plug->window);

     /* Set up the quit function for the window. */
     wmclose = XmInternAtom( imodDisplay(),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(plug->window, wmclose, quit_cb,
			     (caddr_t)plug);

     /* Open up the window. */
     XtPopup(plug->window, XtGrabNone);
}


int plugKeys_cb(ImodView *vw, XKeyEvent *event)
{
    PlugData *plug = &thisPlug;
    KeySym keysym = XLookupKeysym((XKeyEvent *)event, 0);
    
    int ctrl =  event->state & ControlMask;
    if (!ctrl) return 0;
    
    switch(keysym){
	
      case XK_a:
	split(plug, 'a', 'p');
	return 1;
	
      case XK_l:
	split(plug, 'l', 'r');
	return 1;

      case XK_d:
	split(plug, 'd', 'v');
	return 1;
    }
    return 0;
}



/*********************** Window control commands *****************************/

/* Function to get current list item from point list. */
static int ListGetKbdItemPos(Widget w)
{
#if XmVERSION == 1
#if XmREVISION == 1
#define NOXmListGetKbdItemPos
#endif
#endif

#ifdef NOXmListGetKbdItemPos
    int *pos;
    int npos;
    if (XmListGetSelectedPos(w, &pos, &npos)){
	if (npos > 0){
	    npos = *pos;
	    XtFree(pos);
	    return npos;
	}else return 0;
    }else return 0;
#else
    return(XmListGetKbdItemPos(w));
#endif
}

static void split_ap(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = (PlugData *)client;
     split(plug, 'a', 'p');
}
static void split_lr(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = (PlugData *)client;
     split(plug, 'l', 'r');
}
static void split_dv(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = (PlugData *)client;
     split(plug, 'd', 'v');
}

/*  Make a menu item */
static Widget makeItem(char *label, Widget w, char mn,
                       char *acc, char *acct,
		       XtCallbackProc inCallback, XtPointer inData)
{
    Widget item;
    XmString mstr = XmStringCreateSimple(acct);

    item = XtVaCreateManagedWidget
            (label, xmPushButtonWidgetClass, w, 
             XmNmnemonic, mn,
             XmNaccelerator, acc, 
             XmNacceleratorText, mstr,
             NULL);
    XmStringFree(mstr);
    if (inCallback)
	XtAddCallback(item, XmNactivateCallback, inCallback, inData);
    return item;
}



/*  Make the menu bar. */
static Widget makeMenuBar(Widget w)
{
    PlugData *plug = &thisPlug;
    Widget menubar, cascade, menu, item;
    XmString labl;
    int n = 0;
    Arg args[10];

    XtSetArg(args[n], XmNdepth,    imodDepth()); n++;
    XtSetArg(args[n], XmNvisual,   imodVisual()); n++;
    XtSetArg(args[n], XmNcolormap, imodColormap());n++;
    XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
    menubar = XmCreateMenuBar(w, "menubar", args, n); n = 3;

    menu = XmCreatePulldownMenu(menubar, "pulldown", args, n);
    labl = XmStringCreateSimple("File");
    {
	cascade = XtVaCreateManagedWidget
	    ("cascade", xmCascadeButtonWidgetClass, menubar,
	     XmNsubMenuId, menu,
	     XmNlabelString, labl,
	     XmNmnemonic, 'F',
	     NULL);
	item = makeItem("Close Window", menu, 'W',  "Ctrl<Key>W", "Ctrl-W",
			quit_cb, plug);

    }
    XmStringFree(labl);

    menu = XmCreatePulldownMenu(menubar, "pulldown", args, n);
    labl = XmStringCreateSimple("Edit");
    {
	cascade = XtVaCreateManagedWidget
	    ("cascade", xmCascadeButtonWidgetClass, menubar,
	     XmNsubMenuId, menu,
	     XmNlabelString, labl,
	     XmNmnemonic, 'E',
	     NULL);
	item = makeItem("Can't Undo", menu, 'U',  "Ctrl<Key>X", "Ctrl-X",
			NULL, plug);
	XtSetSensitive(item, False);

	XtVaCreateManagedWidget
	    ("separator", xmSeparatorWidgetClass, menu, NULL);

	item = makeItem("Split a p", menu, 'a', "Ctrl<Key>A", "Ctrl-A",
			split_ap, plug);
	item = makeItem("Split l r", menu, 'l', "Ctrl<Key>L", "Ctrl-L",
			split_lr, plug);
	item = makeItem("Split d v", menu, 'a', "Ctrl<Key>D", "Ctrl-D",
			split_dv, plug);


    }
    XmStringFree(labl);

    XtManageChild(menubar);
    return(menubar);
}

static char *getListLabel(PlugData *plug, int pos)
{
     XmStringTable stab;
     XmString      str;
     char *listLabel;

     if (pos < 0) return(NULL);
     XtVaGetValues(plug->list, XmNitems, &stab, NULL);
     if (pos)
	 str = stab[pos-1];
     else
	 str = stab[plug->listSize -1];
     XmStringGetLtoR(str, XmSTRING_DEFAULT_CHARSET, &listLabel);
     if (!listLabel)
	  wprint("\a\nError getting label.\n");
     return(listLabel);
}

static void list_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    XmListCallbackStruct *cbs = (XmListCallbackStruct *)call;
    Imod *imod = ivwGetModel(plug->view);
    int ob, co, pt; 
    char *listLabel;
    char *nptr;
    plug->listPos =   cbs->item_position;

    /* goto point */
    if (plug->listPos < 2) return;

    listLabel = getListLabel(plug, plug->listPos);
    if (!listLabel) return;

    sscanf(listLabel, "%d %d %d", &ob, &co, &pt);
    wprint("listview %s set to %d %d %d\n", listLabel, ob, co, pt);
    imodSetIndex(imod, ob-1, co-1, pt-1);
    XtFree(listLabel);
    
    XmTextSetString(plug->text, &listLabel[14]);
    ivwDraw(plug->view, IMOD_DRAW_RETHINK);
}

static void text_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    char *name = XmTextGetString(w);
    Imod *imod = ivwGetModel(plug->view);
    Icont *cont;
    Ilabel *label;
    int oob, oco, opt;
    int ob, co, pt;
    char *listLabel;
    int pos = ListGetKbdItemPos(plug->list);

    listLabel = getListLabel(plug, plug->listPos);
    if (!listLabel) return;
    imodGetIndex(imod, &oob, &oco, &opt);

    sscanf(listLabel, "%d %d %d", &ob, &co, &pt);
    imodSetIndex(imod, ob-1, co-1, pt-1);
    cont = imodContourGet(imod);
    label = imodContourGetLabel(cont);
    if (!label){
	 label = imodLabelNew();
	 imodContourSetLabel(cont, label);
    }
    imodLabelItemAdd(label, name, pt-1);
    imodSetIndex(imod, oob, oco, opt);
    plug->refresh = 1;
    setWidgets();
    XtFree(listLabel);
    XtFree(name);
}

static Widget makeWorkArea(Widget parent)
{
    PlugData *plug = &thisPlug;

    Widget container, form;
    
    Widget list, menubar;

    int i, n = 0;
    Arg args[10];

    form = container = XtVaCreateWidget
	("container", xmFormWidgetClass, parent, NULL);
    {
	menubar = makeMenuBar(container);


	plug->text = XtVaCreateManagedWidget
	    ("View Label", xmTextWidgetClass, form,
	     XmNcolumns, 40,
	     XmNrows, 1,
	     XmNleftAttachment, XmATTACH_FORM,
	     XmNrightAttachment, XmATTACH_FORM,
	     XmNbottomAttachment, XmATTACH_FORM,
	     NULL);
	XtAddCallback( plug->text, XmNactivateCallback,
		      text_cb, (XtPointer)plug);


	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNtopWidget, menubar); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n], XmNbottomWidget, plug->text); n++;
	XtSetArg(args[n], XmNvisibleItemCount, 24); n++;
	XtSetArg(args[n], XmNselectionPolicy, XmSINGLE_SELECT); n++; 
	plug->listPos = 1;
	plug->list = XmCreateScrolledList(form, "PointLabels", args, n);
	XtAddCallback(plug->list, XmNdefaultActionCallback, 
		      list_cb, (XtPointer)plug);
	XtAddCallback(plug->list, XmNsingleSelectionCallback, 
		      list_cb, (XtPointer)plug);
	

    }
    setWidgets();
     XtManageChild(plug->list);
    XtManageChild(form);
    XtManageChild(container);
    return 0;
}


/***************** The Low Level Guts of the plugin. ***********************/


static void setWidgets(void)
{
    PlugData *plug =  &thisPlug;
    Imod *theModel = ivwGetModel(plug->view);
    Iobj *theObject;
    Icont *theContour;
    Ilabel *theLabel;
    int ctime;
    int ob, co, mpt, pt;
    int pos = 1;
    char *pointLabel;
    char listLabel[128];
    int cob, cco, cpt;
    XmString str;
    int newSize = 0;
    int refreshItems = 1;
    
    /* Get the current time index, and the total number of
     * model point for this time.
     */
    ivwGetTime(plug->view, &ctime);
    imodGetIndex(theModel, &cob, &cco, &cpt);

    for(theObject = imodObjectGetFirst(theModel); theObject;
	theObject = imodObjectGetNext(theModel))
      for(theContour = imodContourGetFirst(theModel); theContour;
	  theContour = imodContourGetNext(theModel)){
	if (imodContourGetTimeIndex(theContour) != ctime) continue;
	newSize += imodContourGetMaxPoint(theContour);
      }
    
    /* Check and see if we really need to update list. */
    if (plug->ctime == ctime){
	 if (newSize == plug->listSize) 
	      refreshItems = 0;
    }
    plug->listSize = newSize;
    plug->ctime = ctime;
    if (plug->refresh){
	 plug->refresh = 0;
	 refreshItems = 1;
    }
    
    /*
     * Update the label list. 
     */
    if (refreshItems){
	 XmListDeleteAllItems(plug->list);
	 sprintf(listLabel, "Obj Co  Pt  : Label                 ");
	 
	 str = XmStringCreateSimple(listLabel);
	 XmListAddItem(plug->list, str, pos);
    }
    pos++;

    for(ob = 1, theObject = imodObjectGetFirst(theModel);
	theObject != NULL;
	theObject = imodObjectGetNext(theModel), ob++){
	 
	 if (!imodObjectGetValue( theObject, IobjFlagTime )) continue;
	 
	 for(co = 1, theContour = imodContourGetFirst(theModel);
	     theContour != NULL;
	     theContour = imodContourGetNext(theModel), co++){
	      if (imodContourGetTimeIndex(theContour) != ctime)
		   continue;
	      
	      mpt = imodContourGetMaxPoint(theContour);
	      theLabel = imodContourGetLabel(theContour);
	      for(pt = 0; pt < mpt; pt++){
		   
		   pointLabel = NULL;
		   if (theLabel)
			pointLabel = imodLabelItemGet(theLabel, pt);
		   
		   if (refreshItems){
			if (!pointLabel)
			     sprintf(listLabel, "%3d %3d %3d : ",
				     ob, co, pt+1);
			else
			     sprintf(listLabel, "%3d %3d %3d : %s",
				     ob, co, pt+1, pointLabel);
			str = XmStringCreateSimple(listLabel);
			XmListAddItem(plug->list, str, pos);
		   }

		   /* Set the current point in the list. 
		    * and autoscroll so we can see point.
		    */
		   if ((plug->listPos != pos) &&
		       (ob == (cob+1)) && (co == (cco+1)) && (pt == cpt)){
			XmListSelectPos(plug->list, pos, False);
			XtVaSetValues(plug->list, 
				      XmNtopItemPosition, pos, NULL);
			XmTextSetString(plug->text, pointLabel);
			plug->listPos = pos;
			
		   }
		   pos++;
	      }
	 }
    }
    imodSetIndex(theModel, cob, cco, cpt);    
    return;
}


/* Split the current point into two points. */
static void split(PlugData *inPlug, char lc, char rc)
{
  Imod   *imod  = ivwGetModel(inPlug->view);
  Iobj   *obj   = imodObjectGet(imod);
  Icont  *cont  = imodContourGet(imod);
  Icont  *tmpCont;
  Ipoint *point = imodPointGet(imod);
  Ilabel *label = imodContourGetLabel(cont);
  Ilabel *tmpLabel;
  int oob, oco, opt;
  char *labelRoot;
  int rootsize;
  char *llab, *rlab;
  Ipoint lpt;

  int currentTime, askFuture = 0;
  int pt, mpt;
  char *tmproot;

  imodGetIndex(imod, &oob, &oco, &opt);

  if (!point){
    wprint("\aSplit %c %c failed. No current point selected.\n", lc, rc);
    return;
  }

  labelRoot = imodLabelItemGet(label, opt);
  if (!labelRoot){
    wprint("\aSplit %c %c failed. No current point with label\n", lc, rc);
    return;
  }

  /*
   *  Check future times for occurence of this lable.
   */
  currentTime = imodContourGetTimeIndex(cont);
  for(tmpCont = imodContourGetFirst(imod);
      (tmpCont != NULL) && (!askFuture);
      tmpCont = imodContourGetNext(imod)){
      if (imodContourGetTimeIndex(tmpCont) <= currentTime)
	  continue;
      mpt = imodContourGetMaxPoint(tmpCont);
      tmpLabel = imodContourGetLabel(tmpCont);
      for(pt = 0; pt<mpt; pt++){
	  tmproot = imodLabelItemGet(tmpLabel,pt);
	  if (!tmproot) continue;
	  if (strcmp(labelRoot, tmproot) == 0){
	      askFuture = 1;
	      break;
	  }
      }
  }
  if (askFuture){
      char left[32], right[32];
      char *newlab;
      char addlabel;
      int choice;

      sprintf(left, "Set to %c", lc);
      sprintf(right, "Set to %c", rc);
      choice = dia_choice("This label exists at future timepoints."
			  "Select extension to be added:",
			  left, right,  "Cancel");
      switch(choice){
	case 1:
	  addlabel = lc;
	  break;
	case 2:
	  addlabel = rc;
	  break;
	case 3:
	  return;
      }
      rootsize = strlen(labelRoot); 
      newlab = malloc(rootsize + 2);
      sprintf(newlab, "%s%c", labelRoot, addlabel);
      for(tmpCont = imodContourGetFirst(imod);
	  tmpCont != NULL;
	  tmpCont = imodContourGetNext(imod)){
	  if (imodContourGetTimeIndex(tmpCont) <= currentTime)
	      continue;
	  mpt = imodContourGetMaxPoint(tmpCont);
	  tmpLabel = imodContourGetLabel(tmpCont);
	  for(pt = 0; pt<mpt; pt++){
	      tmproot = imodLabelItemGet(tmpLabel,pt);
	      if (!tmproot) continue;
	      if (strcmp(labelRoot, tmproot) == 0){
		  imodLabelItemAdd(tmpLabel, newlab, pt); 
		  continue;
	      }
	  }
      }      
      free(newlab);
  }

  /*
   *  Split the current point.
   */
  lpt = *point;
  lpt.x += obj->pdrawsize;
  point->x -= obj->pdrawsize;
  imodPointAdd(cont, &lpt, opt+1);

  rootsize = strlen(labelRoot);
  llab = (char *)malloc(rootsize + 2);
  rlab = (char *)malloc(rootsize + 2);
  sprintf(llab, "%s%c", labelRoot, lc);
  sprintf(rlab, "%s%c", labelRoot, rc);
  imodLabelItemAdd(label, llab, opt);
  imodLabelItemAdd(label, rlab, opt+1);
  free(llab); free(rlab);

  imodSetIndex(imod, oob, oco, opt);
}

/*
 * imod callback functions.
 *
 */
void plugDraw_cb(ImodView *inImodView, void *client, int drawflag)
{
    PlugData *plug = (PlugData *)client;

    /* Only draw if model has changed. */
    if (!(drawflag & IMOD_DRAW_MOD)) return;
	setWidgets();
}

void plugClose_cb(ImodView *vi, void *client, int reason)
{
     PlugData *plug = (PlugData *)client;

     plug->view = NULL;
     XtPopdown(plug->window);
     XtDestroyWidget(plug->window);
     plug->window = 0;
}



