#define PLUGIN_VERSION_STRING "Version 0.13"
/*
 * include basic X-Windows, Motif and OpenGL headers
 */
#include <X11/Xlib.h>
#include <X11/keysym.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/Separator.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GLwMDrawA.h>

/*
 *  Include IMOD headers.
 */

#include <imodel.h>
#include "imodplug.h"

/*
 * Cheat and use these internal imod functions.
 */
void dia_vasmsg(char *, ...);
void diaBusyCursor(int inBusy);
int  b3dDrawPlus(int x, int y, int size);
void imodSetObjectColor(int inObjectNumber);
void inputLastPoint(ImodView *inView);
void inputPointMove(ImodView *, int , int, int);
void imodOverrideTranslations(Widget w, XtTranslations translations);

/* Other interal functions. 
 */
static void copycont_cb(Widget w, XtPointer client, XtPointer call);

#define USEGFX
#define CANT_UNDO 0
#define UNDO      1
#define REDO      2

String plugTranslations =
"<KeyDown>:   glwInput()\n\
 <BtnDown>:   glwInput()\n\
 <BtnMotion>: glwInput()\n\
 <Btn1Up>:    glwInput()\n\
 <EnterNotify>: glwInput()\n\
";

/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
     ImodView    *view;
     Widget       window;
     int          control;

     Widget       wLocate;
     Widget       drawingArea;
     GLXContext   context;
     int          width, height;
     int          zoom;
     int          needUpdate;

     Widget       wUndo;
     XmString     sUndo;
     XmString     sRedo;
     int          undo;

     int cob, cco, cpt;       /* current model index      */
     int ctime, timeStep;     /* current time and section */
     int cx, cy, cz;          /* image position. */     
     int nx, ny, nz;          /* image size. */
     int diff, diffx, diffy, diffz; /* report move for locate_cb */

     unsigned char *cur, *mat, *pre;
     Ipoint curp,   matp;
     int    cmpSize;

     int gtime, gz; /* The number of time and depth images in the 
		     * graphics window.
		     */

     XtWorkProcId workProcId;
     int wob, wco, wpt;       /* workproc model index           */
     int wct, wmt;            /* workproc current and max time. */
     
}PlugData;

static PlugData thisPlug = { 0, 0, 0};

Boolean autoPlaceWorkProc(XtPointer client);
void updateGLViewport(PlugData *plug);
static void plugDraw(PlugData *plug);
static Widget makeWorkArea(Widget parent);
static int locate(PlugData *plug, int ob, int co, int pt);
static int first(PlugData *plug, Imod *imod);
static int next(PlugData *plug, Imod *imod);
static void copy_cb(Widget w, XtPointer client, XtPointer call);

/*
 * Called by the imod plugin load function.
 */
char *imodPlugInfo(int *type)
{
    if (type)
        *type = IMOD_PLUG_MENU;
     return("4D Automod");
}

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = (PlugData *)client;
     ivwDeleteControl(plug->view, plug->control);
}

void plugDraw_cb(ImodView *inImodView, void *client, int drawflag)
{
    PlugData *plug = (PlugData *)client;
    Imod *imod = ivwGetModel(inImodView);
    imodGetIndex(imod, &plug->cob, &plug->cco, &plug->cpt);
    ivwGetTime(inImodView, &plug->ctime);
    plugDraw(plug);
}

void plugClose_cb(ImodView *vi, void *client, int reason)
{
     PlugData *plug = (PlugData *)client;
     if (plug->workProcId)
          XtRemoveWorkProc(plug->workProcId);
     plug->view = NULL;
     XtPopdown(plug->window);
     XtDestroyWidget(plug->window);
     plug->window = 0;
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
         wprint("\a%s: already open.\n", imodPlugInfo(0));
         return;
     }

     plug->view = inImodView;

     /*
      * Bring the window to the front if already open.
      */
     if (plug->window) XtPopup(plug->window, XtGrabNone);
 

     /*
      * Initialize user data.
      */
     plug->undo       = CANT_UNDO;
     plug->width      = 130;
     plug->height     = 200;
     plug->cmpSize    =  32;
     plug->timeStep   = 1;
     plug->workProcId = 0;
     plug->gtime = 0;
     plug->gz = 1;
     plug->needUpdate = True;
     ivwGetImageSize(plug->view, &plug->nx, &plug->ny, &plug->nz);

     
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
     
     plug->sUndo = XmStringCreateSimple("Undo");
     plug->sRedo = XmStringCreateSimple("Redo");

     /*
      * Make window conrols.
      */	
     makeWorkArea(plug->window);

     /* Set up the quit function for the window. */
     wmclose = XmInternAtom( imodDisplay(),
                            "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(plug->window, wmclose, quit_cb,
                             (caddr_t)plug);

     /* Open up the window. */
     XtPopup(plug->window, XtGrabNone);
}

static void about_cb(Widget w, XtPointer client, XtPointer call)
{
    dia_vasmsg("Time Point Automod Plugin\n",
	       "Models 4D image points through time.\n\n",
	       PLUGIN_VERSION_STRING,
	       "\nWritten by James Kremer, <James.Kremer@colorado.edu>",
	       NULL);
}
static void ncontcopy(PlugData *plug, int tn)
{
    int t;
    for(t = 0; t < tn; t++)
	copycont_cb(plug->window, plug, plug);
}
/* Do a copy and match for tn number of time intervals. */
static void ncopy(PlugData *plug, int tn)
{
     Imod *imod = ivwGetModel(plug->view);
     int   maxtime, t;

     if (plug->workProcId)
	  XtRemoveWorkProc(plug->workProcId);

     maxtime = ivwGetTime(plug->view, &plug->wct);
     if (!tn) return;
     plug->wmt = plug->wct + tn;
     if (plug->wmt > maxtime) plug->wmt = maxtime;

     plug->wob = 0;
     plug->wco = 0;
     plug->wpt = 0;

#ifdef COPY_WORKPROC
     plug->workProcId = XtAppAddWorkProc
	  (imodAppContext(), autoPlaceWorkProc, plug);
#else
     for(t = 0; t < tn; t++)
	 copy_cb(plug->window, plug, plug);
#endif
}

#ifdef COPY_WORKPROC
Boolean autoPlaceWorkProc(XtPointer client)
{
    PlugData *plug = (PlugData *)client;
    Imod *imod         = ivwGetModel(plug->view);
    Iobj *obj;
    Icont *cont;
    Ipoint *pt;
    int done = False;

    /*
     * Save the old model index.
     */
    int oob, oco, opt;
    imodGetIndex(imod, &oob, &oco, &opt);

    if (plug->wct > plug->wmt){
	 plug->workProcId = 0;
         return(True);
    }

    if (plug->wob >= imod->objsize){
	plug->wob = plug->wco = plug->wpt = 0;
	plug->wct++;
	return(False);
    }
    
    imodSetIndex(imod, plug->wob, plug->wco, plug->wpt);
    obj  = imodObjectGet(imod);
    if (!obj){
	 imodSetIndex(imod, oob, oco, opt);
	 plug->workProcId = 0;
	 return(True);
    }
    if (plug->wco >= obj->contsize){
	 plug->wpt =  plug->wco = 0;
	 plug->wob++;
	 return(False);
    }

    cont = imodContourGet(imod);
    if (!cont){
	 imodSetIndex(imod, oob, oco, opt);
	 plug->workProcId = 0;
	 return(True);
    }
    /*  copy contour */
    if (plug->wpt == 0){
	Icont *dupcont = imodContourDup(cont);
	dupcont->type = cont->type + 1;
	NewContour(imod);
	cont  = imodContourGet(imod);
	*cont = *dupcont;
	free(dupcont);
    }
    if ((plug->wpt >= cont->psize) || (plug->wct != cont->type)){
	 plug->wpt = 0;
	 plug->wco++;
	 return(False);
    }

    /*  todo copy and move point. */
    locate(plug, plug->wob, plug->wco, plug->wpt);

    imodSetIndex(imod, oob, oco, opt);
    if (done) plug->workProcId = 0;
    return(done);
}
#endif

static void copycont_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    Imod *imod         = ivwGetModel(plug->view);
    Icont *dupcont, *cont;
    int stat;
    int ob, co, pt;
    int currentTime;
    int nextTime, maxTime;

    maxTime = ivwGetTime(plug->view, &currentTime);

    cont = imodContourGet(imod);
    if (!cont) return;
    currentTime = cont->type;
    nextTime = currentTime + plug->timeStep;
    if (nextTime > maxTime){
        wprint("Warning: Copy failed.\n"
               "\tNext time point is invalid.\n");
        return;
    }

    diaBusyCursor(True);

    dupcont = imodContourDup(cont);
    dupcont->type = nextTime;
    NewContour(imod);
    cont  = imodContourGet(imod);
    *cont = *dupcont;
    free(dupcont);

    ivwSetTime(plug->view, nextTime);
    imodGetIndex(imod, &ob, &co, &pt);

    for(pt = 0; pt < cont->psize; pt++){
	locate(plug, plug->cob, plug->cco, pt);
    }
    plug->cob = ob;
    plug->cco = co;
    plug->cpt = 0;
    imodSetIndex(imod, ob, co, 0);
    ivwControlPriority(plug->view, plug->control);
    ivwDraw(plug->view, IMOD_DRAW_RETHINK);
    
    wprint("Countour time copy and move completed.\n");
    diaBusyCursor(False);

}


static void movecont_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    Imod *imod         = ivwGetModel(plug->view);
    Icont *cont = imodContourGet(imod);
    int ob, co, pt;

    if (!cont) return;
    diaBusyCursor(True);
    imodGetIndex(imod, &ob, &co, &pt);
    for(pt = 0; pt < cont->psize; pt++){
        locate(plug, plug->cob, plug->cco, pt);
    }
    plug->cob = ob;
    plug->cco = co;
    plug->cpt = 0;
    imodSetIndex(imod, ob, co, 0);
    ivwControlPriority(plug->view, plug->control);
    ivwDraw(plug->view, IMOD_DRAW_RETHINK);
    wprint("Countour time move completed.\n");
    diaBusyCursor(False);
}

static void copy_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    Imod *imod         = ivwGetModel(plug->view);
    int stat;

    int currentObject;
    int currentContour;
    int currentPoint;
    int currentTime;
    int nextTime, maxTime;
    
    char *string;
    Iobj  *obj;
    Icont *cont, *dupcont;
    int ob, co;

    ivwControlPriority(plug->view, plug->control);
    imodGetIndex(imod, &currentObject, &currentContour, &currentPoint);
    maxTime = ivwGetTime(plug->view, &currentTime);
    nextTime = currentTime + plug->timeStep;
    
    if (nextTime > maxTime){
	wprint("Warning: Copy failed.\n"
	       "\tNext time point is invalid.\n");
	return;
    }
    
    diaBusyCursor(True);
    
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
    
    ivwSetTime(plug->view, nextTime);
    imodSetIndex(imod, currentObject, currentContour+1, currentPoint);
    
    if (currentTime > 1)
	for(stat = first(plug, imod); stat; stat = next(plug, imod)){
	    ivwGetLocation (plug->view, &plug->cx, &plug->cy, &plug->cz);
	    imodGetIndex(imod, &plug->cob, &plug->cco, &plug->cpt);
	    ivwGetTime(plug->view, &plug->ctime);
	    locate(plug, plug->cob, plug->cco, plug->cpt);
	}
    
    first(plug, imod);
/*    plugDraw(plug); */
    ivwDraw(plug->view, IMOD_DRAW_RETHINK);
    wprint("Time copy completed.\n");
    diaBusyCursor(False);
    return;
}

/*
 * Move this point to end of time.
 */
static void moveptime_cb(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = (PlugData *)client;
     Imod     *imod = ivwGetModel(plug->view);
     Iobj     *obj  = imodObjectGet(imod);
     Icont    *cont = imodContourGet(imod);
     Ilabel   *lab  = imodContourGetLabel(cont);
     char     *txt, *ctxt;
     int oob, oco, opt;
     int ob, co, pt;
     int mtime, time;
     int foundpt;

     if (!cont) return;

     imodGetIndex(imod, &ob, &co, &pt);
     imodGetIndex(imod, &oob, &oco, &opt);
     ivwControlPriority(plug->view, plug->control);

     mtime = cont->type;
     txt = imodLabelItemGet(lab, pt);
     if (!txt){
	 wprint("Match Point to end of time error:\n"
		"\tCurrent point has no label.\n");
	 return;
     }

     /* This may take a little while. */
     diaBusyCursor(True);

     /* calculate max time for this object. */
     for(co = 0; co < obj->contsize; co++){
	  if (obj->cont[co].type > mtime)
	       mtime = obj->cont[co].type;
     }
     wprint("Match Point to end of time: from %d to %d\n",
	    cont->type, mtime);

     for(time = cont->type+1; time < mtime; time++)
	  for(co = 0; co < obj->contsize; co++){
	       if (obj->cont[co].type != time)
		    continue;
	       lab  = imodContourGetLabel(cont);
	       if (!lab){
		    wprint("Match Point to end of time error:\n"
			   "\tContour %d has no label.\n", co+1);
		    ivwDraw(plug->view, IMOD_DRAW_RETHINK);
		    diaBusyCursor(False);
		    return;
	       }
	       foundpt = FALSE;
	       for(pt = 0; pt < obj->cont[co].psize; pt++){
		    ctxt = imodLabelItemGet(lab, pt);
		    if ( (ctxt) && (txt)&& ( strcmp(txt, ctxt) == 0)){
			 imodSetIndex(imod, ob, co, pt);
			 plug->ctime = time;
			 locate(plug, ob, co, pt);
			 co = obj->contsize;
			 foundpt = TRUE;
			 break;
		    }
	       }

	       /*
		* If we haven't found a point 
		*   create one. (bad idea)
		*   report and stop.
		*/
	       if (!foundpt){
		    /*
		   Ipoint point;
		   point.x = point.y = point.z = 0.0f;

		   imodSetIndex(imod, ob, co, pt-1);
		   imodNewPoint(imod, &point);
		   imodLabelItemAdd(lab, txt, pt);
		   locate(plug, ob, co, pt);
		   */
		    wprint("Match Point to end of time error: Aborted\n"
                           "\tFailed to find point for time %d.\n", time);
		    wprint("\tThe point may have branched or is missing.\n");
                    diaBusyCursor(False);
		    ivwDraw(plug->view, IMOD_DRAW_RETHINK);
                    return;
	       }
	  }

     /* All done with this for now. */
     
     diaBusyCursor(False);
     ivwDraw(plug->view, IMOD_DRAW_RETHINK);
}

/*
 * Move all points at this time.
 */
static void moveall_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    Imod *imod = ivwGetModel(plug->view);
    int stat;
    int currentTime;

    ivwGetTime(plug->view, &currentTime);
    if (currentTime <= 1) return;

    diaBusyCursor(True);
    for(stat = first(plug, imod); stat; stat = next(plug, imod)){
	 ivwGetLocation (plug->view, &plug->cx, &plug->cy, &plug->cz);
	 imodGetIndex(imod, &plug->cob, &plug->cco, &plug->cpt);
	 ivwGetTime(plug->view, &plug->ctime);
	 locate(plug, plug->cob, plug->cco, plug->cpt);
    }
    first(plug, imod);
    diaBusyCursor(False);
    ivwControlPriority(plug->view, plug->control);
    ivwDraw(plug->view, IMOD_DRAW_RETHINK);
}


/*
 * Find first point at current time.
 */
static int first(PlugData *plug, Imod *imod)
{
    int ct, mt = ivwGetTime(plug->view, &ct);
    Iobj *obj;
    int ob,co;

    for(ob = 0; ob<imod->objsize; ob++){
	obj =  &imod->obj[ob];
	if (!iobjFlagTime(obj)) continue;
	for(co = 0; co < obj->contsize; co++){
	     if (obj->cont[co].type != ct)
		  continue;
	     if (!obj->cont[co].psize)
		  continue;

	     plug->cob = ob;
	     plug->cco = co;
	     plug->cpt = 0;
	     imodSetIndex(imod, plug->cob, plug->cco, plug->cpt);
	     return(1);
	}
    }
    return(0);
}

static void first_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    Imod *imod = ivwGetModel(plug->view);
    Icont *cont;
    Ipoint *p = imodPointGet(imod);
    
    if (first(plug, imod)){
	 p = imodPointGet(imod);
	 if (p) ivwSetLocationPoint(plug->view, p);
	 ivwControlPriority(plug->view, plug->control);
	 ivwDraw(plug->view, IMOD_DRAW_RETHINK);
    }
}

static int next(PlugData *plug, Imod *imod)
{
     Iobj  *obj;
     Icont *cont;
     Ipoint *pnt;
     int ob, co, ct;

     imodSetIndex(imod, plug->cob, plug->cco, plug->cpt);
     obj = imodObjectGet(imod);
     if (!obj) return 0;
     cont = imodContourGet(imod);
     if (!cont) return 0;
     ct = cont->type;

     if (cont->psize > (plug->cpt+1)){
	  plug->cpt++;
	  imodSetIndex(imod, plug->cob, plug->cco, plug->cpt);
	  return 1;
     }
     for(co = plug->cco+1; co < obj->contsize; co++){
	  if (obj->cont[co].type != ct)
	       continue;
	  if (!obj->cont[co].psize)
	       continue;
	  plug->cco = co;
	  plug->cpt = 0;
	  imodSetIndex(imod, plug->cob, plug->cco, plug->cpt);
	  return(1);
     }

     for(ob = plug->cob+1; ob<imod->objsize; ob++){

	  obj =  &imod->obj[ob];
	  if (!iobjFlagTime(obj)) continue;
	  for(co = 0; co < obj->contsize; co++){
	       if (obj->cont[co].type != ct)
		    continue;
	       if (!obj->cont[co].psize)
		    continue;
	       
	       plug->cob = ob;
	       plug->cco = co;
	       plug->cpt = 0;
	       imodSetIndex(imod, plug->cob, plug->cco, plug->cpt);
	       return(1);
	  }
     }
     return(0);     
}

static void next_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    Imod *imod = ivwGetModel(plug->view);
    Icont *cont;
    Ipoint *p;

    if (next(plug, imod)){
	 p = imodPointGet(imod);
	 if (p) ivwSetLocationPoint(plug->view, p);
	 ivwControlPriority(plug->view, plug->control);
	 ivwDraw(plug->view, IMOD_DRAW_RETHINK);
    }
}

static void undo_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    Ipoint *p;
    Imod *imod = ivwGetModel(plug->view);

    if (!imod) return;
    imodSetIndex(imod, plug->cob, plug->cco, plug->cpt);
    p = imodPointGet(imod);
    if (!p) return;

    if (plug->undo == UNDO){
	plug->undo = REDO;
	XtVaSetValues(plug->wUndo, XmNlabelString, plug->sRedo, NULL);
	*p = plug->curp;
	
    }else{
	plug->undo = UNDO;
	XtVaSetValues(plug->wUndo, XmNlabelString, plug->sUndo, NULL);
	*p = plug->matp;
    }
    ivwControlPriority(plug->view, plug->control);
    ivwDraw(plug->view, IMOD_DRAW_RETHINK);
}


static int cmpslice(PlugData *plug, int x, int y)
{
    int diff = 0;
    int i, is, im;
    int j, js, jm;
    int jo, xsize = plug->nx;
    int d;

    unsigned char *base = plug->cur;
    unsigned char *comp = plug->mat;

    is = plug->cx - (plug->cmpSize/2);
    if (is < 0) return 0;
    if ((is + x) < 0) return 0;

    im = plug->cx + (plug->cmpSize/2);
    if (im > plug->nx) return 0;
    if ((im + x) > plug->nx) return 0;

    js = plug->cy - (plug->cmpSize/2);
    if (js < 0) return 0;
    if ((js + y) < 0) return 0;

    jm = plug->cy + (plug->cmpSize/2);
    if (jm > plug->ny) return 0;
    if ((jm + y) > plug->ny) return 0;

    for(j = js; j < jm; j++){
	jo = j * xsize;
	for(i = is; i < im; i++){
	    d =  base[i + jo] - comp[(i+x) + ((j+y)*xsize)];
	    if (d < 0) d *= -1;
	    diff += d;
	}
    }
    return diff;
}

static void locate_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    Imod     *imod = ivwGetModel(plug->view);
    Ipoint   *p    = imodPointGet(imod);
    int       oob,oco,opt;

    if (!p){
        wprint("%s: No point selected.\n", imodPlugInfo(0));
        return;
    }

    imodGetIndex(imod, &plug->cob, &plug->cco, &plug->cpt);
    imodGetIndex(imod, &oob, &oco, &opt);
    ivwGetTime(plug->view, &plug->ctime);
    ivwGetLocation (plug->view, &plug->cx, &plug->cy, &plug->cz);
    ivwGetImageSize(plug->view, &plug->nx, &plug->ny, &plug->nz);

    locate(plug, plug->cob, plug->cco, plug->cpt);
    imodSetIndex(imod, oob, oco, opt);
    ivwControlPriority(plug->view, plug->control);
    ivwDraw(plug->view, IMOD_DRAW_RETHINK);
}


/*
 *  Set new location for point with index given by  
 *  plug cob, cco and cpt.
 *
 */
static int locate(PlugData *plug,
		  int cob, int cco, int cpt)
{
    Imod     *imod = ivwGetModel(plug->view);
    Iobj     *obj;
    Icont    *cont, *ocnt;
    Ipoint   *p, *opnt = NULL;
    
    int cz = plug->cz;
    int ico, iob, ipt;
    int prevTime, curTime;

    /* Save the initial model index. */
    imodGetIndex(imod, &ico, &iob, &ipt);


    /* Set the current model index and get
     * data for this index.
     */
    imodSetIndex(imod, cob, cco, cpt);
    obj  = imodObjectGet(imod);
    cont = imodContourGet(imod);
    p    = imodPointGet(imod);
    if (!p){
	imodSetIndex(imod, ico, iob, ipt);
	return(-1);
    }
    curTime  = cont->type;
    prevTime = curTime - 1;

    /* Compare to same point at an earler time. */
    {
	int pt, co, eco=-1;
	int tet, et = 0;
	Ilabel *elab, *lab = imodContourGetLabel(cont);
	char  *etxt,  *txt = NULL;

	if (lab) 
	    txt = imodLabelItemGet(lab, cpt);

	for(co = 0; co < obj->contsize; co++){
	    tet = curTime - obj->cont[co].type;
	    if (tet <= 0) continue;
	    if (!et){ 
		et = tet; 
		eco = co; 
		prevTime = obj->cont[co].type;
	    }
	    if (tet < et){ 
		et = tet; 
		eco = co; 
		prevTime = obj->cont[co].type;
	    }
	}

	if (et){
	    ocnt = &obj->cont[eco];
	    elab = imodContourGetLabel(ocnt);

	    /* Set default point in case no label matches. */
	    opnt = &ocnt->pts[plug->cpt];
	    plug->cx = opnt->x;
	    plug->cy = opnt->y;
	    plug->cz = opnt->z;

	    if ((elab) && (lab) && (txt)){
		int foundpt = FALSE;
		for(pt = 0; pt < ocnt->psize; pt++){
		    etxt = imodLabelItemGet(elab, pt);
		    
		    if ( (etxt) && ( strcmp(etxt, txt) == 0)){
			foundpt = TRUE;
			opnt = &ocnt->pts[pt];
			plug->cx = opnt->x;
			plug->cy = opnt->y;
			plug->cz = opnt->z;
			break;
		    }
		}
		if (!foundpt){
		    wprint("Point Match Warning:\n"
			   "\tCouldn't find point to match"
			   "ob %d co %d pt %d : %s\n",
			   cob+1, cco+1, cpt+1, txt);
		    return 1;
		}
		
	    }
	}else{
	    wprint("Match Point Error: No earlier time point found.\n");
	    return -1;
	}
    }
    
    /* 
     * The current location of the point, same location as
     * the point at a previous time.
     */
    plug->cur = ivwGetZSectionTime(plug->view, plug->cz, prevTime);
    if (!plug->cur){
	wprint("Match Point Error: No old image data available.\n");
	wprint("    section %d time %d\n", plug->cz, prevTime);
	return -1;
    }
    
    /* The image we will match to. */
    plug->mat = ivwGetZSectionTime(plug->view, plug->cz, plug->ctime);
    if ((!plug->mat)){
	wprint("Match Point Error: No match image data available.\n");
	wprint("    section %d time %d\n", plug->cz, plug->ctime);
	return -1;
    }

    plug->curp = *p;
    plug->matp = *p;
    /* if we found an older point set it instead. */
    if (opnt){
	plug->curp = *opnt;
	plug->matp = *opnt;
    }
    plug->cx = (int)plug->curp.x;
    plug->cy = (int)plug->curp.y;
    plug->cz = (int)plug->curp.z;


    /*
     * Calculate new position using cross-correlations technique.
     */
    {
	int diffm = cmpslice(plug, 0, 0);
	int diff, diffx = 0, diffy = 0, diffz = 0;
	int i, j;
	int ioff=0, joff=0;
	int sa = 12;
	int bx, by;

	/* search small area first and check bounds. */
	for(j = joff-3; j <= joff+3; j++)
	    for(i = ioff-3; i <= ioff+3; i++){
		if ( (j == joff) && (i == ioff)) continue;
		
		diff = cmpslice(plug, i, j);
		/*wprint("\tCheck %d %d = %d\n", i, j, diff);*/
		
		if ((diff) && (diff < diffm)){
		    diffm = diff;
		    diffx = i;
		    diffy  = j;
		}
	    }

	bx = (diffx < 0) ? -diffx : diffx;
	by = (diffy < 0) ? -diffy : diffy;

	if ( (bx == 3) || ( by == 3)){
	    /* find best large area. */
	    for(j = -sa; j <= sa; j+= 4)
		for(i = -sa; i<= sa; i+= 4){
		    if ((!i) && (!j)) continue;
		    diff = cmpslice(plug, i, j);
		    if ((diff) && (diff < diffm)){
			diffm = diff;
			diffx = i;
			diffy  = j;
		    }
		}
	    
	    /* search smaller area. */
	    ioff = diffx;
	    joff = diffy;
	    for(j = joff-3; j <= joff+3; j++)
		for(i = ioff-3; i <= ioff+3; i++){
		    if ( (j == joff) && (i == ioff)) continue;
		    
		    diff = cmpslice(plug, i, j);
		    /*wprint("\tCheck %d %d = %d\n", i, j, diff);*/
		    
		    if ((diff) && (diff < diffm)){
			diffm = diff;
			diffx = i;
			diffy  = j;
		    }
		}
	}
	
	/* what should cz be set to? */
	cz = plug->cz;
	plug->mat = ivwGetZSectionTime(plug->view, cz+1, curTime);
	if (plug->mat){
	    diff = cmpslice(plug, diffx, diffy);
	    if ((diff) && (diff < diffm)){
		diffz = 1;
	    }
	}
	plug->mat = ivwGetZSectionTime(plug->view, cz-1, curTime);
	if (plug->mat){
	    diff = cmpslice(plug, diffx, diffy);
	    if ((diff) && (diff < diffm)){
		diffz = -1;
	    }
	}
	
	
	/*
	   plug->matp.x += diffx;
	   plug->matp.y += diffy;
	   plug->matp.z += diffz;
	 */
	plug->matp.x = plug->cx + diffx;
	plug->matp.y = plug->cy + diffy;
	plug->matp.z = plug->cz + diffz;
	plug->diffx = diffx;
	plug->diffy = diffy;
	plug->diffz = diffz;
	plug->diff  = diffm;
	*p = plug->matp;
    
	wprint("%s ob %d co %d pt %d correlation %d\n", 
	       imodPlugInfo(0),cob+1,cco+1,cpt+1,
	       plug->diff/100);
	wprint("Moved point by: %d %d %d\n", 
	       diffx, diffy, diffz);
   }

    imodSetIndex(imod, ico, iob, ipt);
    return(0);
}

#ifndef USEGFX
static void plugDraw(PlugData *plug)
{
}
#endif

#ifdef USEGFX

/* pixel border around images. */
#define BSIZE 2

static void plugButton1(PlugData *plug, int inX, int inY)
{
    Imod     *imod = ivwGetModel(plug->view);
    Iobj     *obj  = imodObjectGet(imod);
    Icont    *cont = imodContourGet(imod);
    Ipoint   *p    = imodPointGet(imod);
    int ob,co,pt;

    if (p){
	 int   ghn = (plug->gtime * 2) + 1; /* Number of horizontal images. */
	 int   gvn = (plug->gz * 2)    + 1; /* Number of vertical images */
	 float sw  = plug->width/(float)ghn;   /* Scaled width.  */
	 float sh  = plug->height/(float)gvn;  /* Scaled height. */
	 float dx  = -plug->gtime + (float)inX / sw;
	 float dy  = -plug->gz + (float)inY / sh;
	 int ct = cont->type;
	 float cz = p->z;


	 int gwidth  = (int)sw - (BSIZE);  /* graphics area size.     */
	 int gheight = (int)sh - (BSIZE);  /* to draw for each image. */

	 float zoom    = (float)plug->zoom * 0.01f;
	 int   xoffset = p->x-((float)gwidth*0.5f/zoom);
	 int   yoffset = p->y-((float)gheight*0.5f/zoom);

	 if (xoffset < 0)
	      xoffset = 0;
	 if (yoffset < 0)
	      yoffset = 0;

	 imodGetIndex(imod, &ob, &co, &pt);
	 imodSetIndex(imod, ob, co, -1);
	 ivwSetLocation(plug->view, (int)p->x, (int)p->y, (int)(cz + dy));
	 ivwSetTime(plug->view, ct + dx);
	 ivwControlPriority(plug->view, plug->control);
	 ivwDraw(plug->view, IMOD_DRAW_XYZ);
    }

}

Ipoint *getTimeCoord(Iobj *obj, char *pname, int time)
{
    Ipoint *p;
    Icont *cont;
    Ilabel *lab;
    int pt, co;
    char *cname;

    if ((obj == NULL) || (pname == NULL))
	return(NULL);

    for(co = 0; co < obj->contsize; co++){
	cont = &obj->cont[co];
	if (cont->type != time)
	    continue;
	lab = imodContourGetLabel(cont);
	if (lab == NULL) 
	    continue;
	for(pt = 0; pt < cont->psize; pt++){
	    cname = imodLabelItemGet(lab, pt);
	    if (cname == NULL) 
		continue;
	    if (strcmp(cname, pname) == 0)
		return(&cont->pts[pt]);
	}
    }
    return NULL;
}

static void plugDraw(PlugData *plug)
{
    int xoffset = 0, yoffset = 0;
    static GLUquadricObj *qobj = NULL;
    int linewidth = 1;
    Imod     *imod  = ivwGetModel(plug->view);
    Iobj     *obj   = imodObjectGet(imod);
    Icont    *cont  = imodContourGet(imod);
    Ipoint   *p     = imodPointGet(imod);
    Ilabel   *label = imodContourGetLabel(cont);
    char     *pname = NULL;

    unsigned char *idata;
    int ob, co,pt;
    GLenum type, format;
    

    glXMakeCurrent(XtDisplay(plug->drawingArea),
		   XtWindow(plug->drawingArea), plug->context);

    updateGLViewport(plug); 
    if (!qobj)
	qobj = gluNewQuadric();

    glClear(GL_COLOR_BUFFER_BIT);
    if (( imodObjectGetValue(obj, IobjFlagConnected)) ||	
	( !imodObjectGetValue(obj, IobjFlagTime)))
	 return;

    imodGetIndex(imod, &ob, &co, &pt);
    imodSetObjectColor(ob);
    if (label && cont && p){
	pname = imodLabelItemGet(label, pt);
    }

    if (p){
	int xdrawsize, ydrawsize;
	int xdrawstart = 0, ydrawstart = 0;
	int ct = cont->type;
	int pt = ct - 1;
	int cz = p->z;
	int t, z;
	float sw, sh;
	float zoom;
	int gwidth, gheight;
	float psize, gsize;
	float x, y, xoffset, yoffset;
	int gvn, ghn;
	float dx, dy;
	psize = obj->pdrawsize * 2;
	if (psize < 64.0f) psize = 64.0f;
	gsize = (float)psize+2.0f;

	ghn = (plug->gtime * 2) + 1; /* Number of horizontal images. */
	gvn = (plug->gz * 2)    + 1; /* Number of vertical images */

	sw = (float)plug->width/(float)ghn;   /* Scaled width.  */
	sh = (float)plug->height/(float)gvn;  /* Scaled height. */
	gwidth = (int)sw - (BSIZE);  /* The size of the graphics area. */
	gheight = (int)sh - (BSIZE); /* we will draw for each image. */

	zoom = (float)plug->zoom * 0.01f;
	xoffset = p->x-((float)gwidth*0.5f/zoom);
	yoffset = p->y-((float)gheight*0.5f/zoom);

	ivwGetImageType(plug->view, &type, &format);
	glPixelZoom(zoom, zoom);

	dx = -xoffset;
	if (xoffset < 0){
	    xdrawstart = -xoffset*zoom;
	    xoffset = 0;
	}
	dy = -yoffset;
	if (yoffset < 0){
	    ydrawstart = -yoffset*zoom;
	    yoffset = 0;
	}

	xdrawsize = gwidth/zoom;
	ydrawsize = gheight/zoom;

	/*
	if ((xdrawsize-xoffset) > (plug->nx*zoom))
	    xdrawsize = plug->nx;
	ydrawsize = gheight/zoom;
	if ((ydrawsize-yoffset) > (plug->ny*zoom))
	    ydrawsize = plug->ny;
	   */
	
	if ((xdrawsize+xoffset) > (plug->nx))
	    xdrawsize = plug->nx-xoffset;
	if ((ydrawsize+yoffset) > (plug->ny))
	    ydrawsize = plug->ny - yoffset;

	glPixelStorei(GL_UNPACK_ROW_LENGTH, plug->nx);
	glPixelStorei(GL_UNPACK_SKIP_ROWS,   yoffset);
	glPixelStorei(GL_UNPACK_SKIP_PIXELS, xoffset);

	for(x = BSIZE, t = -plug->gtime + ct; 
	    t <= (plug->gtime + ct);
	    t++,x+=gwidth+BSIZE){
	    Ipoint *tp = getTimeCoord(obj, pname, t);

	    for(y = BSIZE, z = -plug->gz + cz; 
		z <= (plug->gz + cz); 
		z++,y+=gheight+BSIZE){


		idata = ivwGetZSectionTime(plug->view, z, t);
		if (idata){
		    glRasterPos2f(x+xdrawstart, y+ydrawstart);
		    glDrawPixels(xdrawsize, 
				 ydrawsize,
				 format, type, idata);
		}else{
		    if ((t > 0) && (z >=0) && (z < plug->nz))
			wprint("No image data for section %d, time %d\n",z,t);
		}

		
		if ((z == cz) && (t == ct)){
		  
		    int linewidth = 1;
		    int radius = (float)obj->pdrawsize * zoom;

		    glLineWidth(1.0f);
		    glPushMatrix();
		    glTranslatef(x + (gwidth/2), y + (gheight/2), 0.0f);
		    glGetIntegerv(GL_LINE_WIDTH, &linewidth);
		    gluDisk(qobj, radius-linewidth, radius, radius+4, 2);
		    glPopMatrix();
		    b3dDrawPlus((int)x + (gwidth/2),
				(int)y + (gheight/2), 3);
		}else{
		    if ((t != ct) && (tp) && ((int)tp->z == z)){
			int linewidth = 1;
			int radius = (float)obj->pdrawsize * zoom;

			glLineWidth(1.0f);
			glPushMatrix();
			glTranslatef(x + (gwidth/2) +
				     ((tp->x - p->x) * zoom), 
				     y + (gheight/2) +
				     ((tp->y - p->y) * zoom), 
				     0.0f);
			glGetIntegerv(GL_LINE_WIDTH, &linewidth);
			gluDisk(qobj, radius-linewidth, radius, radius+4, 2);
			glPopMatrix();
		    }
		    if (tp){
			b3dDrawPlus((int)x + (gwidth/2) +
				    ((tp->x - p->x) * zoom),
				    (int)y + (gheight/2)+ 
				    ((tp->y - p->y) * zoom), 3);
		    }else{
			b3dDrawPlus((int)x + (gwidth/2),
				    (int)y + (gheight/2), 3);
		    }
		}
		
	    }
	}
    }

    glXSwapBuffers(XtDisplay(plug->drawingArea),
		   XtWindow(plug->drawingArea));

/* for debugging only. */
/*    wprint("Match Point draw: done\n"); */
}

/*
 *  OpenGL / Motif callback functions.
 *
 */
static void ginit_cb(Widget w, XtPointer client, XtPointer call)
{
     GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
     PlugData *plug = (PlugData *)client;
     XVisualInfo *vi;

     XtVaGetValues(w, GLwNvisualInfo, &vi, NULL);
     plug->zoom    = 100;
     plug->context = glXCreateContext(XtDisplay(w), vi, NULL, GL_TRUE);
     
     glXMakeCurrent(XtDisplay(w), XtWindow(w), plug->context);
     plug->needUpdate = True;
     glClearIndex((GLfloat)imodColorValue(COLOR_BACKGROUND));
}

void updateGLViewport(PlugData *plug)
{
    Dimension width, height;
    Widget w = plug->drawingArea;
    if (!XtIsRealized(w)) return;
    plug->needUpdate = False;

    XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
    glViewport(0, 0, (GLint) width, (GLint) height);
    
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    plug->width  = width;
    plug->height = height;
    glOrtho(0.0 , (double)width, 0.0, (double)height, 0.5, -0.5);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

}

/*  Window needs redrawing.
 */
static void expose_cb(Widget w, XtPointer client, XtPointer call)
{
    GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
    PlugData *plug = (PlugData *)client;
    
    if (!plug->context)
	return;
    plugDraw(plug);
}

/* User has resized the window. */
static void resize_cb(Widget w, XtPointer client, XtPointer call)
{
     GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
     PlugData *plug = (PlugData *)client;
     Dimension width, height;

     /* don't try OpenGL until window is realized! */
     plug->needUpdate = True;
     if (!XtIsRealized(w))
	 return;
     if (!plug->context)
          return;
     plugDraw(plug);
}

/* handle input into the drawing area. */
static void input_cb(Widget w, XtPointer client, XtPointer call)
{
     GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
     PlugData *plug = (PlugData *)client;
     Imod *imod = ivwGetModel(plug->view);
     XKeyEvent *event = (XKeyEvent *)cbs->event;
     KeySym keysym;
     int ctrl =  event->state & ControlMask;
     switch(cbs->event->type){

       case KeyPress:
          keysym = XLookupKeysym((XKeyEvent *)cbs->event, 0);
          switch(keysym){

	    case XK_q:
	    case XK_Escape:
               break;

	     case XK_m:
	       moveall_cb(plug->window, plug, plug);
	       break;

	     case XK_braceleft:
	     case XK_bracketleft:
	       if (event->state & ShiftMask)
		   first(plug, imod);
	       else
		   if (plug->cpt > 0){
		       plug->cpt--;
		       imodSetIndex(imod, plug->cob, plug->cco, plug->cpt);
		   }
	       break;

	     case XK_braceright:
	     case XK_bracketright:
	       if (event->state & ShiftMask)
		   inputLastPoint(plug->view);
	       else
		   next(plug, imod);
	       break;

	       
	     case XK_semicolon:
	     case XK_F35:
	     case XK_KP_Next:
	       inputPointMove(plug->view, 0, 0, -1);
	       break;

	     case XK_quoteright:
	     case XK_quoteleft:
	     case XK_F29:
	     case XK_KP_Prior:
	       inputPointMove(plug->view, 0, 0, 1);
	       break;

	     case XK_Up:
	     case XK_KP_Up:
	       inputPointMove(plug->view, 0, 1, 0);
	       break;

	     case XK_Down:
	     case XK_KP_Down:
	       inputPointMove(plug->view, 0, -1, 0);
	       break;

	     case XK_Right:
	     case XK_KP_Right:
	       inputPointMove(plug->view, 1, 0, 0);
	       break;

	     case XK_Left:
	     case XK_KP_Left:
	       inputPointMove(plug->view, -1, 0, 0);
	       break;

	     case XK_0:
	       if (ctrl)
		   ncontcopy(plug, 0);
	       else
		   ncopy(plug, 0);
	       break;
	     case XK_1:
	       if (ctrl)
		   ncontcopy(plug, 1);
	       else
		   ncopy(plug, 1);
	       break;
	     case XK_2:
	       if (ctrl)
		   ncontcopy(plug, 2);
	       else
		   ncopy(plug, 2);
	       break;
	     case XK_3:
	       if (ctrl)
		   ncontcopy(plug, 3);
	       else
		   ncopy(plug, 3);
	       break;
	     case XK_4:
	       if (ctrl)
		   ncontcopy(plug, 4);
	       else
		   ncopy(plug, 4);
	       break;
	     case XK_5:
	       if (ctrl)
		   ncontcopy(plug, 5);
	       else
		   ncopy(plug, 5);
	       break;
	     case XK_6:
	       if (ctrl)
		   ncontcopy(plug, 6);
	       else
		   ncopy(plug, 6);
	       break;
	     case XK_7:
	       if (ctrl)
		   ncontcopy(plug, 7);
	       else
		   ncopy(plug, 7);
	       break;
	     case XK_8:
	       if (ctrl)
		   ncontcopy(plug, 8);
	       else
		   ncopy(plug, 8);
	       break;
	     case XK_9:
	       if (ctrl)
		   ncontcopy(plug, 9);
	       else
		   ncopy(plug, 9);
	       break;
	       
	     case XK_minus:
	       plug->zoom -= 25;
	       if (plug->zoom < 25) plug->zoom = 25;
	       break;

	     case XK_equal:
	       plug->zoom += 25;
	       break;

	     case XK_t:
	       if (event->state & ShiftMask)
		   plug->gtime++;
	       else
		   plug->gtime--;
	       if (plug->gtime < 0) plug->gtime = 0;
	       break;

	     case XK_z:
	       if (event->state & ShiftMask)
		   plug->gz++;
	       else
		   plug->gz--;
	       if (plug->gz < 1) plug->gz = 1;
	       break;

             default: /* pass keys to imod */
               ivwControlActive(plug->view, 0);
               inputDefaultKeys((XKeyEvent *)cbs->event, plug->view);
               break;
	       
	   }
	  ivwControlPriority(plug->view, plug->control);
	  ivwDraw(plug->view, IMOD_DRAW_RETHINK);
	  break;

	case EnterNotify:
	  XmProcessTraversal(plug->drawingArea, XmTRAVERSE_CURRENT);
	  break;

        case ButtonPress:
	  XmProcessTraversal(plug->drawingArea, XmTRAVERSE_CURRENT);
          switch(cbs->event->xbutton.button){
	    case Button1:
	      plugButton1(plug, cbs->event->xbutton.x,cbs->event->xbutton.y);
	      break;

	    case Button2:
	    case Button3:
               break;
	   }
      }
}
#endif
static void ncopy_cb(Widget w, XtPointer client, XtPointer call)
{
    int nofcopy = (int)client;
    PlugData *plug = &thisPlug;
    ncopy(plug, nofcopy);
}
static void ncontcopy_cb(Widget w, XtPointer client, XtPointer call)
{
    int nofcopy = (int)client;
    PlugData *plug = &thisPlug;
    ncontcopy(plug, nofcopy);
}
static void timeset_cb(Widget w, XtPointer client, XtPointer call)
{
    int up = (int)client;
    PlugData *plug = &thisPlug;

    if (up)
	plug->gtime++;
    else
	plug->gtime--;
    if (plug->gtime < 0) plug->gtime = 0;
    plugDraw(plug);
}
static void secset_cb(Widget w, XtPointer client, XtPointer call)
{
    int up = (int)client;
    PlugData *plug = &thisPlug;

     if (up)
	 plug->gz++;
     else
	 plug->gz--;
    if (plug->gz < 1) plug->gz = 1;
    plugDraw(plug);
}

static Widget makeItem(char *label, Widget w, char mn,
		       char *acc, char *acct)
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
    return item;
}
static void zoomset_cb(Widget w, XtPointer client, XtPointer call)
{
    int up = (int)client;
    PlugData *plug = &thisPlug;

    if (up)
	plug->zoom += 25;
    else
	plug->zoom -= 25;

    if (plug->zoom <= 0)
	plug->zoom = 25;
    if (plug->zoom > 500)
	plug->zoom = 500;
    plugDraw(plug);
}

static Widget makeMenuBar(Widget w)
{
    PlugData *plug = &thisPlug;
    Widget menubar, cascade, item, cMenu;
    Widget fileMenu, editMenu, modelMenu, contourMenu;
    int n = 0;
    Arg args[10];

    XmString filestr    = XmStringCreateSimple("File");
    XmString editstr    = XmStringCreateSimple("Edit");
    XmString modelstr   = XmStringCreateSimple("Model");
    XmString contourstr = XmStringCreateSimple("Contour");
    
    int i;
    char mlabel[32];
    char alabel[32];
    char tlabel[32];

    XtSetArg(args[n], XmNdepth,    imodDepth()); n++;
    XtSetArg(args[n], XmNvisual,   imodVisual()); n++;
    XtSetArg(args[n], XmNcolormap, imodColormap());n++;

    menubar = XmCreateMenuBar(w, "menubar", args, n);
    
    cMenu = fileMenu = XmCreatePulldownMenu(menubar, "file_pulldown", args, n);
    {
	cascade = XtVaCreateManagedWidget
	    ("cascade", xmCascadeButtonWidgetClass, menubar,
	     XmNsubMenuId, fileMenu,
	     XmNlabelString, filestr,
	     XmNmnemonic, 'F',
	     NULL);

	item = makeItem("About this plugin...", 
			fileMenu, 'A', NULL, NULL);
	XtAddCallback(item,  XmNactivateCallback, about_cb, plug);
	XtVaCreateManagedWidget
	    ("separator", xmSeparatorWidgetClass,  fileMenu, NULL);
	item = makeItem("Close Window", fileMenu, 'W', "Ctrl<Key>W", "Ctrl-W");
	XtAddCallback(item, XmNactivateCallback, quit_cb, plug);
	XmStringFree(filestr);
/*	XtManageChild(fileMenu); */
    }
    
    cMenu =editMenu = XmCreatePulldownMenu(menubar, "epulldown", args, n);
    {
	cascade = XtVaCreateManagedWidget
	    ("cascade", xmCascadeButtonWidgetClass, menubar,
	     XmNsubMenuId, editMenu, XmNlabelString, editstr, 
	     XmNmnemonic, 'E',
	     NULL);
	item = makeItem("First Point", editMenu, 'F', "Ctrl<Key>F", "Ctrl-F");
	XtAddCallback(item, XmNactivateCallback, first_cb, plug);

	item = makeItem("Next", editMenu, 'N', "Ctrl<Key>N", "Ctrl-N");
	XtAddCallback(item, XmNactivateCallback, next_cb, plug);

	item = makeItem("Match", editMenu, 'M', "Ctrl<Key>H",  "Ctrl-H");
	XtAddCallback(item, XmNactivateCallback, locate_cb, plug);

	item = makeItem("Match to End",  editMenu,  'E',
			 "Ctrl<Key>E",  "Ctrl-E");
	XtAddCallback(item, XmNactivateCallback, moveptime_cb, plug);

	item = XtVaCreateManagedWidget
	    ("separator", xmSeparatorWidgetClass,  editMenu, NULL);

	item = makeItem("Zoom Up", editMenu, 'U', NULL, "=");
	XtAddCallback(item, XmNactivateCallback, zoomset_cb, (XtPointer)1);
	item = makeItem("Zoom Down", editMenu, 'D', NULL, "-");
	XtAddCallback(item, XmNactivateCallback, zoomset_cb, (XtPointer)0);

	item = XtVaCreateManagedWidget
	    ("separator", xmSeparatorWidgetClass,  editMenu, NULL);

	item = makeItem("More Time Panes", editMenu, 'M', NULL, "T");
	XtAddCallback(item, XmNactivateCallback, timeset_cb, (XtPointer)1);
	item = makeItem("Fewer Time Panes", editMenu, 'F', NULL, "t");
	XtAddCallback(item, XmNactivateCallback, timeset_cb, (XtPointer)0);

	item = XtVaCreateManagedWidget
	    ("separator", xmSeparatorWidgetClass,  editMenu, NULL);

	item = makeItem("More Section Panes", editMenu, 'o', NULL, "Z");
	XtAddCallback(item, XmNactivateCallback, secset_cb, (XtPointer)1);
	item = makeItem("Fewer Section Panes", editMenu, 'e', NULL, "z");
	XtAddCallback(item, XmNactivateCallback, secset_cb, (XtPointer)0);


	XmStringFree(editstr);
	XtManageChild(editMenu);
    }

    cMenu = modelMenu = XmCreatePulldownMenu(menubar, "mpulldown", args, n);
    {
	cascade = XtVaCreateManagedWidget
	    ("cascade", xmCascadeButtonWidgetClass, menubar,
	     XmNsubMenuId, modelMenu, XmNlabelString, modelstr, 
	     XmNmnemonic, 'M',
	     NULL);

	item = makeItem("Copy and Move", cMenu, 'C', NULL, "1");
	XtAddCallback(item, XmNactivateCallback, copy_cb, plug);

    	item = makeItem("Move", cMenu, 'M', NULL, "M");
	XtAddCallback(item, XmNactivateCallback, moveall_cb, plug);

	XtVaCreateManagedWidget
	    ("separator", xmSeparatorWidgetClass,  modelMenu, NULL);

	for(i = 2; i < 10; i++){
	    sprintf(mlabel, "Copy & Move %d Times", i);
	    sprintf(alabel, "%d", i);
	    sprintf(tlabel, "%d", i);
	    item = makeItem(mlabel, cMenu, tlabel[0], alabel, tlabel);
	    XtAddCallback(item, XmNactivateCallback, ncopy_cb, 
			  (XtPointer)(i));
	}
	XmStringFree(modelstr);
	XtManageChild(modelMenu);
    }

    cMenu = contourMenu = XmCreatePulldownMenu(menubar, "cpulldown", args, n);
    {
	cascade = XtVaCreateManagedWidget
	    ("cascade", xmCascadeButtonWidgetClass, menubar,
	     XmNsubMenuId, contourMenu, XmNlabelString, contourstr,
	     XmNmnemonic, 'C',
	     NULL);
	item = makeItem("Copy and Move", cMenu, 'C', "Ctrl<Key>1", "Ctrl-1");
	XtAddCallback(item, XmNactivateCallback, copycont_cb, plug);

	item = makeItem("Move", cMenu, 'M', "Ctrl<Key>M", "Ctrl-M");
	XtAddCallback(item, XmNactivateCallback, movecont_cb, plug);
	
	item = XtVaCreateManagedWidget
	    ("separator", xmSeparatorWidgetClass,  contourMenu, NULL);

	for(i = 2; i < 10; i++){
	    sprintf(mlabel, "Copy & Move %d Times", i);
	    sprintf(alabel, "Ctrl<Key>%d", i);
	    sprintf(tlabel, "Ctrl-%d", i);
	    item = makeItem(mlabel, cMenu, tlabel[5], alabel, tlabel);
	    XtAddCallback(item, XmNactivateCallback, ncontcopy_cb, 
			  (XtPointer)(i));
	}

	XmStringFree(contourstr);
	XtManageChild(contourMenu);
    }


    XtManageChild(menubar);
    return 0;
}


static Widget makeWorkArea(Widget parent)
{
    Widget window, frame;
    Widget container;
    Widget button;
    Widget row;
    PlugData *plug;

    plug = &thisPlug;

    window = XtVaCreateWidget
	("form", xmFormWidgetClass, parent,
	 NULL);

    frame = XtVaCreateWidget
	("frame", xmFrameWidgetClass,  window,
	 XmNtopAttachment,    XmATTACH_FORM,
	 XmNleftAttachment,   XmATTACH_FORM,
	 XmNrightAttachment,  XmATTACH_FORM,
	 XmNshadowType, XmSHADOW_OUT,
	 NULL);
    
    container = XtVaCreateWidget
        ("container", xmRowColumnWidgetClass, frame, 
	 NULL);
    
    makeMenuBar(container);

#ifdef USE_TOOL_BUTTONS
    row = XtVaCreateWidget
	("row", xmRowColumnWidgetClass, container, 
	 XmNorientation, XmHORIZONTAL,
	 NULL);
    {

	button = XtVaCreateManagedWidget
	    ("Copy/Move Model", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, copy_cb, plug);
	
	button = XtVaCreateManagedWidget
	    ("Move Model", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, moveall_cb, plug);
    }
    XtManageChild(row);

    XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass,
                            container, NULL);

    row = XtVaCreateWidget
        ("row", xmRowColumnWidgetClass, container,
         XmNorientation, XmHORIZONTAL,
         NULL);
    {
	button = XtVaCreateManagedWidget
            ("Copy/Move Contour", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, copycont_cb, plug);


	button = XtVaCreateManagedWidget
            ("Move Contour", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, movecont_cb, plug);

    }
    XtManageChild(row);
    

    XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass,
                            container, NULL);

    row = XtVaCreateWidget
	("row", xmRowColumnWidgetClass, container,
	 XmNorientation, XmHORIZONTAL,
	          NULL);
    {
	button = XtVaCreateManagedWidget
	    ("First Point", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, first_cb, plug);
	
	button = XtVaCreateManagedWidget
	    ("Next", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, next_cb, plug);
	
	plug->wLocate = XtVaCreateManagedWidget
	    ("Match", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(plug->wLocate, XmNactivateCallback, locate_cb, plug);

	button = XtVaCreateManagedWidget
	    ("Match to End", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, moveptime_cb, plug);

    }
    XtManageChild(row);
#endif
/*    
    XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass,
                            container, NULL);

    row = XtVaCreateWidget
	("row", xmRowColumnWidgetClass, container,
	 XmNorientation, XmHORIZONTAL,
	 NULL);
    plug->wUndo = XtVaCreateManagedWidget
        ("Undo", xmPushButtonWidgetClass, row, NULL);
    XtAddCallback(plug->wUndo, XmNactivateCallback, undo_cb, plug);
    XtSetSensitive(plug->wUndo, False);
    
    button = XtVaCreateManagedWidget
        ("Help", xmPushButtonWidgetClass, row, NULL);
    XtSetSensitive(button, False);

    button = XtVaCreateManagedWidget
        ("Close", xmPushButtonWidgetClass, row, NULL);
    XtAddCallback(button, XmNactivateCallback, quit_cb, plug);
    XtManageChild(row);
*/

#ifdef USEGFX


    plug->drawingArea = XtVaCreateManagedWidget
          ("gfx",  glwMDrawingAreaWidgetClass, window,
	   XmNbottomAttachment,   XmATTACH_FORM,
	   XmNleftAttachment,     XmATTACH_FORM,
	   XmNrightAttachment,    XmATTACH_FORM,	   
	   XmNtopAttachment,      XmATTACH_WIDGET,
	   XmNtopWidget,          frame,
	   XmNwidth,              plug->width,
	   XmNheight,             plug->height,
	   XmNcolormap,           imodColormap(),
           XmNdepth,              imodDepth(),
	   XmNnavigationType,     XmNONE,
	   XmNtraversalOn,        True,
	   XmNtranslations,       XtParseTranslationTable (plugTranslations),
           GLwNvisualInfo,        imodVisualInfo(),
           GLwNinstallBackground, False,
           GLwNinstallColormap,   False,
           NULL);
    
     /* Setup
      * OpenGL Motif widget callback functions.
      */
     XtAddCallback(plug->drawingArea,
                   GLwNginitCallback,
                   ginit_cb, (XtPointer)plug);
     XtAddCallback(plug->drawingArea,
                   GLwNexposeCallback,
                   expose_cb, (XtPointer)plug);
     XtAddCallback(plug->drawingArea,
                   GLwNresizeCallback,
                   resize_cb, (XtPointer)plug);
     XtAddCallback(plug->drawingArea,
                   GLwNinputCallback,
                   input_cb, (XtPointer)plug);
    imodOverrideTranslations(plug->drawingArea,
			     XtParseTranslationTable(plugTranslations));
    
#endif

    XtManageChild(container);
    XtManageChild(frame);
    XtManageChild(window);
    return 0;
}
