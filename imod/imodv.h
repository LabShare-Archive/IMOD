/*  IMOD VERSION 2.50
 *
 *  imodv.h -- Main imodv include file.
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
    Revision 3.2  2002/09/04 00:26:25  mast
    Added declarations for imodv_init and imodvGetVisuals

    Revision 3.1  2002/07/18 20:19:38  rickg
    Changed include of GLwMDrawA to rely upon -I compiler option

*/

#ifndef IMODV_H
#define IMODV_H

#include <limits.h>
#include <time.h>
#include <X11/Xlib.h>
#include <Xm/Xm.h>
#include <GL/gl.h>
#include <GLwMDrawA.h>
#include <imodconfig.h>

#include <dia.h>          /* local dialog library. */
#include <imodel.h>       /* imod library include. */

/* used for finding bounding box. */
#ifndef FLT_MAX
#include <limits.h>
#ifndef FLT_MAX
#define FLT_MAX 5e+29f
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct __imodv_struct
{
     /* model data.   */
     Imod **mod;     /* model array              */
     Imod *imod;     /* current model            */
     int  nm;        /* number of models         */
     int  cm;        /* current model number     */
     
     Iobj *obj;      /* Current object edit.     */
     int    ob;      /* Current obj number       */
     Iobj *dobj;     /* default obj=bounding box */
     struct Mod_Draw  *md;  /* transformations   */
     Imat  *mat;     /* Transformation matrix storage.   */
     Imat  *rmat;    /* rotation matrix. */
     Ipoint minp;    /* bouding box for model.   */
     Ipoint maxp;
     float  r;       /* max radius of bounding box */
     int    dlist;   /* use display list           */
     int    update_dlist; /* update display list    */

     /* windowing data */
     XtAppContext context;
     XtWorkProcId wpid;
     Display      *display;
     Visual       *visual;
     XVisualInfo  *visualInfoSB;
     XVisualInfo  *visualInfoDB;
     int          depth;
     Colormap     cmap;
     Colormap     gcmap;
     Widget       topLevel;
     Widget       mainWin;
     Widget       form;     /* form contains drawing area.        */
     Widget       cgfx;     /* current drawing area.              */
     Widget       dgfx;     /* double-buffer rgb drawing area.    */
     Widget       gfx;      /* single-buffer rgb drawing area.    */
     Widget       cigfx;    /* double-buffer color-index (unused) */
     Widget       menubar;
     Widget       popup;
     Widget       objnumber;
     Widget       objname;
     GLXContext   gc;      /* OpenGL Graphics contexts.           */
     GLXContext   dgc;     /* double buffer gc                    */
     GLXContext   cigc;    /* color index graphics context.       */
     char         *rbgname;
     XColor       rbgcolor; /* background color for rendering.    */
     int          dgfx_init;
     int          gfx_init;


     _XtString stereoCommand;
     _XtString restoreCommand;
     _XtString SGIStereoCommand;
     _XtString SGIRestoreCommand;
	      

     /* global viewing flags */
     int cnear;       /* clipping planes.                        */
     int cfar;
     int fovy;        /* field of view angle for perspective.    */
     int db;          /* use doublebuffer widget if true         */
     int hidemenu;    /* flag: popup menu if true; else menubar. */
     int winx, winy;  /* current drawing window size.            */
     int wscale;      /* scale size into window.                 */
     int wxt, wyt, wzt; /* translations into current window.     */
     int lmx, lmy;    /* last x,y mouse location.                */
     int lightx,
         lighty;

                      /* -1, -2 .. worse rendering.              */
     int fastdraw;    /* 0 = standard rendering.                 */
                      /* 1 = better rendering.                   */
		      /* 2 = best rendering.                     */
     int mousemove;   /* 0 = move model, 1 light, 2 clip.        */
     int stereo;      /* 0 = no stereo view.                     */
                      /* 1 = cross, 2 = wall, 3 = red/green      */
                      /* 4 = display hardware stereo.            */
     float plax;      /* parallax for stereo separation.         */
     int movie;       /* allow movies.                           */
     int drawall;     /* draw all models at once.                */
     int cstart;      /* colorindex start.                       */
     int cstep;       /* colorindex step.                        */
     int bindex;      /* background colorindex.                  */
     int alpha;       /* number of alpha planes.                 */
     int current_subset;  /* display subset of model (current element) */
                          /* 0 = all, 1 = obj, 2 = surf, 3 = cont */
     int movieFrames;    /* Number of movie frames displayed     */
     clock_t movieStart; /* Starting CPU time of movie           */
     clock_t movieCurrent; /* Current CPU time of movie          */

     /* start-up flags */
     int  moveall;    /* move all models if true.                 */
     int  crosset;    /* edit all models if true.                 */
     int  noborder;   /* open with no window decorations if true. */
     int  fullscreen; /* open full sized screen with no border.   */
     int  standalone; /* type of program being used.              */
     int  cindex;     /* use colorindex mode instead of true color. */
     Iview view;

     /* texture mapping */
     int    texMap;
     int    texTrans;
     struct ViewInfo *vi;

     /* picking */
     int doPick;
     int xPick, yPick;
     int wPick, hPick;

     int lighting;
     int depthcue;
     int wireframe;
     int lowres;
     int first, dfirst;

}ImodvApp;


extern ImodvApp *Imodv;
extern int ImodvClosed;

int  imodv_main(int argc, char **argv);
void imodv_movie(ImodvApp *a);
void imodv_exit(ImodvApp *a);

void imodv_resize_cb(Widget w, XtPointer client, XtPointer call);
void imodv_expose_cb(Widget w, XtPointer client, XtPointer call);
void imodv_ginit_cb(Widget w, XtPointer client, XtPointer call);
int  imodv_init_drawing_widget(ImodvApp *a, Widget form);
void imodv_clear(ImodvApp *a);
void imodv_setbuffer(ImodvApp *a);
void imodv_viewtrans(int fovy, int r, int cnear, int cfar);
void imodvDraw(ImodvApp *a);
void imodvDraw_model(ImodvApp *a, Imod *imod);
void imodvDraw_object(Iobj *obj, Imod *imod);
void imodvDraw_contours(Iobj *obj, int mode);
void imodvDraw_filled_contours(Iobj *obj);
int imodv_snapshot(ImodvApp *a, char *fname);
int imodv_auto_snapshot(char *inName, int format_type);
void imodv_reset_snap_cb(Widget w, XtPointer client, XtPointer call);
void imodvSetViewbyModel(ImodvApp *a, Imod *imod);
int imodv_winset(ImodvApp *a);

/* menu.c functions */
Widget imodv_create_menu(ImodvApp *a);
Widget imodv_create_popup(ImodvApp *a);
void imodvMenuLight(int value);
void imodvMenuWireframe(int value);
void imodvMenuLowres(int value);
void imodv_file_save_cb(Widget w, XtPointer client, XtPointer call);
void menu_bgcolor_cb(Widget w, XtPointer client, XtPointer call);

/* input.c functions */
void imodv_input_cb(Widget w, XtPointer client, XtPointer call);
void imodv_light_move(ImodvApp *a);
void imodv_translate(ImodvApp *a, int x, int y);
void imodv_translated(ImodvApp *a, int x, int y, int z);
void imodv_rotate_model(ImodvApp *a, int x, int y, int z);
void imodv_compute_rotation(ImodvApp *a, float x, float y, float z);
void imodv_rotate(ImodvApp *a, int throwFlag);
void imodv_zoomd(ImodvApp *a, double zoom);
void objed(ImodvApp *a);
int object_edit_kill(void);
void imodvModelEditDialog(ImodvApp *a, int state);
clock_t imodv_sys_time(void);
void ximodv_quit_cb(Widget w, XtPointer client, XtPointer call);

/* control.c */
int imodv_control(ImodvApp *a, int state);
int imodvSelectModel(ImodvApp *a, int ncm);
void imodvControlSetArot(ImodvApp *a, int newval);
void imodvControlSetView(ImodvApp *a);
void imodvControlUpdate(ImodvApp *a);
int imodvGetVisuals(ImodvApp *a);
int imodv_init(ImodvApp *a, struct Mod_Draw *md);
void imodvMapModel(ImodvApp *a, Imod *imod);
void imodvMapColor(Widget w,  unsigned int color,
		   unsigned short red,
		   unsigned short green,
		   unsigned short blue);

/* imodv_objed.c: object editing functions */
/* update the display after a new view is selected */
void imodvObjedNewView(void);
void imodvObjectListDialog(ImodvApp *a, int state);

/* view editing functions */
void imodvUpdateView(ImodvApp *a);
void imodvUpdateModel(ImodvApp *a);
void imodvViewEditDialog(ImodvApp *a, int state);
void imodvAutoStoreView(ImodvApp *a);

void imodvMovieDialog(ImodvApp *a, int state);

/* light functions */
void light_init(void);
void light_getparam(int param, float *outValue);
void light_setparam(int param, double value);
void light_move(int *x, int *y);
int clip_obj(Iobj *obj, int flag, double zscale, double zoom);
void light_on(struct Mod_Object *obj);
void light_off(void);
void imodvSetLight(Iview *vw);

/* depth cue functions */
void imodvDepthCueSet(void);
void imodvDepthCueSetWidgets(void);

#define IMODV_MAX_INDEX 225


/* Stereo Control functions. */
#define IMODV_STEREO_OFF 0
#define IMODV_STEREO_RL  1
#define IMODV_STEREO_TB  2
#define IMODV_STEREO_HW  3

void imodvStereoEditDialog(ImodvApp *a, int state);
void imodvStereoToggle(void);
void stereoHWOff(void);

/* Image Control functions. */
void imodvDrawImage(ImodvApp *a);
void imodvImageEditDialog(ImodvApp *a, int state);

/* background color edit callback function. */
void imodv_setbgcolor(Widget w, XtPointer client, XtPointer call);
     

/*
 *  Callback functions for imodv.
 */
typedef struct
{
     void (*func)(ImodvApp *, XtPointer, int);
     XtPointer client;
}ImodvCallBack;

/*
 *  Destroy callback called when imodv is closed.
 */
void imodvAddCloseCB(void (*func)(ImodvApp *, XtPointer, int),
		     XtPointer client);
void imodvRemoveCloseCB(void (*func)(ImodvApp *, XtPointer, int),
			XtPointer client);
void imodvCallCloseCB(void);

/*
 *  Draw model callback, with given reason.
 */
#define IMODV_DRAWCB_UNKNOWN    0
#define IMODV_DRAWCB_TRANSFORM 10
#define IMODV_DRAWCB_MODEL     20
#define IMODV_DRAWCB_OBJECT    30
#define IMODV_DRAWCB_CONTOUR   40

void imodvAddDrawCB(void (*func)(ImodvApp *, XtPointer, int),
		    XtPointer client);
void imodvRemoveDrawCB(void (*func)(ImodvApp *, XtPointer, int),
		       XtPointer client);
void imodvCallDrawCB(int reason);

#ifdef __cplusplus
}
#endif

#endif /* imodv.h */
