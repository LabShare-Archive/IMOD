/*  IMOD VERSION 2.50
 *
 *  imod.h -- Main header file for imod.
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
    Revision 3.7  2002/11/25 19:19:06  mast
    Eliminated conditional on USE_IMOD_CONTROL

    Revision 3.6  2002/10/23 15:56:23  mast
    Blew away part of file before previous checkin

    Revision 3.5  2002/10/22 22:45:34  mast
    Removed some declarations from old workproc routines

    Revision 3.4  2002/10/22 22:39:24  mast
    *** empty log message ***

    Revision 3.3  2002/09/19 22:53:36  rickg
    Added MESSAGE_QUIT define

    Revision 3.2  2002/09/13 20:57:45  mast
    Added defines for MESSAGES's, removed redundant declarations that are also
    in imod_io.h

*/

#ifndef IMODP_H
#define IMODP_H

#include <Xm/Xm.h>

#include <imodconfig.h>

#define Colorindex unsigned short
#define Device     unsigned short

#include <stdio.h>
#include <imodel.h> 
#include <imodi.h>
#include <dia.h>
#include "b3dgfx.h"
#include "autox.h"
#include "imod_io.h"
#include "sslice.h"
#include "xxyz.h"

typedef struct imod_application
{
     int nv;               /* Number of view data structures. */
     int cv;               /* current view number.            */
     struct ViewInfo *vi;  /* Array of views, size n.         */
     struct ViewInfo *cvi; /* current view                    */     

     Display     *display; /* Display charactoristics.        */
     Visual      *visual;
     XVisualInfo *visualinfo;
     Colormap     cmap;
     Widget       toplevel;
     XtAppContext context;
     int          depth;
     int          doublebuffer;
     int          rgba;
     Visual      *visualGL;
     XVisualInfo *visualinfoGL;
     Colormap     cmapGL;

     /* Global color pixel values */
     unsigned int base;
     unsigned int objbase;
     unsigned int curobj;
     unsigned int background;
     unsigned int foreground;
     unsigned int select;
     unsigned int shadow;
     unsigned int endpoint;
     unsigned int bgnpoint;
     unsigned int curpoint;
     unsigned int ghost;
     unsigned int imodvbgcolor;

     /* gl window storage */
     int current_window;
     int imod_window;
     int tilt_window;
     int zap_window;

     Cursor cursor_cross;
     short wzoom;

}ImodApp;
extern ImodApp *App;

typedef struct
{
     /* Resources */
     char         *rbgname; /* background color */
     Bool    wzoom;  

#ifdef __sgi
     _XtString    SGIStereoCommand;
     _XtString    SGIRestoreCommand;
#endif
}ImodResourceStruct;

extern ImodResourceStruct ImodResource;

/* Each window that shows the view below uses this control 
 * stucture to have the view update the window.
 */
typedef void (*ImodControlProc)(struct ViewInfo *, void *, int);

typedef struct
{
     void *userData;
     ImodControlProc draw_cb;
/*     void (*draw_cb)(struct ViewInfo *vi, void *user_data, int drawflag); */
     void (*close_cb)(struct ViewInfo *vi, void *user_data, int status);
     int  id;
     int  status;

}ImodControl;

/* This structure sits inside of each view and is the
 * master controller.
 */
typedef struct
{
     Ilist *      list;
     int          active;
     int          top;
     int          reason;
     XtWorkProcId workID;
}ImodControlList;


typedef struct
{
     int     cz;
     int     ct;
     int     used;
     Islice *sec;

}ivwSlice;


/* 
 * The view that imod used to store all image and model data.
 */
typedef struct ViewInfo
{
     unsigned char **idata;  /* 8 bit 3-D grey scale data. */

     int   xsize, ysize, zsize;      /* Size of idata */
     int   xysize;                   /* section size. */
     float  xmouse, ymouse, zmouse;   /* Current point in idata. */

     int   nt, ct; /* number of time frames, current time.       */
     int   nw, cw; /* number of wavelenghs, current wavelength.  */

     struct LoadInfo *li; 
     ImodImageFile   *image;
     ImodImageFile   *imageList;
     ImodImageFile   *hdr;

     int      vmSize;            /* virtual memory z-section size. */
     ivwSlice *vmCache;          /* the cache of z-section data.   */
     int      vmCount;           /* Use counter for cache */
     int      vmLastUsed;        /* Index of last accessed section */

     /* Image data scaleing for gray scale images. */
     int    rampbase;
     int    rampsize;
     int    black;
     int    white;

     /* motion control */
     int movierate;
     int xmovie, ymovie, zmovie, tmovie;
     int movieWorkProc;
     XtIntervalId  movieTimeOut;
     int (*movieProc)();
     unsigned int movieInterval;

     /* XYZ slice points. */
     struct imod_showslice_struct slice;
     struct imod_showslice_struct lslice;

     /* Grey Scale Ramp Data. */
     Cramp *cramp;

     /* THE MODEL */
     Imod  *imod;

     /* Extra Window Data. */
     struct xxyzwin *xyz;
     struct zapwin  *zap;
     Autox  *ax;
     ImodControlList *ctrlist;

     /* Some Flags. */
     int    dim;         /* bits 0..4, x, y, z, t */
     int    obj_moveto;  /* default object to move contour to. */
     int    ghostmode;
     int    ghostlast;   /* last value of mode, when toggled by g */
     int    insertmode;  /* insert points before/after current point. */
     int    fastdraw;    
     int    drawcursor;
     int    ifd;

     
     int      flippable;     /* Flag that images can be y-z flipped */
     short    fakeImage;     /* No real image data. */
     short    rawImageStore; /* the MRC_MODE in which the raw image is stored. 
			      * if not 0, data will be cached.
			      * 0  = unsigned bytes.
			      * 16 = color rgb unsigned byte triplets.
			      */

     /* Added to fix problem in version 2.00 Beta 5 */
/*     ImodImageFile *imageList;*/
     int            imageSize;

     FILE   *fp;                /* current image file pointer.    */

}ImodView;


#define IMOD_CTRL_OPEN 1
#define IMOD_CTRL_DONE 0


/*****************************************************************************/

#define IMOD_ZOOM_COMMAND "zoom &"

#define IMOD_SELSIZE 15   /* Distance for selecting model points with mouse. */
#define IMOD_AUTOSAVE_MINUTES 3

/*****************************************************************************/

extern char   Tltwind[128];
extern struct ViewInfo *XYZ_vi;
extern struct ViewInfo *Tilt_vi;
extern struct ViewInfo *Imod_vi;
extern FILE *Imod_Imagefp;
extern struct MRCheader Imod_hdata;

extern struct TiltInfo Tilts;
extern int MouseMode;
extern int Stereo;
extern int Ghostmode;
extern int Imod_winfreeze;
extern int ImodTrans;

/* globels from imod_draw.c */
extern int Imod_Menu;
extern int Modelmenu;
extern int Objmenu;
extern int Contmenu;
extern int Pointmenu;
extern int Imagemenu;
extern int Helpmenu;

extern char *Imod_autosave_string;
extern char *Imod_imagefile;
extern char *Imod_cwdpath;
extern char *Imod_IFDpath;

/* globals from imod_input.c */
extern int Imod_obj_moveto;

/* globals from imod_autox.c */
extern int Imod_info_quit;

extern int Rampbase;

#define MOVIE_DEFAULT 52965
#define IMOD_MM_TOGGLE 0
#define IMOD_LOAD_TILT 3

#define IMOD_GHOST_NEXTSEC (1)
#define IMOD_GHOST_PREVSEC (2)
#define IMOD_GHOST_SECTION (3)
#define IMOD_GHOST_SURFACE (1<<2)

#define MESSAGE_NO_ACTION   0
#define MESSAGE_OPEN_MODEL  1
#define MESSAGE_SAVE_MODEL  2
#define MESSAGE_VIEW_MODEL  3
#define MESSAGE_QUIT  4

/* Colors for 12-bit colormap systems */
#define RAMPBASE 256
#define RAMPSIZE 256  
#define RAMPSTEP 1
#define BSIZE    6    /* Border Size */
#define RCOLOR(rcolor) ((rcolor) + RAMPBASE)
#define REDC(c)   (((c) | 0x000000ff))
#define GREENC(c) (((c) | 0x0000ff00) >> 8)
#define BLUEC(c)  (((c) | 0x00ff0000) >> 16)
#define ALPHAC(c) (((c) | 0xff000000) >> 24)

#define IMOD_BASE                  266   /* Colorindex base to model colors. */
#define IMOD_COLOR_BG              -1
#define IMOD_COLOR_FG              -2
#define IMOD_COLOR_SELECT          -3    /* Offsets to IMOD_BASE.            */
#define IMOD_COLOR_SELECT_SHADOW   -4
#define IMOD_COLOR_END             -5
#define IMOD_COLOR_BEGIN           -6
#define IMOD_COLOR_POINT           -7
#define IMOD_COLOR_GHOST           -8

/* colors for 8-bit systems */
#define IMOD_MAX_INDEX  236
#define IMOD_VIEWBG     (IMOD_MAX_INDEX - 1)
#define IMOD_GHOST      (IMOD_MAX_INDEX - 2)
#define IMOD_CURPOINT   (IMOD_MAX_INDEX - 3)
#define IMOD_BGNPOINT   (IMOD_MAX_INDEX - 4)
#define IMOD_ENDPOINT   (IMOD_MAX_INDEX - 5)
#define IMOD_SHADOW     (IMOD_MAX_INDEX - 6)
#define IMOD_SELECT     (IMOD_MAX_INDEX - 7)
#define IMOD_FOREGROUND (IMOD_MAX_INDEX - 8)
#define IMOD_BACKGROUND (IMOD_MAX_INDEX - 9)
#define IMOD_CUROBJ     (IMOD_MAX_INDEX - 10)
#define RAMPMAX         (IMOD_MAX_INDEX - 11)
#define RAMPMIN         101
#define IMOD_MIN_INDEX  16

#define SELECT_COLOR 0x0000ffff
#define SHADOW_COLOR 0x00007f7f
#define ENDPNT_COLOR 0x000000ff
#define BGNPNT_COLOR 0x007fff7f
#define FORGND_COLOR 0x00bfbfbf
#define BAKGND_COLOR 0x003f3f3f
#define CURPNT_COLOR 0x00000000

#ifndef X
#define X 0
#endif

#ifndef Y
#define Y 1
#endif

#ifndef Z
#define Z 2
#endif


/* Global Variables */
extern struct Mod_Model *Model;
extern int   Imod_Window;
extern char   Imod_filename[256];
extern int    Modeltouch;


/****************************************************************************/
/* Public functions that can be used via plugin.                            */
#include <imod.h>

/****************************************************************************/
/* Private functions for internal imod use.                                 */
#ifdef __cplusplus
extern "C" {
#endif

void imod_quit(void);

/* imodview.c private control functions */
void ivwControlListDrawCancel(ImodView *iv);
void ivwControlListDraw(ImodView *iv, int reason);
void ivwControlListDelete(ImodView *iv);

unsigned char *ivwGetCurrentSection(ImodView *iv);
int ivwInitCache(ImodView *vi);

void ivwBindMouse(ImodView *vw);

int  ivwScale(ImodView *vw);
int  ivwFlip(ImodView *vw);
void ivwInit(ImodView *vi);
int  ivwPointVisible(ImodView *vw, Ipoint *pnt);
float ivwGetFileValue(ImodView *vw, int cx, int cy, int cz);

int  imodImageFileDesc(FILE *fin);
int  ivwLoadImage(ImodView *iv);
void ivwFlushCache(ImodView *vi);
int  ivwSetScale(ImodView *vi);
void ivwMultipleFiles(ImodView *iv, char *argv[], int firstfile, 
		      int lastimage);

void ivwTransModel(ImodView *iv);
void ivwSetModelTrans(ImodView *iv);
void ivwFlipModel(ImodView *iv);
void imodCheckWildFlag(Imod *imod);
void ivwCheckWildFlag(Imod *imod);
void ivwScaleDepth8(ImodView *iv, ivwSlice *tempSlicePtr);
void ivwReadZ(ImodView *iv, unsigned char *buf, int cz);


/* workprocs */
int imod_start_autosave(void);
int imodMovieXYZT(struct ViewInfo *vi, int x, int y, int z, int t);

/* imod_display.c */
int  imod_display_init(ImodApp *ap, char **argv, int *argc);
int  imod_color_init(ImodApp *ap);
void imod_cmap(Imod *m);
void imodSetObjectColor(int ob);
int  imodDraw(ImodView *vw, int flag);
void imod_redraw_all(void);
void stereoHardware(Widget w, int flag);
void imodOverrideTranslations(Widget w, XtTranslations translations);
void imodOverrideTransTable(Widget w, String table);
int mapcolor(unsigned long color, 
	     unsigned short red, 
	     unsigned short green, 
	     unsigned short blue);
int alloc_object_colors(Imod *m, int obstart, int obend);
int free_object_colors(Imod *m, int obstart, int obend);

int handle_input(struct ViewInfo *vi);
char *imodwfname(char *intro);
char *imodwEithername(char *intro, char *filename);
int imodMovie(struct ViewInfo *vi);
void imod_imgcnt(char *string);
char *ImodRes_SGIStereoCommand(void);
char *ImodRes_SGIRestoreCommand(void);

/* imod_draw.c */
/*
void imod_draw_window(void);
int  imod_open(FILE *mfin);
void show_status(char *info);
void imod_draw_window(void);
void imod_draw_xyzinfo(void);
void DrawModel(struct Mod_Model *mod);
void DrawObject(struct Mod_Object *obj, int index);
int  DrawClosedContour(struct Mod_Contour *cont, int obcolor, int select);
int  DrawOpenContour(struct Mod_Contour *cont, int obcolor);
int  drawghost(struct Mod_Model *mod);
void imod_cmap(struct Mod_Model *mod);
int  xyz_draw(struct ViewInfo *vi);
*/

/* window fuctions */
int  xyz_draw(struct ViewInfo *vi);
int  imod_zap_open(struct ViewInfo *vi);
int  imod_zap_draw(struct ViewInfo *vi);
int  imod_zap_close(struct ViewInfo *vi);
void zapDraw_cb(struct ViewInfo *vi, void *client, int drawflag);
void imodv_draw(void);
void imodv_new_model(Imod *mod);
int  imod_object_edit_draw(void);
int  imod_object_edit(Widget top);
int  open_pixelview(struct ViewInfo *vi);
int  xgraphOpen(struct ViewInfo *vi);
int imod_tumble_open(struct ViewInfo *vi);
int tltopen(ImodView *vw, struct ViewInfo *ti);
void imodv_open(struct Mod_Model *imod, int cmapbase);
int sslice_showslice(struct Super_slicer *ss);
int xtumOpen(struct ViewInfo *vi);
int set_pixelview(struct ViewInfo *vi);

/* imod_info public functions */
void imod_info_setglwin(void);
int  imod_info_input(void);
int  imod_info_open(int argc, char **argv);
void imod_info_quit(Widget w, XtPointer client, XtPointer call);
void imod_info_setobjcolor(void);
void imod_info_setocp(void);
void imod_info_setxyz(void);
void imod_info_setbw(int black, int white);
int  imod_info_bwfloat(ImodView *vw, int section, int time);
void imod_info_float_clear(int section, int time);
int  imod_open(FILE *mfin);
void show_status(char *info);
void imod_show_info(char *info, int line);
void imod_info_msg(char *top, char *bot);
void imod_info_forbid(void);
void imod_info_enable(void);
void imod_set_mmode(int mode);
void imod_draw_window(void);


/* old imod_igraph.c defines */
/*
#define IGRAPH_SIZE 256
#define IGRAPH_SCALE 1;

extern int Igraphx_Window;
extern int Igraphy_Window;
extern int Igraphz_Window;

extern struct ViewInfo Igraphx_vi;
extern struct ViewInfo Igraphy_vi;
extern struct ViewInfo Igraphz_vi;

int  imod_igraph_open(char axis);
void imod_igraph_close(char axis);
int  imod_igraph_input(unsigned short dev, short val); 
int  imod_igraph_draw(struct ViewInfo *vi);
void imod_igraph_gdata(struct ViewInfo *vi);
*/


void imodImageScaleDialog(ImodView *iv);
void imodImageScaleUpdate(ImodView *iv);

/* imod_model_edit.c */
int openModelEdit(ImodView *vw);
int openModelOffset(ImodView *vw);

/* imod_cachefill.c */
int icfGetAutofill(void);
unsigned char *icfDoAutofill(ImodView *vw, int cz);
void imodCacheFillDialog(ImodView *vw);
void imodCacheFill(ImodView *vw);

/* imod_edit.c */
int imod_setxyzmouse(void);
void imod_contour_move(int ob);
int imod_movepoint(int x, int y, int z);
int imod_nearest(Imod *mod);
int imod_obj_nearest(struct Mod_Object *obj, 
		     struct Mod_Index *index,
		     struct Mod_Point *pnt,
		     float selsize);

/* imod_model_draw.c */
void imodDrawModel(Imod *imod);

/* imod_cont_copy.c dialog */
int openContourCopyDialog(ImodView *vw);

/* image process dialog. */
int inputIProcOpen(ImodView *vw);
int iprocRethink(ImodView *vw);

/* imod_moviecon.c dialog */
int imcGetIncrement(ImodView *vw, int xyzt);
void imcGetStartEnd(ImodView *vw, int xyzt, int *stout, int *endout);
void imodMovieConDialog(ImodView *vw);
float imcGetInterval(void);
void imcSetMovierate(ImodView *vw, int newrate);
void imcResetAll(ImodView *vw);
int imcGetLoopMode(ImodView *vw);
int imcGetSnapshot(ImodView *vw);
void imcStartTimer(void);
void imcReadTimer(void);

/* plugin mods for imod. */
int imodPlugInit(void);
int imodPlugLoaded(int type);
int imodPlugCall(ImodView *vw, int type, int reason);
void imodPlugMenu(Widget parent, int pos); /* build plugin menu. */
int imodPlugHandleKey(ImodView *vw, XKeyEvent *event);
#ifdef __cplusplus
}
#endif

#include "imod_input.h" 

#endif     





