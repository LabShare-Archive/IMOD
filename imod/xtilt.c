/*  IMOD VERSION 2.41
 *
 *  xtilt.c -- Open the tilt window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <Xm/MainW.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/ArrowB.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <X11/keysym.h>
#include <Xm/VirtKeys.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <math.h>
#include <stdlib.h>
#include <mrcc.h>
#include "imod.h"

#ifdef DRAW_GL
int tilt_draw(struct ViewInfo *vi){return(0);}
#endif

#define button_width  16
#define button_height 16

static unsigned char movie_bits[] = {
     0x07, 0xe0, 0x05, 0xa0, 0xff, 0xff, 0xfd, 0xbf, 0x07, 0xe0, 0x05, 0xa0,
     0x07, 0xe0, 0x05, 0xa0, 0x07, 0xe0, 0x05, 0xa0, 0x07, 0xe0, 0x05, 0xa0,
     0xff, 0xff, 0xfd, 0xbf, 0x07, 0xe0, 0x05, 0xa0};

struct imod_xtilt_struct
{
     /* image data for xtilt */
     struct ViewInfo *vi;

     unsigned char  **idata;
     int  xsize, ysize, zsize;
     unsigned int xysize;

     Widget        dialog;
     Widget        gfx;
     Widget        label;
     XtWorkProcId  work_id;
     XtIntervalId  iid;
     XtWorkProcId  mwork_id;
     XtIntervalId  miid;
     int           movie_timeout;
     XtAppContext  app;
     XID           context;
     Pixmap        moviepix;
     B3dCIImage    *image;

     int    isModelImage;
     int    ginit;
     int    exposed;
     int    width, height;
     int    zoom;
     int    cx, cy, cz;
     int    ct, nt;
     int    tstep;
     int    highres;
     int    locked;
     int    stereo;
     int    black;
     int    white;
     int    rampbase;
     int    movie;
     int xo, yo, xso, yso;
     int xtrans, ytrans;
     int object;

     /* Tilt info */
     float *tilt;
     float  axis_x;
     float  axis_y;
     float  axis_z;
     float  scale_x;
     float  scale_y;
     float  scale_z;
     float  alpha;
     float  beta;
     float  gamma;

};

void xtiltDraw( struct imod_xtilt_struct *xtilt);
void tilt_help_cb(Widget w, XtPointer client, XtPointer call);

/* internal functions */
static void xtiltMovie(struct imod_xtilt_struct *xtilt);
static Boolean xtilt_movie_wp(XtPointer client);
static int tltfopen(ImodView *vw, char *ff);
static int xtiltOpen(struct imod_xtilt_struct *xtilt);
static int xtiltGetStereoTilt(struct imod_xtilt_struct *xtilt);
static int xtiltGetx(struct imod_xtilt_struct *xtilt, 
		     double tilt, int x, int z);
static void addworkproc(XtPointer client, XtIntervalId *id);
static void tupdate(XtPointer client, XtIntervalId *id);
static Boolean work_update(XtPointer client);

void tilt_help_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;

     dia_vasmsg("Imod Tilt Help\n",
		"----------------------------------------------------------\n",
		"Starting the Tilt Window.\n",
		"\tAfter selecting \"Tilt\" from the \"Image\" menu, ",
		"a file requester will appear.  ",
		"The Tilt Window will accept two different types of files.  ",
		"One is a standard MRC file; the Tilt Window will try to ",
		"get the tilt information from the MRC header.  ",
		"The other is a text file with the following format:\n",
		"First line is mrc image file name.\n",
		"Lines 2-4 are translation values for x,y,z\n",
		"Lines 5-7 are scale values for x,y,z.\n",
		"Lines 8-10 are angles x,y,z.\n",
		"Lines 11+ are a list of tilts.\n",
		"\nToolBar commands:\n"
		"\tThe + and - gadgets change the zoom factor.\n",
		"\tThe movie gadget toggles movie mode.\n",
		"\nKeyboard commands:\n",
		"\ts\tToggle Cross-eyed Stereo View.\n",
		"\t-/=\tDecrease/Increase Zoom.\n",
		"\tArrows\tPan Image.\n",
		"\tPageUp\tNext Tilt View.\n",
		"\tPageDown\tPrevious Tilt View.\n",
		NULL);
     return;
}

void tiltDelete(struct imod_xtilt_struct *xtilt)
{
     int i;

     if (!xtilt) return;
     if (!xtilt->isModelImage){
	  if (xtilt->idata){
	       for(i = 0; i < xtilt->zsize; i++)
		    if (xtilt->idata[i])
			 free(xtilt->idata[i]);
	       free(xtilt->idata);
	  }
     }
     if (xtilt->tilt) free (xtilt->tilt);
     free(xtilt);
}

unsigned char *xtiltGetImageData(struct imod_xtilt_struct *xtilt, int z)
{
     if (xtilt->isModelImage)
	  return(ivwGetZSection(xtilt->vi, z));
     return(xtilt->idata[z]);
}


void tilt_imgcnt(char *string)
{
     wprint("Loading Tilt image file.\n%s\r", string);
     return;
}

static void tilt_open_cb(Widget w, XtPointer client, XtPointer call)
{
     DiaFactCallbackStruct *cbs = (DiaFactCallbackStruct *)call;
     

     if (cbs->infilename == NULL || cbs->infilename[0] == '\0') {
/*	  show_status("Tilter: No file selected."); */
	  wprint("\nOpening tilt window with loaded image.\n");
	  tltfopen(XYZ_vi, NULL);
     }else
	  tltfopen(XYZ_vi, cbs->infilename);

}

int tltopen(ImodView *vw, struct ViewInfo *ti)
{
     dia_fload("Load a Tilt file.", NULL, tilt_open_cb, NULL);
     return(-1);
}

static int tltfopen(ImodView *vw, char *ff)
{
     struct imod_xtilt_struct *xtilt;
     char tiltfilename[256];
     FILE *fin = NULL;
     FILE *hfin;
     struct MRCheader hdata;
     int istiltfile = False;
     float finc, ftilt;
     unsigned int i,k;
     unsigned int xysize,ksize,rbase;
     unsigned char pix, **idata;
     float scale;
     struct MRCheader savehdr;

     xtilt = (struct imod_xtilt_struct *)malloc
	  (sizeof(struct imod_xtilt_struct));
     xtilt->tilt  = 0;
     xtilt->idata = 0;
     xtilt->ginit = 0;
     if (!xtilt)
	  return(-1);

     xtilt->vi = vw;

     if (ff){
	  xtilt->isModelImage = FALSE;
	  fin = fopen(ff, "r");
	  if (!fin){
	       wprint("Error:\n\tTilt Window Couldn't Open.\n\tFile (%s)%s",
		      ff, "open failed.");
	       return(-1);
	  }
	  
	  
	  if (mrc_head_read(fin, &hdata)){
	       /* Must be tilt text file. */
	       rewind(fin);
	       fscanf(fin, "%s\n", tiltfilename);
	       hfin = fopen(tiltfilename, "r");
	       if (!hfin){
		    wprint("Tilt: error opening %s\n", tiltfilename);
		    return(-1);
	       }
	       if (mrc_head_read(hfin, &hdata)){
		    imod_info_msg("Tilt Window Error:", 
				  "Error reading header.");
		    return(-1);
	       }
	       
	       istiltfile = TRUE;
	       
	       fscanf(fin, "%f\n", &(xtilt->axis_x));
	       fscanf(fin, "%f\n", &(xtilt->axis_y));
	       fscanf(fin, "%f\n", &(xtilt->axis_z));
	       fscanf(fin, "%f\n", &(xtilt->scale_x));
	       fscanf(fin, "%f\n", &(xtilt->scale_y));
	       fscanf(fin, "%f\n", &(xtilt->scale_z));
	       fscanf(fin, "%f\n", &(xtilt->alpha));
	       fscanf(fin, "%f\n", &(xtilt->beta));
	       fscanf(fin, "%f\n", &(xtilt->gamma));
	       xtilt->tilt = (float *)malloc( hdata.nz * sizeof(float));
	       for(i = 0; i < hdata.nz; i++){
		    fscanf(fin, "%f\n", &(xtilt->tilt[i]));
	       }
	  }
     }else{
	  xtilt->isModelImage = TRUE;
#ifndef USEIMODI
	  hdata = *(vw->hdr);
#else
	  if (vw->image->file != IIFILE_MRC){
	      wprint("Tilt only works with MRC files.\n");
	      free(xtilt);
	      return(-1);
	  }
	  hdata = *((struct MRCheader *)(vw->image->header));
#endif
     }

     xtilt->xsize  = hdata.nx;
     xtilt->ysize  = hdata.ny;
     xtilt->zsize  = hdata.nz;
     xtilt->xysize = xtilt->xsize * xtilt->ysize;
     xtilt->rampbase = vw->rampbase;
     xtilt->black = 0;
     xtilt->white = 255;
     xtilt->movie = False;
     xtilt->cx = vw->xmouse;
     xtilt->cy = vw->ymouse;
     xtilt->cz = vw->zmouse;
     
     xtilt->xo = xtilt->yo = xtilt->xso = xtilt->yso = 0;
     xtilt->xtrans = xtilt->ytrans = 0;
	  
     /* header.idtype = 1 for tilt series.  */
     /* header.nd2 tilt axis x,y,z -> 1,2,3 */
     /* header.vd1 = start tilt angle       */
     /* header.vd2 = step in tilt angle     */
     if (!istiltfile){
	  xtilt->tilt = (float *)malloc( hdata.nz * sizeof(float));
	  ftilt = (float)hdata.vd1 / 100.0f;
	  finc  = (float)hdata.vd2 / 100.0f;
	  for(i = 0; i < hdata.nz; i++, ftilt += finc)
	       xtilt->tilt[i] = ftilt;
     }

     if (xtilt->isModelImage){
	 int idtype = 0;
	  xtilt->idata = vw->idata;
	  xtilt->tilt = (float *)malloc(vw->zsize * sizeof(float));
#ifndef USEIMODI
	 idtype = vw->hdr->idtype;
#else
	 if (vw->image->file == IIFILE_MRC){
	     struct MRCheader *hptr = (struct MRCheader *)vw->image->header;
	     idtype = hptr->idtype;
	 }
#endif
	  if (idtype == 1){
	       ftilt = (float)hdata.vd1 / 100.0f;
	       finc  = (float)hdata.vd2 / 100.0f;
	       for(i = 0; i < hdata.nz; i++, ftilt += finc)
		    xtilt->tilt[i] = ftilt;
	  }else{
	       for(i = 0; i < hdata.nz; i++)
		    xtilt->tilt[i] = i+1;
	  }
     }else{
	  wprint("Loading tilt file");
	  wprint(Statstring, "Image size %d x %d, %d sections",
		 hdata.nx, hdata.ny, hdata.nz);
	  
	  /* DNM: just in case, do save and restore of mode here */
	  savehdr = hdata;
	  if (istiltfile)
	       xtilt->idata = (unsigned char **)mrc_read_byte
		    (hfin, &hdata, NULL, tilt_imgcnt);
	  else
	       xtilt->idata = (unsigned char **)mrc_read_byte
		    (fin, &hdata, NULL, tilt_imgcnt);
	  hdata = savehdr;

	  if (!xtilt->idata){
	       wprint("Tilt Window Error:\nImage data not read.");
	       if (xtilt->tilt)
		    free (xtilt->tilt);
	       free(xtilt);
	       return(-1);
	  }
     
	  if (App->depth == 8){
	       rbase = vw->rampbase;
	       scale = vw->rampsize/256.0;
	       xysize = xtilt->xsize * xtilt->ysize;
	       ksize = xtilt->zsize;
	       idata = xtilt->idata;
	       for(k = 0; k < ksize; k++)
		    for(i = 0; i < xysize; i++){
			 pix = idata[k][i];
			 pix *= scale;
			 pix += rbase;
			 idata[k][i] = pix;
		    }
	  }
     }

     return(xtiltOpen(xtilt));
}

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     if (!xtilt)
	  return;
     if (xtilt->iid)
	  XtRemoveTimeOut(xtilt->iid);
     if (xtilt->work_id)
	  XtRemoveWorkProc(xtilt->work_id);
     if (xtilt->miid)
	  XtRemoveTimeOut(xtilt->miid);
     if (xtilt->mwork_id)
	  XtRemoveWorkProc(xtilt->mwork_id);

     b3dWinset(XtDisplay(xtilt->gfx), 0, 0);
     XtPopdown(xtilt->dialog);
     XtDestroyWidget(xtilt->dialog);
     tiltDelete(xtilt);
     return;
}

static void ginit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     Dimension winx, winy;
     
     if (xtilt->ginit) return;
     xtilt->context = b3dGetContext(xtilt->gfx);
     b3dWinset(XtDisplay(xtilt->gfx), xtilt->gfx, xtilt->context);
     xtilt->image   = b3dGetNewCIImage(NULL, App->depth);
     xtilt->cx = xtilt->vi->xmouse;
     xtilt->cy = xtilt->vi->ymouse;
     xtilt->cz = xtilt->vi->zmouse;
     xtilt->iid = XtAppAddTimeOut
	  (xtilt->app, 3000L, tupdate, (XtPointer)xtilt);
     xtilt->ginit = 1;
     return;
}

static void expose_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     Dimension winx, winy;
     int ox, oy;

     if (!xtilt->exposed){
	  XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
	  xtilt->width  = winx;
	  xtilt->height = winy;
	  ginit_cb(w, client, call);
	  xtilt->exposed = True;
     }
     XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
     ox = xtilt->width; oy = xtilt->height;
     xtilt->width  = winx; xtilt->height = winy;
     b3dWinset(XtDisplay(xtilt->gfx), xtilt->gfx, xtilt->context);
     b3dResizeViewport();
     if ((winx != ox) || (winy != oy)){
	  xtilt->image   = b3dGetNewCIImage(xtilt->image, App->depth);
     }
     
     xtiltDraw(xtilt);
     return;
}

static void resize_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     Dimension winx, winy;
     
     XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
     xtilt->width  = winx;
     xtilt->height = winy;

     if (xtilt->ginit){
	  b3dWinset(XtDisplay(xtilt->gfx), xtilt->gfx, xtilt->context);
	  b3dResizeViewport();
	  xtilt->image   = b3dGetNewCIImage(xtilt->image, App->depth);
	  xtiltDraw(xtilt);
     }
     return;
}

static void input_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     B3dDrawingAreaCallbackStruct *cbs = (B3dDrawingAreaCallbackStruct *)call;
     KeySym keysym;
     
     switch(cbs->event->type){
	  
	case KeyPress:
	  keysym = XLookupKeysym((XKeyEvent *)cbs->event, 0);
	  
	  switch(keysym){
	     case XK_Up:
	       xtilt->ytrans += 5;
	       xtiltDraw(xtilt);
	       break;
	     case XK_Down:
	       xtilt->ytrans -= 5;
	       xtiltDraw(xtilt);
	       break;
	     case XK_Right:
	       xtilt->xtrans += 5;
	       xtiltDraw(xtilt);
	       break;
	     case XK_Left:
	       xtilt->xtrans -= 5;
	       xtiltDraw(xtilt);
	       break;
			      
	     case '=':
	       xtilt->zoom += 1.0;
	       xtiltDraw(xtilt);
	       break;
	     case '-':
	       xtilt->zoom -= 1.0;
	       if (xtilt->zoom < 1.0)
		    xtilt->zoom = 1.0;
	       xtiltDraw(xtilt);
	       break;
	     case XK_Next:
	       xtilt->ct++;
	       if (xtilt->ct >= xtilt->nt)
		    xtilt->ct = xtilt->nt - 1;
	       xtiltDraw(xtilt);
	       break;
	     case XK_Prior:
	       xtilt->ct--;
	       if (xtilt->ct < 0)
		    xtilt->ct = 0;
	       xtiltDraw(xtilt);
	       break;
	     case XK_s:
	       if (xtilt->stereo)
		    xtilt->stereo = 0;
	       else
		    xtilt->stereo = 1;
	       xtiltDraw(xtilt);
	       break;

	     case XK_3:
	       if (xtilt->stereo != 3){
		    xtilt->stereo = 3;
		    stereoHardware(xtilt->dialog, 1);
	       }else{
		    xtilt->stereo = 0;
		    stereoHardware(xtilt->dialog, 0);
	       }
	       xtiltDraw(xtilt);
	       break;

	     case XK_m:
	       if (xtilt->movie)
		    xtilt->movie = False;
	       else
		    xtilt->movie = True;
	       break;
	     case XK_r:
	       /* reload */
	       break;
	  }
	  break;
     
	case ButtonPress:
	  XmProcessTraversal(xtilt->gfx, XmTRAVERSE_CURRENT);
	  if (cbs->event->xbutton.button == 2){
	       xtilt->ct--;
	       if (xtilt->ct < 0) xtilt->ct = 0;
	  }
	  if (cbs->event->xbutton.button == 3){
	       xtilt->ct++;
	       if (xtilt->ct >= xtilt->nt) xtilt->ct = xtilt->nt-1;
	  }
	  break;
     
     }
     return;
}

static void zoomup_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     xtilt->zoom++;
     xtiltDraw(xtilt);
     return;
}

static void zoomdown_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     xtilt->zoom--;
     if (!xtilt->zoom)
	  xtilt->zoom = 1;
     xtiltDraw(xtilt);
     return;
}

static void movie_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     
     if (xtilt->movie){
	  xtilt->movie = False;
	  if (xtilt->mwork_id)
	       XtRemoveWorkProc(xtilt->mwork_id);
	  if (xtilt->miid)
	       XtRemoveTimeOut(xtilt->miid);
	  xtilt->work_id = xtilt->iid = 0;
     }else{
	  xtilt->movie = True;
	  xtiltMovie(xtilt);
     }
     return;
}

static int xtiltOpen(struct imod_xtilt_struct *xtilt)
{
     char *window_name;
     Widget mw, form, tools;
     Widget row, button;
     Atom            wmclose;
     Pixel           fg, bg;
     int             depth;
     
     xtilt->exposed = False;
     xtilt->width   = 256;
     xtilt->height  = 256;
     xtilt->zoom    = 1;
     xtilt->image   = NULL;
     xtilt->work_id = 0;
     xtilt->iid     = 0;
     xtilt->mwork_id = 0;
     xtilt->miid     = 0;
     xtilt->app     = App->context;
     xtilt->highres = 0;
     xtilt->ct      = 0;
     xtilt->nt      = xtilt->zsize;
     xtilt->tstep   = 1;
     xtilt->stereo  = 0;
     xtilt->movie_timeout = 0;
     xtilt->locked  = 0;
     xtilt->context = 0;
     xtilt->gfx     = 0;
     window_name    = imodwfname("IMOD Tilt: ");
     
     xtilt->dialog = XtVaCreatePopupShell
	  ("Tilt", topLevelShellWidgetClass, App->toplevel,
	   XmNvisual, App->visual,
	   XtNtitle, window_name,
	   XmNwidth, xtilt->width,
	   XmNheight, xtilt->height,
	   NULL);
     if (!xtilt->dialog){
	  tiltDelete(xtilt);
	  return(-1);
     }
     if (window_name)
	  free(window_name);

     mw = XtVaCreateManagedWidget
	  ("tumbler",  xmMainWindowWidgetClass,  xtilt->dialog,
	   NULL);
     
     tools = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, mw,
	   XmNshadowType, XmSHADOW_OUT,
	   NULL);
     row = XtVaCreateManagedWidget
	  ("toolrow", xmRowColumnWidgetClass, tools,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     {
	  button = XtVaCreateManagedWidget
	       ("+", xmPushButtonWidgetClass, row, NULL);
	  XtAddCallback(button, XmNactivateCallback, zoomup_cb,
			(XtPointer)xtilt);
	  button = XtVaCreateManagedWidget
	       ("-", xmPushButtonWidgetClass, row, NULL);
	  XtAddCallback(button, XmNactivateCallback, zoomdown_cb,
			(XtPointer)xtilt);


	  XtVaGetValues(button,
			XmNforeground, &fg,
			XmNbackground, &bg,
			XmNdepth, &depth,
			NULL);
			
	  xtilt->moviepix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)movie_bits,
		button_width, button_height,
		fg, bg, depth);
	  button = XtVaCreateManagedWidget
	       ("Lock", xmPushButtonWidgetClass, row,
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, xtilt->moviepix,
		NULL);
	  XtAddCallback(button, XmNactivateCallback, movie_cb,
			(XtPointer)xtilt);

	   button = XtVaCreateManagedWidget
		("?", xmPushButtonWidgetClass, row, NULL);
	  XtAddCallback(button, XmNactivateCallback, tilt_help_cb,
			(XtPointer)xtilt);

	  xtilt->label = XtVaCreateManagedWidget
	       ("Tilt", xmLabelWidgetClass, row, NULL);

     }
     XtManageChild(row);
     XtManageChild(tools);

     form  = XtVaCreateWidget("form", xmFormWidgetClass, mw, NULL);
     xtilt->gfx = XtVaCreateManagedWidget
	  ("gfx", B3dDrawingAreaWidgetClass, form,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   XmNnavigationType, XmNONE,
	   XmNtraversalOn, True,
	   XmNtranslations, XtParseTranslationTable (B3DGFX_Translations),
#ifdef DRAW_OpenGL
	   GLwNvisualInfo, App->visualinfoGL,
	   XmNcolormap,         App->cmapGL,
#endif
#ifdef DRAW_GL
	   GlxNglxConfig, B3DGFX_GLXconfig_doublebuffer,
#endif
	   NULL);

     XtAddCallback(xtilt->gfx,B3dNexposeCallback, expose_cb, (XtPointer)xtilt);
     XtAddCallback(xtilt->gfx,B3dNresizeCallback, resize_cb, (XtPointer)xtilt);
     XtAddCallback(xtilt->gfx,B3dNinputCallback,  input_cb,  (XtPointer)xtilt);
     
     imodOverrideTransTable(xtilt->gfx, B3DGFX_Translations);

     XtVaSetValues(xtilt->gfx, XmNbackground, App->background, NULL);

     XtManageChild(form);

#ifdef MWSETWORK
     XtVaSetValues(mw,XmNworkWindow,form,NULL);
#endif
     XmMainWindowSetAreas( mw, NULL, tools, NULL, NULL, form);
     XtManageChild(mw);
     
     wmclose = XmInternAtom( XtDisplay(xtilt->dialog),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(xtilt->dialog, wmclose, quit_cb,
			     (caddr_t)xtilt);
     XtPopup(xtilt->dialog, XtGrabNone);
     
     return(0);
}

static void xtiltDrawGraphics(struct imod_xtilt_struct *xtilt)
{
     unsigned char *image = xtiltGetImageData(xtilt, xtilt->ct);
     int z = xtilt->ct;
     int nz;
     int urx = xtilt->width;
     int ury = xtilt->height;

     if (xtilt->stereo == 1){
	  urx /= 2; urx -= 2;
     }
     if (xtilt->stereo > 1){
	  ury /= 2; ury -= 20;
     }

     b3dDrawGreyScalePixelsSubArea
	  (xtilt->image, image, xtilt->xsize, xtilt->ysize,
	   xtilt->xtrans, xtilt->ytrans,
	   0,0, urx, ury,
	   xtilt->rampbase, xtilt->zoom,
	   &(xtilt->xo), &(xtilt->yo));
     

     if (xtilt->stereo){
	  unsigned char *nimage =
	       xtiltGetImageData(xtilt, xtiltGetStereoTilt(xtilt));
	  if (xtilt->stereo == 1)
	       b3dDrawGreyScalePixelsSubArea
		    (xtilt->image, nimage, 
		     xtilt->xsize, xtilt->ysize,
		     xtilt->xtrans, xtilt->ytrans,
		     xtilt->width/2+2,
		     0,
		     xtilt->width,
		     xtilt->height,
		     xtilt->rampbase, xtilt->zoom,
		     &(xtilt->xso), &(xtilt->yso));
	  else
	       b3dDrawGreyScalePixelsSubArea
		    (xtilt->image, nimage,
		     xtilt->xsize, xtilt->ysize,
		     xtilt->xtrans, xtilt->ytrans,
		     0,
		     ury + 40,
		     xtilt->width,
		     xtilt->height,

		     xtilt->rampbase, xtilt->zoom,
		     &(xtilt->xso), &(xtilt->yso));
     }
     
     return;
}

static void xtiltDrawModel(struct imod_xtilt_struct *xtilt)
{
     Imod *imod;
     Icont *cont;
     int pt, nt;
     int ct;
     
     imod = xtilt->vi->imod;
     xtilt->object = imod->cindex.object;
     if (imod->cindex.point < 0)
	  return;
     cont = imodContourGet(imod);
     if (!cont)
	  return;
     if (!cont->psize)
	  return;
     ct = xtilt->ct;

     imodSetObjectColor(imod->cindex.object);
     b3dBeginLine();
     for (pt = 0; pt < cont->psize; pt++){
	  b3dVertex2i(b3dGetDrawCoord
		      (xtiltGetx(xtilt, 
				 xtilt->tilt[ct],
				 (int)cont->pts[pt].x, 
				 (int)cont->pts[pt].z),
		       xtilt->zoom, xtilt->xo),
		      b3dGetDrawCoord(cont->pts[pt].y,xtilt->zoom,xtilt->yo));
/*
	  b3dVertex2i(b3dGetDrawCoord(cont->pts[pt].x,xtilt->zoom,xtilt->xo),
		      b3dGetDrawCoord(cont->pts[pt].y,xtilt->zoom,xtilt->yo));
*/
     }
     b3dEndLine();

     if (xtilt->stereo == 1){
	  b3dBeginLine();
	  nt = xtiltGetStereoTilt(xtilt);
	  for (pt = 0; pt < cont->psize; pt++){
	       b3dVertex2i((xtilt->width/2) + 
			   b3dGetDrawCoord
			   (xtiltGetx(xtilt, 
				      xtilt->tilt[nt],
				      (int)cont->pts[pt].x, 
				      (int)cont->pts[pt].z),
			    xtilt->zoom, xtilt->xso),
			   b3dGetDrawCoord(cont->pts[pt].y,
					   xtilt->zoom,xtilt->yso));
	  }
	  b3dEndLine();
     }

     if (xtilt->stereo > 1){
	  b3dBeginLine();
	  nt = xtiltGetStereoTilt(xtilt);
	  for (pt = 0; pt < cont->psize; pt++){
	       b3dVertex2i((xtilt->width/2) + 
			   b3dGetDrawCoord
			   (xtiltGetx(xtilt, 
				      xtilt->tilt[nt],
				      (int)cont->pts[pt].x, 
				      (int)cont->pts[pt].z),
			    xtilt->zoom, xtilt->xso),
			   b3dGetDrawCoord(cont->pts[pt].y,
					   xtilt->zoom,xtilt->yso));
	  }
	  b3dEndLine();
     }
     return;
}

static void xtiltDrawLabel(struct imod_xtilt_struct *xtilt)
{
     XmString str;
     char labl[32];
     int nz;

     nz = xtilt->ct + 1;
     if (nz >= xtilt->nt)
	 nz = xtilt->nt - 1;

     if (xtilt->stereo)
	  sprintf(labl, "Tilt %3.1f / %3.1f", xtilt->tilt[xtilt->ct],
		  xtilt->tilt[nz]);
     else
	  sprintf(labl, "Tilt %3.1f", xtilt->tilt[xtilt->ct]);
     str = XmStringCreateSimple(labl);
     XtVaSetValues(xtilt->label, XmNlabelString, str, NULL);
     XmStringFree(str);
     return;
}

static void xtiltUpdate(struct imod_xtilt_struct *xtilt)
{
     int object = xtilt->vi->imod->cindex.object;

     if (xtilt->object < 0)
	  if (object < 0)
	       return;

     xtiltDraw(xtilt);
     return;
}


void xtiltBoarder(struct imod_xtilt_struct *xtilt)
{
     int cx, cy, gx, gy, sx, sy;
     cx = xtilt->width/2;
     gx = (xtilt->xsize * xtilt->zoom)/2;
     cy =  xtilt->height/2;
     gy = (xtilt->ysize * xtilt->zoom)/2;

     b3dColorIndex(App->background);

     switch(xtilt->stereo){
	case 0: /* OFF */
	  b3dDrawBoxout(cx-gx, cy-gy, cx + gx, cy + gy);
	  break;
	case 1: /* SIDE BY SIDE */
	  b3dDrawBoxout((cx/2)-gx, cy-gy, ((cx/2)*3)+gx, cy + gy);
	  sx = (cx/2)-gx;
	  if (sx > 0)
	       b3dDrawFilledRectangle(cx - sx, cy-gy, sx*2,  gy*2);
	  break;

	case 2: /* TOP BOTTOM , STR_RECT*/
	  b3dDrawBoxout(cx-gx, (cy/2)-gy,  cx + gx, ((cy/2)*3)+gy);
	  sy = (cy/2)-gy;
	  if (sy > 0)
	       b3dDrawFilledRectangle(cx - gx, cy-sy, gx*2,  sy*2);
	  break;
     }
     return;
}

void xtiltDraw( struct imod_xtilt_struct *xtilt)
{
     if ((!xtilt->exposed) || (!xtilt->gfx) || (!xtilt->context))
	  return;

     b3dWinset(XtDisplay(xtilt->gfx), xtilt->gfx, xtilt->context);

     xtiltDrawGraphics(xtilt);
     xtiltDrawModel(xtilt);
     xtiltBoarder(xtilt);
     b3dSwapBuffers();
     xtiltDrawLabel(xtilt);
     return;
}


static int xtiltGetx(struct imod_xtilt_struct *xtilt, 
		     double tilt, int x, int z)
{
     int xpos;
     float tx, tz;
     float tdeg;
     float xc;
     float zc;
     xc = xtilt->axis_x;
     zc = xtilt->axis_z;
     
     xpos = x;
     
     tdeg =   tilt * 0.017453293;  /* Convert tilt from degrees to radians. */
     
     /* translate */
     tx = x - xc;
     tz = z - zc;
     
     /* rotate */
     xpos = (tx * cos(tdeg)) + (tz * sin(tdeg));
     
     /* translate back */
     xpos += xc;
     
     return(xpos);
/*     return(b3dGetDrawCoord(xpos, xtilt->yo, xtilt->zoom)); */
}

static int xtiltGetStereoTilt(struct imod_xtilt_struct *xtilt)
{
     int st = xtilt->ct+xtilt->tstep;
     if (st >= xtilt->nt)
	  st = xtilt->nt - 1;
     return(st);
}

/*****************************************************************************/
static void addworkproc(XtPointer client, XtIntervalId *id)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     
     xtilt->work_id = XtAppAddWorkProc(xtilt->app, work_update, xtilt);
     xtilt->iid = 0;
     return;
}

static void tupdate(XtPointer client, XtIntervalId *id)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     
     if (!xtilt->vi)
	  return;
     if (!xtilt->iid)
	  return;
     if (!xtilt->locked)
	  if ((xtilt->cx != xtilt->vi->xmouse) ||
	      (xtilt->cy != xtilt->vi->ymouse) ||
	      (xtilt->cz != xtilt->vi->zmouse)){
	       xtilt->cx = xtilt->vi->xmouse;
	       xtilt->cy = xtilt->vi->ymouse;
	       xtilt->cz = xtilt->vi->zmouse;
	       xtiltUpdate(xtilt);
	  }
     
     if (xtilt->locked)
	  xtilt->iid = XtAppAddTimeOut (xtilt->app, 1000L, tupdate,
				     (XtPointer)xtilt);
     else
	  xtilt->iid = XtAppAddTimeOut (xtilt->app, 100L, tupdate,
				     (XtPointer)xtilt);
     return;
}

/* Workproc update function */
static Boolean work_update(XtPointer client)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     
     if ((xtilt->cx != xtilt->vi->xmouse) ||
	 (xtilt->cy != xtilt->vi->ymouse) ||
	 (xtilt->cz != xtilt->vi->zmouse)){
	  xtilt->cx = xtilt->vi->xmouse;
	  xtilt->cy = xtilt->vi->ymouse;
	  xtilt->cz = xtilt->vi->zmouse;
	  xtiltUpdate(xtilt);
	  return(True);
     }
     
     xtilt->iid = XtAppAddTimeOut (xtilt->app, 100L, addworkproc,
				(XtPointer)xtilt);
     xtilt->work_id = 0;
     return(True);
}


static void xtilt_movie_to(XtPointer client, XtIntervalId *id)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     
     xtilt->mwork_id = XtAppAddWorkProc
	  (xtilt->app, xtilt_movie_wp, (XtPointer)xtilt);
     xtilt->miid = 0;
     return;
}

static Boolean xtilt_movie_wp(XtPointer client)
{
     struct imod_xtilt_struct *xtilt = (struct imod_xtilt_struct *)client;
     static int direction = 1;

     xtilt->ct += direction;

     if (xtilt->ct < 0){
	  direction = 1;
	  xtilt->ct = 1;
     }
     if (xtilt->ct >= xtilt->nt){
	  direction = -1;
	  xtilt->ct = xtilt->nt - 2;
     }
     xtiltDraw(xtilt);

     if (xtilt->movie_timeout){
	  xtilt->miid = XtAppAddTimeOut(xtilt->app, xtilt->movie_timeout, 
					xtilt_movie_to, (XtPointer)xtilt);
	  xtilt->mwork_id = 0;
	  return(True);
     }
     return(False);
}

static void xtiltMovie(struct imod_xtilt_struct *xtilt)
{
     if (xtilt->nt < 2)
	  return;

     xtilt->mwork_id = XtAppAddWorkProc
	  (xtilt->app, xtilt_movie_wp, (XtPointer)xtilt);

     return;
}


