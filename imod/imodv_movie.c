/*  IMOD VERSION 2.42
 *
 *  imodv_movie.c -- Movie creation dialog for imodv.
 *
 *  Original Original author: James Kremer
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

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <dia.h>
#include "imodv.h"
#include "b3dgfx.h"

static void set_sensitivities(void);

struct imodvMovieDialogStruct
{
     diaDialog *dia;
     ImodvApp  *a;
     Widget amin, amax, bmin, bmax, gmin, gmax, zmin, zmax;
     Widget xtmin, xtmax, ytmin, ytmax, ztmin, ztmax;
     Widget frames;
     Widget startbut, endbut, fullxbut, fullybut, longbut, revbut;
     Widget montframes, montoverlap, framelabel, overlabel;
     Widget save;
     int saved;
     int reverse;
     int longway;
     int montage;
     int file_format;
};

static struct imodvMovieDialogStruct *MovieDialog = NULL;
static int    abort_movie;
static void imodvMakeMovie(struct imodvMovieDialogStruct *movie);
static void imodvMakeMontage(struct imodvMovieDialogStruct *movie);

static int fullaxis = 0;

static void help_cb()
{
     dia_vasmsg
	  ("Make Movie Dialog Help.\n\n",
	   "Movie Making\n\n",
	   "The upper controls make a movie by stepping through "
	   "from the values in the start set to the values in the end set.\n\n"
	   "Select the [Set Start] button to set the starting values to the "
	   "values of the current display, or the [Set End] button to set "
	   "the ending values.\n\n"
	   "Selecting the [Make] button will cause imodv to "
	   "display the number of frames given.  The rotation between the "
	   "starting and ending positions will be resolved into a rotation "
	   "around a single axis, and the rotation will occur at even "
	   "increments around that axis.\n\n"
	   "Select the [Stop] button to stop after the next display.\n\n"
	   "If [Write Files] is selected then a snapshot will "
	   "be taken of each image.  The file will be an RGB or a TIFF file, "
	   "depending on which radio button is selected.\n\n"
	   "To make a movie through 360 degrees around the X or the Y axis, "
	   "select the [Full 360 X] or [Full 360 Y] button.  Then select the "
	   "[Make] button.  The number of frames can be set before or "
	   "after selecting a Full 360 button.  Selecting [Set Start] or "
	   "[Set End] will cancel the Full 360 selection.\n\n",
	   "If [Reverse] is selected, the movie will run in reverse, from "
	   "the ending to the starting position, or rotate in the opposite "
	   "direction for a Full 360 movie.\n\n"
	   "If [Long way] is selected, the rotation will go the long way "
	   "around, through an angle greater instead of less than 180 "
	   "degrees.\n\n\n",
	   "Montage Making\n\n",
	   "The lower controls allow one to save a montage of zoomed-up views "
	   "of the model.  The model will be zoomed up by an appropriate "
	   "factor, then translated to a regular array of positions.  "
	   "The collection of "
	   "images will form a montage of whatever is currently in the window "
	   "before you start.  Perspective must be set to zero in order for "
	   "this to work correctly.\n\n"
	   "Set the number of frames to the desired zoom factor.  Set the "
	   "amount of overlap to whatever is most convenient for reassembling "
	   "the montage.  "
	   "If [Write Files] is selected then a snapshot will "
	   "be taken of each image.  The file will be an RGB or a TIFF file, "
	   "depending on which radio button is selected.\n",
	   NULL);
		
     return;
}

static void xinput(void)
{
     XEvent event_return;
     XFlush(XtDisplay(Imodv->topLevel));
     /* DNM: need to either mask for X events in the while, or process ALL
	types of events; so just call XtAppProcessEvent with XtIMAll */
     while(XtAppPending(Imodv->context)){
       /*	  XtAppNextEvent(Imodv->context, &event_return);
		  XtDispatchEvent(&event_return); */
       XtAppProcessEvent(Imodv->context, XtIMAll);
     }
     return;
}

static void set_sensitivities()
{
     struct imodvMovieDialogStruct *mv = MovieDialog;
     Boolean state = True;
     if (mv->montage)
	  state = False;
     XtSetSensitive(mv->startbut, state);
     XtSetSensitive(mv->endbut, state);
     XtSetSensitive(mv->fullxbut, state);
     XtSetSensitive(mv->fullybut, state);
     XtSetSensitive(mv->longbut, state);
     XtSetSensitive(mv->revbut, state);
     XtSetSensitive(mv->amin, state);
     XtSetSensitive(mv->amax, state);
     XtSetSensitive(mv->bmin, state);
     XtSetSensitive(mv->bmax, state);
     XtSetSensitive(mv->gmin, state);
     XtSetSensitive(mv->gmax, state);
     XtSetSensitive(mv->zmin, state);
     XtSetSensitive(mv->zmax, state);
     XtSetSensitive(mv->xtmin, state);
     XtSetSensitive(mv->xtmax, state);
     XtSetSensitive(mv->ytmin, state);
     XtSetSensitive(mv->ytmax, state);
     XtSetSensitive(mv->ztmin, state);
     XtSetSensitive(mv->ztmax, state);
     XtSetSensitive(mv->frames, state);

     state = False;
     if (mv->montage)
	  state = True;
     XtSetSensitive(mv->montframes, state);
     XtSetSensitive(mv->montoverlap, state);
     XtSetSensitive(mv->framelabel, state);
     XtSetSensitive(mv->overlabel, state);
}

static void movie_mont_cb(Widget w, XtPointer client, XtPointer call)
{
     MovieDialog->montage = (int)client;
     set_sensitivities();
}

static void rgb_tiff_cb(Widget w, XtPointer client, XtPointer call)
{
     MovieDialog->file_format = (int)client;
}


static void saved_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     if (cbs->set)
	  MovieDialog->saved = True;
     else
	  MovieDialog->saved = False;
     return;
}

static void setstart_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = Imodv;
     Imod *imod = Imodv->imod;
     char valstr[32];
     Iview *vw = &imod->view[0];
     fullaxis = 0;

     sprintf(valstr, "%g", vw->rot.x);
     XtVaSetValues(MovieDialog->amin, XmNvalue, valstr, NULL);

     sprintf(valstr, "%g", vw->rot.y);
     XtVaSetValues(MovieDialog->bmin, XmNvalue, valstr, NULL);

     sprintf(valstr, "%g", vw->rot.z);
     XtVaSetValues(MovieDialog->gmin, XmNvalue, valstr, NULL);

     sprintf(valstr, "%g", vw->rad);
     XtVaSetValues(MovieDialog->zmin, XmNvalue, valstr, NULL);

     sprintf(valstr, "%g", vw->trans.x);
     XtVaSetValues(MovieDialog->xtmin, XmNvalue, valstr, NULL);
     sprintf(valstr, "%g", vw->trans.y);
     XtVaSetValues(MovieDialog->ytmin, XmNvalue, valstr, NULL);
     sprintf(valstr, "%g", vw->trans.z);
     XtVaSetValues(MovieDialog->ztmin, XmNvalue, valstr, NULL);

     return;
}

static void setend_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = Imodv;
     Imod *imod = Imodv->imod;
     char valstr[32];
     Iview *vw = &imod->view[0];
     fullaxis = 0;

     sprintf(valstr, "%.2f", vw->rot.x);
     XtVaSetValues(MovieDialog->amax, XmNvalue, valstr, NULL);

     sprintf(valstr, "%.2f", vw->rot.y);
     XtVaSetValues(MovieDialog->bmax, XmNvalue, valstr, NULL);

     sprintf(valstr, "%.2f", vw->rot.z);
     XtVaSetValues(MovieDialog->gmax, XmNvalue, valstr, NULL);

     sprintf(valstr, "%g", vw->rad);
     XtVaSetValues(MovieDialog->zmax, XmNvalue, valstr, NULL);

     sprintf(valstr, "%g", vw->trans.x);
     XtVaSetValues(MovieDialog->xtmax, XmNvalue, valstr, NULL);
     sprintf(valstr, "%g", vw->trans.y);
     XtVaSetValues(MovieDialog->ytmax, XmNvalue, valstr, NULL);
     sprintf(valstr, "%g", vw->trans.z);
     XtVaSetValues(MovieDialog->ztmax, XmNvalue, valstr, NULL);
     return;
}

static void fullaxis_cb(Widget w, XtPointer client, XtPointer call)
{
     setstart_cb(w, client, call);
     setend_cb(w, client, call);
     fullaxis = (int)client;
}

void longway_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     MovieDialog->longway = cbs->set;
     return;
}

void reverse_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     MovieDialog->reverse = cbs->set;
     return;
}

static void makeWidgetSet(char *label, Widget parent, 
			  Widget *wMin, Widget *wMax,
			  double value)
{
     Widget row;
     char valstr[64];

     row = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, parent,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     sprintf(valstr, "%g", value);
     *wMin = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, row,
	   XmNcolumns, 13,
	   XmNvalue, valstr, NULL);
     *wMax = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, row,
	   XmNcolumns, 13,
	   XmNvalue, valstr, NULL);
     XtVaCreateManagedWidget
	  (label, xmLabelWidgetClass, row, NULL);
     XtManageChild(row);
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     char valstr[64];
     diaDialog *dia = (diaDialog *)call;
     struct imodvMovieDialogStruct *movie = 
	  (struct imodvMovieDialogStruct *)client;
     Widget rowcol, row, col, button, minicol;
     Imod *imod;
     ImodvApp *a;
     Iview *vw;
     float rad;
     Arg args[3]; 
     int n = 0;

     a = movie->a;
     rad = a->imod->view->rad;
     imod = a->imod;
     vw = &imod->view[0];
     fullaxis = 0;

     col    = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, w, NULL);

     /*     XtVaCreateManagedWidget
	    ("Make a movie:", xmLabelWidgetClass, col, NULL); */

     row    = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     minicol = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNorientation, XmVERTICAL,
	   NULL);
     button = XtVaCreateManagedWidget
	  ("Set Start", xmPushButtonWidgetClass, minicol, NULL);
     XtAddCallback(button, XmNactivateCallback, setstart_cb, NULL);
     movie->startbut = button;
     button = XtVaCreateManagedWidget
	  ("Set End", xmPushButtonWidgetClass, minicol, NULL);
     XtAddCallback(button, XmNactivateCallback, setend_cb, NULL);
     XtManageChild(minicol);
     movie->endbut = button;

     minicol = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNorientation, XmVERTICAL,
	   NULL);
     button = XtVaCreateManagedWidget
	  ("Full 360 X", xmPushButtonWidgetClass, minicol, NULL);
     XtAddCallback(button, XmNactivateCallback, fullaxis_cb, (XtPointer)-1);
     movie->fullxbut = button;
     button = XtVaCreateManagedWidget
	  ("Full 360 Y", xmPushButtonWidgetClass, minicol, NULL);
     XtAddCallback(button, XmNactivateCallback, fullaxis_cb, (XtPointer)1);
     XtManageChild(minicol);
     movie->fullybut = button;

     minicol = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNorientation, XmVERTICAL,
	   NULL);
     button  = XtVaCreateManagedWidget
	  ("Long way",  xmToggleButtonWidgetClass, minicol, NULL);
     XtAddCallback(button, XmNvalueChangedCallback, 
		   longway_cb, NULL);
     XmToggleButtonSetState(button, False, False);
     movie->longbut = button;

     button  = XtVaCreateManagedWidget
	  ("Reverse",  xmToggleButtonWidgetClass, minicol, NULL);
     XtAddCallback(button, XmNvalueChangedCallback, 
		   reverse_cb, NULL);
     XmToggleButtonSetState(button, False, False);
     movie->revbut = button;
     XtManageChild(minicol);

     XtManageChild(row);

     makeWidgetSet("X Rotation", col,
		   &movie->amin, &movie->amax,
		   a->imod->view->rot.x);
     makeWidgetSet("Y Rotation", col,
		   &movie->bmin, &movie->bmax,
		   a->imod->view->rot.y);
     makeWidgetSet("Z Rotation", col,
		   &movie->gmin, &movie->gmax,
		   a->imod->view->rot.z);

     makeWidgetSet("X Translation", col,
		   &movie->xtmin, &movie->xtmax,
		   a->imod->view->trans.x);
     makeWidgetSet("Y Translation", col,
		   &movie->ytmin, &movie->ytmax,
		   a->imod->view->trans.y);
     makeWidgetSet("Z Translation", col,
		   &movie->ztmin, &movie->ztmax,
		   a->imod->view->trans.z);

     makeWidgetSet("Zoom Factor", col,
		   &movie->zmin, &movie->zmax, rad);

     row    = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col, 
	   XmNorientation, XmHORIZONTAL, 
	   NULL);
     movie->frames = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, row,
	   XmNcolumns, 10,
	   XmNvalue, "10", NULL);
     XtVaCreateManagedWidget
	  ("# of Movie Frames" , xmLabelWidgetClass, row, NULL);
     XtManageChild(row);

     XtVaCreateManagedWidget
	  ("Separator", xmSeparatorWidgetClass, col, 
	   XmNseparatorType, XmSHADOW_ETCHED_OUT,
	   XmNshadowThickness, 5,
	   NULL);     

     row    = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col, 
	   XmNorientation, XmHORIZONTAL, 
	   NULL);
     movie->montframes = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, row,
	   XmNcolumns, 10,
	   XmNvalue, "2", NULL);
     movie->framelabel = XtVaCreateManagedWidget
	  ("# of Montage Frames in X & Y" , xmLabelWidgetClass, row, NULL);
     XtManageChild(row);

     row    = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col, 
	   XmNorientation, XmHORIZONTAL, 
	   NULL);
     movie->montoverlap = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, row,
	   XmNcolumns, 10,
	   XmNvalue, "4", NULL);
     movie->overlabel = XtVaCreateManagedWidget
	  ("Montage Overlap in Pixels" , xmLabelWidgetClass, row, NULL);
     XtManageChild(row);

     XtVaCreateManagedWidget
	  ("Separator", xmSeparatorWidgetClass, col,
	   XmNseparatorType, XmSHADOW_ETCHED_OUT,
	   XmNshadowThickness, 5,
	   NULL);     

     row    = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col, 
	   XmNorientation, XmHORIZONTAL, 
	   NULL);

	 
     XtSetArg(args[n], XmNorientation,  XmVERTICAL); n++;
     minicol = XmCreateRadioBox(row, "movie", args, n);
     button = XtVaCreateManagedWidget
	  ("Make Movie", xmToggleButtonWidgetClass, minicol, NULL);
     XtAddCallback(button, XmNvalueChangedCallback, movie_mont_cb, 
		   (XtPointer)0);
     XmToggleButtonSetState(button, True, False);

     button = XtVaCreateManagedWidget
	  ("Make Montage", xmToggleButtonWidgetClass, minicol, NULL);
     XtAddCallback(button, XmNvalueChangedCallback, movie_mont_cb, 
		   (XtPointer)1);
     XmToggleButtonSetState(button, False, False);

     XtManageChild(minicol);
     movie->montage = 0;
     set_sensitivities();

     minicol = XmCreateRadioBox(row, "movie", args, n);
     button = XtVaCreateManagedWidget
	  ("RGBs", xmToggleButtonWidgetClass, minicol, NULL);
     XtAddCallback(button, XmNvalueChangedCallback, rgb_tiff_cb, 
		   (XtPointer)SnapShot_RGB);
     XmToggleButtonSetState(button, True, False);

     button = XtVaCreateManagedWidget
	  ("TIFFs", xmToggleButtonWidgetClass, minicol, NULL);
     XtAddCallback(button, XmNvalueChangedCallback, rgb_tiff_cb, 
		   (XtPointer)SnapShot_TIF);
     XmToggleButtonSetState(button, False, False);

     XtManageChild(minicol);

     movie->save = XtVaCreateManagedWidget
	  ("Write Files", xmToggleButtonWidgetClass, row, NULL);
     XtAddCallback(movie->save, XmNvalueChangedCallback, saved_cb, NULL);
     XtManageChild(row);
     XtManageChild(col);
     return;
}

static void done_cb(Widget w, XtPointer client, XtPointer call)
{
      /* DNM: passing dia in the call spot didn't work on PC, and the
	client data is enough for the job */
     /*  diaDialog *dia = (diaDialog *)call; */
     struct imodvMovieDialogStruct *movie = 
	  (struct imodvMovieDialogStruct *)client;

     MovieDialog =  NULL;
     diaDestroyDialog(movie->dia);
     movie->dia = NULL;
     abort_movie = True;
     return;
}

static void stop_cb(Widget w, XtPointer client, XtPointer call)
{
     abort_movie = True;
     return;
}

static void make_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     struct imodvMovieDialogStruct *movie =
	  (struct imodvMovieDialogStruct *)client;

     /* DNM: only make if not already making */
     if (abort_movie) {
          abort_movie = False;
	  if (movie->montage)
	       imodvMakeMontage(movie);
	  else
	       imodvMakeMovie(movie);
     }
     return;
}

void imodvMovieDialog(ImodvApp *a, int state)
{
     static struct imodvMovieDialogStruct movie;
     static int first = 1;


     if (first){
	  movie.dia = NULL;
	  first = 0;
     }
     if (!state){
	  /* DNM: just send client data, the other entries aren't needed */
	  if (movie.dia)
	       done_cb(NULL, (XtPointer)&movie, 
		       NULL);
	  return;
     }

     if (movie.dia){
	  XRaiseWindow(a->display, XtWindow(movie.dia->dialog));
	  return;
     }
     MovieDialog = &movie;
     movie.a = a;
     movie.saved   = False;
     movie.reverse = False;
     movie.longway = False;
     abort_movie = True;   /* DNM: make this a flag that not making movie */
     movie.file_format = SnapShot_RGB;
     movie.dia = diaVaCreateDialog
	  ("Imodv: Movie Dialog", a->topLevel, a->context,
	   DiaNcontrolButton, "Close", done_cb, (XtPointer)&movie,
	   DiaNcontrolButton, "Make", make_cb, (XtPointer)&movie,
	   DiaNcontrolButton, "Stop", stop_cb, (XtPointer)&movie,
	   DiaNcontrolButton, "Help", help_cb, (XtPointer)&movie,
	   DiaNworkAreaFunc, workarea_cb, (XtPointer)&movie,
	   DiaNwindowQuit, done_cb, (XtPointer)&movie,
	   0);
     return;
}

static void setstep(struct imodvMovieDialogStruct *movie, int frame,
		    Widget wMin, Widget wMax, float *start, float *step)
{
     char *string;
     float tmin, tmax;

     string = XmTextGetString(wMin);
     tmin = atof(string);
     XtFree(string);
     string = XmTextGetString(wMax);
     tmax = atof(string);
     XtFree(string);
     if (movie->reverse){
	  *start = tmax;
	  *step  = (tmin - tmax) / (float)frame;
     }else{
	  *start = tmin;
	  *step = (tmax - tmin) / (float)frame;
     }
}


static void imodvMakeMovie(struct imodvMovieDialogStruct *movie)
{
     ImodvApp *a = movie->a;
     Iview *vw;
     char *string;
     int frames;
     int frame;
     float astart, astep;
     float bstart, bstep;
     float gstart, gstep;
     float zstart, zstep, zfac;
     float xm, ym, zm;
     float xtstart, ytstart, ztstart;
     float xtstep, ytstep, ztstep;
     float tmin, tmax;
     int mov, moveall;
     double angle, delangle;
     double alpha, beta, gamma;
     Ipoint v;
     Imat *mat, *mati, *matp;

     string = XmTextGetString(movie->frames);
     frames = atoi(string);
     XtFree(string);

     if (frames <= 0)
	  return;
     frame = frames - 1;
     if (!frame)
	  frame = 1;

     setstep(movie, frame, movie->amin, movie->amax, &astart, &astep);
     setstep(movie, frame, movie->bmin, movie->bmax, &bstart, &bstep);
     setstep(movie, frame, movie->gmin, movie->gmax, &gstart, &gstep);
     setstep(movie, frame, movie->zmin, movie->zmax, &zstart, &zstep);

     setstep(movie, frame, movie->xtmin, movie->xtmax, &xtstart, &xtstep);
     setstep(movie, frame, movie->ytmin, movie->ytmax, &ytstart, &ytstep);
     setstep(movie, frame, movie->ztmin, movie->ztmax, &ztstart, &ztstep);

     a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
     a->movie = 0;
     a->moveall = 0;

     zfac = pow ((double)(zstart + zstep * frame) / zstart,
		 1.0 / (double)frame);

     vw = a->imod->view;
     vw->rad   = zstart;
     vw->rot.x = astart;
     vw->rot.y = bstart;
     vw->rot.z = gstart;
     vw->trans.x = xtstart;
     vw->trans.y = ytstart;
     vw->trans.z = ztstart;
     mat = imodMatNew(3);
     mati = imodMatNew(3);
     matp = imodMatNew(3);

     /* get incremental rotation matrix */

     delangle = 360. / frames;
     if (movie->reverse)
          delangle *= -1.0;

     if(fullaxis < 0)
          imodMatRot(mati, delangle, X);
     else if(fullaxis > 0)
          imodMatRot(mati, delangle, Y);
     else {

       /* In general case, net change is final matrix times inverse of starting
	  matrix - then find the vector and angle for that rotation and
	  divide angle by # of frames to get incremental matrix */

	  imodMatRot(mat, (double)-astart, X);
	  imodMatRot(mat, (double)-bstart, Y);
	  imodMatRot(mat, (double)-gstart, Z);
	  imodMatRot(mat, (double)(gstart + frame * gstep), Z);
	  imodMatRot(mat, (double)(bstart + frame * bstep), Y);
	  imodMatRot(mat, (double)(astart + frame * astep), X);
	  imodMatFindVector(mat, &angle, &v);
	  delangle = angle / frame;
	  if (movie->longway)
	       delangle = (angle - 360.) / frame;
	  imodMatRotateVector(mati, delangle, &v);
     }

     for(frame = 0; frame < frames; frame++){
	  if (movie->saved)
	       imodv_auto_snapshot(NULL, movie->file_format);
	  else
	       imodvDraw(a);

	  xinput(); 

	  if (abort_movie)
	       break;

	  /* DNM: don't change the angle after the last step */
	  if (frame < frames - 1){

	    /* change zoom by a factor, not an increment */
	    vw->rad   *= zfac;

	    /* Get current rotation matrix, multiply by increment rotation,
	       and convert back to angles */
	    imodMatId(mat);
	    imodMatRot(mat, (double)vw->rot.z, Z);
	    imodMatRot(mat, (double)vw->rot.y, Y);
	    imodMatRot(mat, (double)vw->rot.x, X);
	    imodMatMult(mat, mati, matp);
	    imodMatGetNatAngles(matp, &alpha, &beta, &gamma);

	    vw->rot.x = alpha;
	    vw->rot.y = beta;
	    vw->rot.z = gamma;
	    vw->trans.x += xtstep;
	    vw->trans.y += ytstep;
	    vw->trans.z += ztstep;
	  }
     }
     abort_movie = True;

     imodMatDelete(mat);
     imodMatDelete(mati);
     imodMatDelete(matp);
     return;
}

/* Routine to make a montage */
static void imodvMakeMontage(struct imodvMovieDialogStruct *movie)
{
     ImodvApp *a = movie->a;
     Iview *vw;
     char *string;
     int frames, overlap;
     Ipoint transave;
     Imat *mat;
     Ipoint ipt, spt, xunit, yunit;
     float scrnscale, radsave;
     int ix, iy;
     float zoom, yzoom;

     /* Get the frames and overlap; limit the overlap */
     string = XmTextGetString(movie->montframes);
     frames = atoi(string);
     XtFree(string);
     string = XmTextGetString(movie->montoverlap);
     overlap = atoi(string);
     XtFree(string);

     if (frames <= 1)
	  return;
     if (overlap < 0)
	  overlap = 0;
     if (overlap > a->winx / 2)
	  overlap = a->winx / 2;
     if (overlap > a->winy / 2)
	  overlap = a->winy / 2;

     a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
     a->movie = 0;
     a->moveall = 0;

     /* Save current zoom and translations */
     vw = a->imod->view;
     radsave = vw->rad;
     transave = vw->trans;

     /* new zoom is minimum of zoom needed to get each dimension to work */
     zoom = (a->winx + (frames - 1) * (a->winx - overlap)) / (float)a->winx;
     yzoom = (a->winy + (frames - 1) * (a->winy - overlap)) / (float)a->winy;
     if (zoom > yzoom)
	  zoom = yzoom;
     vw->rad /= zoom;

     /* Compute translation offsets implied by the given pixel shifts in X and
	Y in the display, using same code as imodv_translated */
     mat = imodMatNew(3);
     imodMatId(mat);
     imodMatRot(mat, -(double)vw->rot.x, X);
     imodMatRot(mat, -(double)vw->rot.y, Y);
     imodMatRot(mat, -(double)vw->rot.z, Z);
     
     scrnscale = 0.5 * (a->winx > a->winy ? a->winy : a->winx) / vw->rad;
    
     spt.x = 1.0f/scrnscale;
     spt.y = 1.0f/scrnscale;
     spt.z = 1.0f/scrnscale * 1.0f/a->imod->zscale;
     imodMatScale(mat, &spt);
    
     ipt.x = a->winx - overlap;
     ipt.y = 0.;
     ipt.z = 0.;
     imodMatTransform(mat, &ipt, &xunit);
     xunit.x *= (1.0/ vw->scale.x);
     xunit.y *= (1.0/ vw->scale.y);
     xunit.z *= (1.0/ vw->scale.z);

     ipt.x = 0.;
     ipt.y = a->winy - overlap;
     imodMatTransform(mat, &ipt, &yunit);
     yunit.x *= (1.0/ vw->scale.x);
     yunit.y *= (1.0/ vw->scale.y);
     yunit.z *= (1.0/ vw->scale.z);
    
     /* do initial displacement to lower left corner */
     vw->trans.x += 0.5 * (frames - 1.) * (xunit.x + yunit.x) ;
     vw->trans.y += 0.5 * (frames - 1.) * (xunit.y + yunit.y) ;
     vw->trans.z += 0.5 * (frames - 1.) * (xunit.z + yunit.z) ;


     for(iy = 0; iy < frames; iy++){
	  for(ix = 0; ix < frames; ix++){
	       if (movie->saved)
		    imodv_auto_snapshot(NULL, movie->file_format);
	       else
		    imodvDraw(a);

	       xinput(); 

	       if (abort_movie)
		    break;

	       /* Each X, advance along row */
	       vw->trans.x -= xunit.x;
	       vw->trans.y -= xunit.y;
	       vw->trans.z -= xunit.z;
	  }

	  /* End of row: advance in Y, move X back to start of next row */
	  vw->trans.x -= yunit.x - frames * xunit.x;
	  vw->trans.y -= yunit.y - frames * xunit.y;
	  vw->trans.z -= yunit.z - frames * xunit.z;
	  if (abort_movie)
	       break;
     }
     abort_movie = True;
     vw->rad = radsave;
     vw->trans = transave;
     imodvDraw(a);
     imodMatDelete(mat);
     return;
}
