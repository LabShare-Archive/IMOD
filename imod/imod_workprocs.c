/*  IMOD VERSION 2.50
 *
 *  imod_workprocs.c -- imod timer and background processing functions.
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
    Revision 3.4  2002/10/22 22:44:03  mast
    Fixed bug that was exposed by the EXPOSE_HACK (needed to clear the
    movietimeout value when done).  Eliminated unused code.

    Revision 3.3  2002/09/11 04:33:40  mast
    Correct comments on crashes from dual-processor to Ti 4600

    Revision 3.2  2002/09/10 19:49:35  mast
    Needed to flush events before movie image draw as well as after, to
    prevent crashes possibly due to expose events after a resize with
    Nvidia Ti 4600

    Revision 3.1  2002/01/28 16:47:12  mast
    Fixed problem with movie rate counter when movie was stopped with mouse
    button

*/

#include <stdlib.h>
#include <diaP.h>
#include "imod.h"

/*
 * Autosave workproc.
 *
 */
static int AUTOSAVE_TIMEOUT = 300000;

void imodv_anim(void);

void imod_autosave_to(XtPointer client, XtIntervalId *id)
{
     if (Model)
	  imod_autosave(Model);

     XtAppAddTimeOut(Dia_context,  AUTOSAVE_TIMEOUT, imod_autosave_to, NULL);
}

/* DNM: make negative numbers specify seconds instead of minutes */

int imod_start_autosave()
{
    char *userto = getenv("IMOD_AUTOSAVE");
    if (userto){
	AUTOSAVE_TIMEOUT = atoi(userto);
	if (AUTOSAVE_TIMEOUT == 0) 
	    return(0);
	else if (AUTOSAVE_TIMEOUT < 0)
	    AUTOSAVE_TIMEOUT *= - 1000;
	else 
	    AUTOSAVE_TIMEOUT *= 60000;
    }

     XtAppAddTimeOut(Dia_context, AUTOSAVE_TIMEOUT, imod_autosave_to, NULL);
     return(0);
}


/*
 * Movie for xyz images
 * Movie time outs for tilt window
 * Handle all gl events.
 * imodMovie function
 * Movie for imodv
 * DNM deleted all these obsolete items 10/22/02
 */



/***************************************************************************/
/* new movie workprock added Aug 1996. */
/* DNM: New timing scheme, where the MovieProc starts a MovieTimer as soon
   as it comes in.  If the timer goes off before the MovieProc is done
   displaying images, then the MovieProc sets up a timeout to call itself
   immediately.  If the timer fires after the MovieProc is done, it sets up 
   the timeout to call MovieProc instead.  This maintains a constant interval
   between displays if possible.  With SGIs, the timing is in 10 msec
   increments, and the timeouts round up; unless the interval is exactly a
   multiple of 10, in which case they may end up averaging the next multiple
   of 5.  To compensate for this effect, the call to MovieProc is done with an
   interval of 0.  This seems to be needed on PCs too.  If this is a problem,
   define NO_ZERO_INTERVAL */

#ifndef NO_ZERO_INTERVAL
#define MININTERVAL 0
#else
#define MININTERVAL 1
#endif

static int display_busy, timer_fired;
static int first_frame;
void imodMovieProc(XtPointer client_data, XtIntervalId *id);

static void xinput(void)
{
     XEvent event_return;
     XFlush(XtDisplay(App->toplevel));
     /* DNM: need to either mask for X events in the while, or process ALL
	types of events; use (XtIMXevent | XtIMTimer) or XtIMAll in both
	places */
     while(XtAppPending(App->context) & XtIMXEvent){
	  /*  fprintf(stderr, "processing X event..."); */
	  XtAppProcessEvent(App->context, XtIMXEvent);
	  /* fprintf(stderr, "back\n"); */
     }
     return;
}

/* DNM: generalized routine to get increment and limits, and handle looping
   in the proper way */
static void movie_inc(ImodView *vi, float *mouse, int *movie, int axis,
		      int *show)
{
     int start, end;
     if (*movie != 0){
	  *show = 1;
	  *mouse += *movie * imcGetIncrement(vi, axis);
	  imcGetStartEnd(vi, axis, &start, &end);
	  if (axis == 3) {
	       start++;
	       end++;
	  }
	  if ( *mouse < start){
	       if (imcGetLoopMode(vi))
		    *mouse = end;
	       else {
		    *mouse = start;
		    *movie *= -1;
	       }
	  }
	  if ( *mouse > end){
	       if (imcGetLoopMode(vi))
		    *mouse = start;
	       else {
		    *mouse = end;
		    *movie *= -1;
	       }
	  }
     }
}

void imodMovieTimer(XtPointer client_data, XtIntervalId *id)
{
     struct ViewInfo *vi = (struct ViewInfo *)client_data;
     if (display_busy) {
          timer_fired = True;
	  
	  /* DNM 10/22/02: need to zero out this value to keep the startup 
	     routine from clearing other timeouts by mistake */
	  vi->movieTimeOut = 0;
     } else
          vi->movieTimeOut = XtAppAddTimeOut(Dia_context, MININTERVAL,
					     imodMovieProc, client_data);
}

void imodMovieProc(XtPointer client_data, XtIntervalId *id)
{
     unsigned int interval; /* milliseconds */
     struct ViewInfo *vi = (struct ViewInfo *)client_data;
     int show = 0;
     long drawflag = IMOD_DRAW_XYZ;
     int start, end, signedint;
     float timetmp;

     /* Set flag that this routine is busy, and start the timer */
     display_busy = True;
     timer_fired = False;
     interval = (unsigned int)(imcGetInterval() - 0.5);
     vi->movieTimeOut = XtAppAddTimeOut(Dia_context, interval,
					imodMovieTimer, client_data);
     
     
     /* printf("movieProc(%d, %d, %d), xyz( %g, %g, %g)\n",
	    vi->xmovie, vi->ymovie, vi->zmovie, vi->xmouse, vi->ymouse,
	    vi->zmouse); */

     movie_inc(vi, &vi->xmouse, &vi->xmovie, 0, &show);
     movie_inc(vi, &vi->ymouse, &vi->ymovie, 1, &show);
     movie_inc(vi, &vi->zmouse, &vi->zmovie, 2, &show);

     timetmp = vi->ct;
     movie_inc(vi, &timetmp, &vi->tmovie, 3, &show);
     vi->ct = (int)timetmp;

     if (vi->tmovie != 0){
	  vi->hdr = vi->image = &vi->imageList[vi->ct-1];
	  drawflag |= IMOD_DRAW_IMAGE;
     }
     
     if (first_frame)
	  imcStartTimer();
     else
	  imcReadTimer();
     first_frame = False;

     /* If none of the axes has a movie, return; the display_busy flag will
	keep the timer from restarting the movie, and it will clear 
	movieTimeout */
     if (!show)
	  return;

     /* DNM 9/10/02: Process events before the draw to prevent expose events
	from crashing with Ti 4600 */
     xinput();

     /*  fprintf(stderr, "calling imodDraw..."); */
     imodDraw(vi, drawflag);
     /* fprintf(stderr, "back\n"); */

     /* Process all events to allow the timer to fire */
     xinput();
     display_busy = False;
     if (timer_fired) 
          vi->movieTimeOut = XtAppAddTimeOut
	    (Dia_context, MININTERVAL, imodMovieProc, client_data);
}

static int setmovievar(int set, int movie)
{
     switch(set){
	case 0:
	  return (0);
	case MOVIE_DEFAULT:
	  return movie;
	default:
	  if (movie) return (0);
	  return(set);
     }
}

/* Modify the movie flags and restart movie if needed:
   for each of the four axes, pass 1 to toggle the movie on that axis,
   0 to stop the movie, and MOVIE_DEFAULT to leave it unmodified */
int imodMovieXYZT(struct ViewInfo *vi, int x, int y, int z, int t)
{
     unsigned int interval; /* milliseconds */
     
     vi->xmovie = setmovievar(x, vi->xmovie);
     vi->ymovie = setmovievar(y, vi->ymovie);
     vi->zmovie = setmovievar(z, vi->zmovie);
     vi->tmovie = setmovievar(t, vi->tmovie);

     if (vi->movieTimeOut){
	  XtRemoveTimeOut(vi->movieTimeOut);
	  vi->movieTimeOut = 0;
     }

     interval = (unsigned int)(imcGetInterval() + 0.5);

     /* DNM 1/25/02: test whether the omvie is ending before setting 
	first_frame */
     if (vi->xmovie || vi->ymovie || vi->zmovie || vi->tmovie)
	  first_frame = True;
     
     vi->movieTimeOut = 
	  XtAppAddTimeOut(Dia_context, interval, imodMovieProc, (XtPointer)vi);
     return 0;
}
