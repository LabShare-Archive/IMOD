/*  IMOD VERSION 2.50
 *
 *  imod_moviecon.c -- Movie controller dialog
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
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
Revision 1.1.2.7  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.6  2003/01/14 22:00:26  mast
fix aberrant character

Revision 1.1.2.5  2003/01/14 21:37:55  mast
revised help, cleaned up unused variables

Revision 1.1.2.4  2003/01/14 17:05:17  mast
Qt version

Revision 1.1.2.3  2002/12/17 21:39:14  mast
include imodconfig so NO_SYS_TIMES canbe defined

Revision 1.1.2.2  2002/12/17 18:40:24  mast
Changes and new includes with Qt version of imodv

Revision 1.1.2.1  2002/12/15 21:14:02  mast
conversion to cpp

Revision 3.2  2002/12/01 16:51:34  mast
Changes to eliminate warnings on SGI

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <limits.h>
#include "imodconfig.h"
#ifndef NO_SYS_TIMES
#include <sys/times.h>
#ifdef FIXED_CLK_TCK
#define USE_CLK_TCK FIXED_CLK_TCK
#else
#define USE_CLK_TCK CLK_TCK
#endif
#endif

#include "form_moviecon.h"
#include "imodv_input.h"
#include "imod_moviecon.h"
#include "imod.h"
#include "imodv.h"
#include "dia_qtutils.h"
#include "control.h"

/* Local variables */
/* There will be trouble if multiple views are ever implemented */
static int start[4], end[4], increment[4];
static int maxend[4];
static int firsttime = 1;
static int axiscon;
static MovieController *dia = NULL;
static ImodView *view;
static float realrate, realint;
static int looponeway;
static int autosnap;
static int movieCount = 0;
static clock_t movieCurrent, movieStart, movieLast;

#define BASEINT 50.0
#define RATEFAC 1.25992
#define MINRATE 0.2
#define MAXRATE 200.

static void set_sliders(void);

/* Set the slider values and maxima based on settings for the current axis */
static void set_sliders()
{
  int startmax, endmin, endmax, turnon;

  startmax = maxend[axiscon];
     
  turnon = (startmax >= 2) ? 1 : 0;
  if (!turnon)
    startmax = 2;

  endmin = 2;
  endmax = maxend[axiscon] + 1;
  if (!turnon) {
    endmin = endmax;
    endmax++;
  }

  dia->setSliders( start[axiscon] + 1, startmax, end[axiscon] + 1, endmin,
                   endmax, increment[axiscon], turnon);
}

/*
 * FUNCTIONS FOR THE REST OF THE PROGRAM TO ACCESS/CONTROL MOVIE PROPERTIES
 *
 * Reset all axes to full range, increment 1
 */
void imcResetAll(ImodView *vw)
{
  int i;
  for (i = 0; i < 4; i++) {
    start[i] = 0;
    increment[i] = 1;
  }
  end[0] = vw->xsize - 1;
  end[1] = vw->ysize - 1;
  end[2] = vw->zsize - 1;
  end[3] = vw->nt - 1;
  for (i = 0; i < 4; i++) {
    if (end[i] < 0) end[i] = 0;
    maxend[i] = end[i];
  }
  looponeway = 0;
  autosnap = 0;
  firsttime = 0;
  if (dia)
    set_sliders();
}

/* Get the increment for an axis */
int imcGetIncrement(ImodView *vw, int xyzt)
{
  if (firsttime)
    imcResetAll(vw);
  return increment[xyzt];
}

/* Get the looping mode for auto snapshots */
int imcGetLoopMode(ImodView *vw)
{
  if (firsttime)
    imcResetAll(vw);
  return looponeway;
}

/* Get the snaphot flag */
int imcGetSnapshot(ImodView *vw)
{
  if (firsttime)
    imcResetAll(vw);
  return autosnap;
}

/* Get start and end for the axis */
void imcGetStartEnd(ImodView *vw, int xyzt, int *stout, int *endout)
{
  if (firsttime)
    imcResetAll(vw);
  *stout = start[xyzt];
  *endout = end[xyzt];
}

float imcGetInterval()
{
  return realint;
}

/* Set the movie rate by changing the interval */
void imcSetMovierate(ImodView *vw, int newrate)
{
  do {
    vw->movierate = newrate;
    realint = BASEINT * pow(RATEFAC, (double)newrate);
    realrate = 1000. / realint;
    if (realrate > MAXRATE)
      newrate++;
    if (realrate < MINRATE)
      newrate--;
  } while (realrate < MINRATE || realrate > MAXRATE);

  if (dia)
    dia->setRateBox(realrate);
}


/*
 * Routines for keeping track of actual timing 
 */
/* DNM 5/21/01: add an actual frame rate output to the box; the variant if
   there is no times function is pretty lame */

void imcStartTimer(void)
{
  movieCount = 0;
  movieStart = imodv_sys_time();
  movieCurrent = movieStart;
}

void imcReadTimer(void)
{
  QString qstr;
#ifdef NO_SYS_TIMES
  char persec[] = "CPU sec";
#else
  char persec[] = "sec";
#endif

  float elapsed, instrate;

  movieCount++;
  movieLast = movieCurrent;
  movieCurrent = imodv_sys_time();
  if (!dia || movieCurrent == movieLast)
    return;

#ifdef NO_SYS_TIMES
  elapsed = (float)(movieCurrent - movieStart) / (float)CLOCKS_PER_SEC;
  instrate = (float)CLOCKS_PER_SEC / (float)(movieCurrent - movieLast);
#else
  elapsed = (float)(movieCurrent - movieStart) / (float)USE_CLK_TCK;
  instrate = (float)USE_CLK_TCK / (float)(movieCurrent - movieLast);
#endif
  qstr.sprintf("Actual /%s: %5.2f (avg)  %5.2f", persec,
	  movieCount / elapsed, instrate);
  dia->setActualRate(qstr);
}

/*
 * Open the dialog box
 */
void imodMovieConDialog(ImodView *vw)
{
  if (dia){
    dia->raise();
    return;
  }

  view = vw;
  imcResetAll(view);

  dia = new MovieController(NULL, NULL, 
			    Qt::WType_TopLevel | Qt::WDestructiveClose);
  dia->setCaption(imodCaption("Imod Movies"));

  imodDialogManager.add((QWidget *)dia, IMOD_DIALOG);

  axiscon = 2;
  set_sliders();
  dia->setRateBox(realrate);
  dia->enableTime(vw->nt);
  dia->show();
}

/****************************************************************************/
/*  Dialog controls.                                                 */

void imcHelp()
{
  dia_vasmsg
    ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
     "Imod Movie Controller \n"
     "~~~~~~~~~~~~~~~~~~~~~~~~"
     "\n\n",
     "This window allows you to control the starting and ending "
     "coordinates within which a movie will run, the increment between "
     "successive frames, and the frame rate.\n\n"
     "Use the axis radio buttons to select whether the sliders will adjust "
     "the start, end, and increment for X, Y, Z or Time.  The initial "
     "default setting is for the sliders to adjust movies in Z.\n\n"
     "The [Reset] button will reset the start, end, and increment to "
     "their initial settings (the minimum and maximum coordinates and "
     "an increment of 1) for all four axes.\n\n"
     "The [Done] button will close the window and reset these values in "
     "the same way.\n\n"
     "The Rate text box will display the current frame rate as it is "
     "changed by the down and up Arrows or by \",\" and \".\" and will "
     "also allow you to enter a custom value.  You must type Enter to "
     "have your value take effect.\n\n",
     "The next line shows two actual frame rates, the first averaged "
     "over the time since the current movie was started, the second "
     "an instantaneous rate since the last frame was displayed.  The "
     "actual rate will be less than the selected rate if the display is "
     "too slow to keep up, and can also differ from the selected rate "
     "due to lack of precision of the system clock.\n\n"
     "If [Round Trip] is selected, then movies will loop from one "
     "end to the other then from that end back to the start; "
     "if [One Way] is selected, then movies will always go in one "
     "direction and jump back to the start when the end is reached.\n\n",
     "If one of the Snapshot buttons [RGB] or [TIFF] is selected, "
     "and a movie is started in a Zap window, then that Zap window "
     "will automatically take an RGB or Tiff snapshot of each frame for "
     "one complete cycle of the movie.  The movie will start at the "
     "starting or the ending section depending on whether it is started "
     "with the middle or the right mouse button.  It will stop when it "
     "reaches the opposite end if [One Way] is selected; otherwise it "
     "will loop back to the end at which it started.\nIf you stop the "
     "movie before it is done with a mouse click in the same window, "
     "then another mouse click in that or a different zap window will "
     "start a completely new movie.  If you stop the movie by "
     "clicking in a different window, then clicking again in a "
     "different window will resume the interrupted movie in the "
     "original window.\n",
     NULL);
  return;
}

/* Buttons pressed */
void imcClosing()
{
  imodDialogManager.remove((QWidget *)dia);
  dia = NULL;
  imcResetAll(view);
}

void imcResetPressed()
{
  imcResetAll(view);
}

/* One of the sliders changed */
void imcSliderChanged(int which, int value)
{
  switch (which) {
  case 0:
    start[axiscon] = value - 1;
    if (start[axiscon] >= end[axiscon] && start[axiscon] + 1 <=
        maxend[axiscon]) {
      end[axiscon] = start[axiscon] + 1;
      set_sliders();
    }
    break;

  case 1:
    end[axiscon] = value - 1;
    if (end[axiscon] <= start[axiscon]) {
      start[axiscon] = end[axiscon] - 1;
      set_sliders();
    }
    break;

  case 2:
    increment[axiscon] = value;
    break;
  }
}

/* Various selections */
void imcAxisSelected(int which)
{
  axiscon = which;
  set_sliders();
}

void imcExtentSelected(int which)
{
  looponeway = which;
}

void imcSnapSelected(int which)
{
  autosnap = which;
}

/* Rate entered through text box */
void imcRateEntered(float value)
{
  realrate = value;
  if (realrate <= 0) {
    imcSetMovierate(view, 0);
    return;
  }

  if (realrate < MINRATE || realrate > MAXRATE) {
    if (realrate < MINRATE)
      realrate = MINRATE;
    if (realrate > MAXRATE)
      realrate = MAXRATE;
    dia->setRateBox(realrate);
  }

  realint = 1000./realrate;
  view->movierate = (int)(log((double)realint / BASEINT) / 
			  log(RATEFAC) + 0.5);
}

/* Up or down buttons */          
void imcIncrementRate(int dir)
{
  imcSetMovierate(view, view->movierate + dir);
}
