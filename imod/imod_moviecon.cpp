/*  imod_moviecon.c -- Movie controller dialog
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 * Log at end
 */

#include <stdlib.h>
#include <math.h>
#include <limits.h>

#include "form_moviecon.h"
#include "imodv_input.h"
#include "imod_moviecon.h"
#include "imod.h"
#include "imodv.h"
#include "dia_qtutils.h"
#include "control.h"

/* Local variables */
/* There will be trouble if multiple views are ever implemented */
static int start[5], end[5], increment[4];
static int maxend[4];
static int firsttime = 1;
static int axiscon;
static MovieController *dia = NULL;
static ImodView *view;
static float realrate, realint;
static int looponeway;
static int startHere;
static int autosnap;
static int movieCount = 0;
static int starterCtrlID;    /* Ctrl ID of window that started movie */
static int specialAxis = -1;      /* Axis on which to use special limits */
static int movieCurrent, movieStart, movieLast;
static int montageFac = 2;
static bool scaleSizes = 0;
static int sizeScaling = 1;
static bool wholeMont = false;
static bool snapMontage = false;

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

/* Update the dialog if open */
void imcUpdateDialog()
{
  if (dia)
    dia->setNonTifLabel();
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

/* Get the flag for whether to start at current position */
int imcStartSnapHere(ImodView *vw)
{
  return startHere;
}

/* Get the snaphot flag */
int imcGetSnapshot(ImodView *vw)
{
  if (firsttime)
    imcResetAll(vw);
  return autosnap;
}

/* Get or set the ID of the control that started movie, reset special axis */
int imcGetStarterID()
{
  return starterCtrlID;
}
void imcSetStarterID(int value)
{
  starterCtrlID = value;
  specialAxis = -1;
}

/* Get start and end for the axis or limit by special values */
void imcGetStartEnd(ImodView *vw, int xyzt, int *stout, int *endout)
{
  if (firsttime)
    imcResetAll(vw);
  if (xyzt == specialAxis) {
    *stout = start[xyzt] > start[4] ? start[xyzt] : start[4];
    *endout = end[xyzt] < end[4] ? end[xyzt] : end[4];
  } else {
    *stout = start[xyzt];
    *endout = end[xyzt];
  }
}

float imcGetInterval()
{
  return realint;
}

bool imcGetSnapMontage(bool forDoing)
{
  return snapMontage && (!forDoing || dia != NULL);
}
void imcSetSnapMontage(bool state)
{
  snapMontage = state;
}

bool imcGetSnapWholeMont(void)
{
  return wholeMont;
}

void imcSetSnapWholeMont(bool state)
{
  wholeMont = state;
}

int imcGetMontageFactor()
{
  return montageFac;
}

void imcSetMontageFactor(int val)
{
  montageFac = val;
}

bool imcGetScaleSizes(void)
{
  return scaleSizes;
}

void imcSetScaleSizes(bool state)
{
  scaleSizes = state;
}

int imcGetSizeScaling(void)
{
  return sizeScaling;
}

void imcSetSizeScaling(int value)
{
  sizeScaling = value;
}

void imcSetSpecialLimits(int axis, int inStart, int inEnd)
{
  specialAxis = axis;
  start[4] = inStart;
  end[4] = inEnd;
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
  float elapsed, instrate;

  movieCount++;
  movieLast = movieCurrent;
  movieCurrent = imodv_sys_time();
  if (!dia || movieCurrent == movieLast)
    return;

  elapsed = (float)(movieCurrent - movieStart) / 1000.f;
  instrate = 1000.f / (float)(movieCurrent - movieLast);

  qstr.sprintf("Actual FPS: %5.2f (avg)  %5.2f",
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

  dia = new MovieController(imodDialogManager.parent(IMOD_DIALOG), Qt::Window);
  dia->setWindowTitle(imodCaption("3dmod Movies"));

  imodDialogManager.add((QWidget *)dia, IMOD_DIALOG);

  axiscon = 2;
  set_sliders();
  dia->setRateBox(realrate);
  dia->enableTime(vw->nt);
  adjustGeometryAndShow((QWidget *)dia, IMOD_DIALOG);
}

/****************************************************************************/
/*  Dialog controls.                                                 */

void imcHelp()
{
  imodShowHelpPage("imageMovie.html");
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

void imcStartHereSelected(int which)
{
  startHere = which;
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

/*
$Log$
Revision 4.9  2009/01/15 16:33:17  mast
Qt 4 port

Revision 4.8  2008/12/08 17:24:04  mast
Increased interface for montage snapshots

Revision 4.7  2005/09/15 14:25:25  mast
Added zap montage factor functions

Revision 4.6  2004/11/29 19:25:21  mast
Changes to do QImage instead of RGB snapshots

Revision 4.5  2003/12/18 22:43:50  mast
Changes for keeping track of which window started movie and for doing
movie from current point

Revision 4.4  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.3  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.2  2003/02/27 23:08:35  mast
Change to Qt time function

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

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
