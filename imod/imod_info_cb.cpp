/*  IMOD VERSION 2.50
 *
 *  imod_info_cb.c -- Callback functions for the Information Window.
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
Log at end of file
*/

#include <stdlib.h>
#include <math.h>
#include <qapplication.h>
#include "imod_object_edit.h"
#include "form_info.h"

#include "imod_info.h"
#include "imod.h"
#include "imod_display.h"
#include "imod_edit.h"
#include "imod_input.h"
#include "imod_cont_edit.h"
#include "imod_io.h"
#include "imod_iscale.h"
#include "imod_info_cb.h"
#include "preferences.h"
#include "xcramp.h"
#include "xzap.h"
#include "control.h"

extern "C" {
int sampleMeanSD(unsigned char **image, int type, int nx, int ny,
                 float sample, int nxMatt, int myMatt, int nxUse, int nyUse,
                 float *mean, float *sd);
}

static void getSampleLimits(ViewInfo *vw, int &ixStart, int &iyStart, 
                            int &nxUse, int &nyUse, float &sample);

/* Global variable: the forbid level, hope to eliminate */
int ImodForbidLevel = 0;

static int ctrlPressed = 0;
static int Imod_obj_cnum = -1;
static int float_on = 0;
static int doingFloat = 0;
static int float_subsets = 0;

/*
 * FUNCTIONS FOR THE CONTROLS TO REPORT CHANGES
 *
 * New object, contour, or point
 */
void imodInfoNewOCP(int which, int value, int edited)
{
  int ob, co, pt;
  Imod *imod = App->cvi->imod;
  imodGetIndex(imod, &ob, &co, &pt);

  ImodInfoWin->setFocus();

  // Get to index value; if it is illegal, try to refresh window and return
  value--;
  if (value < 0) {
    imod_info_setocp();
    return;
  }

  switch (which) {
  case 0:
    // Object change.  If via the edit box, detach from contour/point
    if (edited) {
      imod->cindex.object = value;
      imod->cindex.contour =  - 1;
      imod->cindex.point =  - 1;
    } else {
      imodSetIndex(imod, value, co, pt);
      inputKeepContourAtSameTime(App->cvi);
    }
    break;

  case 1:
    // Contour change.  If via edit box, detach from point
    if (edited) {
      imod->cindex.contour = value;
      imod->cindex.point = -1;
    } else {
      imodSetIndex(imod, ob, value, pt);
      inputRestorePointIndex(App->cvi);
    }
    break;

  case 2:
    // Point change
    imodSetIndex(imod, ob, co, value);
    break;
  }

  // This takes care of many updates through redraw and imod_info_setocp
  // Set active to 0 to get new point to sync correctly
  ivwControlActive(App->cvi, 0);
  imod_setxyzmouse();
}

/*
 * New point position
 */
void imodInfoNewXYZ(int *values)
{
  ImodInfoWin->setFocus();
  App->cvi->xmouse = values[0] - 1;
  App->cvi->ymouse = values[1] - 1;
  App->cvi->zmouse = values[2] - 1 ;
  imodDraw(App->cvi, IMOD_DRAW_XYZ);
}

/*
 * New positions of the black/white sliders
 */
void imodInfoNewBW(int which, int value, int dragging)
{
  int white, black;
  int float_save = float_on;

  // THIS IS REALLY SCARY UNLESS WE ASSERT THE SLIDERS BACK
  if (ImodForbidLevel) {
    ImodInfoWidget->setBWSliders(App->cvi->black, App->cvi->white);
    return;
  }

  // Exit if RGBA and there is not a hot slider active
  if (App->rgba && dragging && 
      ((ctrlPressed && hotSliderFlag() != HOT_SLIDER_KEYDOWN) || 
       (!ctrlPressed && hotSliderFlag() != HOT_SLIDER_KEYUP)))
    return;

  // Keep the sliders from crossing
  if (which) {
    white = value;
    black = App->cvi->black;
    if (black > white) {
      black = white;
      ImodInfoWidget->setBWSliders(black, white);
    }
  } else {
    white = App->cvi->white;
    black = value;
    if (black > white) {
      white = black;
      ImodInfoWidget->setBWSliders(black, white);
    }
  }

  xcramp_setlevels(App->cvi->cramp,black,white);
  App->cvi->black = black;
  App->cvi->white = white;

  /* Set the float flag to false to prevent this change from being 
     undone in a redraw */
  float_on = FALSE;
  imod_info_setbw(black, white);
  float_on = float_save;
}

/*
 * Float button, movie-model mode, ctrl key, and quit
 */
void imodInfoFloat(int state)
{
  if (state) {
    float_on = 1;
    imod_info_setbw(App->cvi->black, App->cvi->white);
  } else
    float_on = 0;
}

void imodInfoSubset(int state)
{
  float_subsets = state;
}

/* DNM 6/8/01: fixed bug in getting mode, changed to pass mode to function */
void imodInfoMMSelected(int mode)
{
  imod_set_mmode(mode ? IMOD_MMODEL : IMOD_MMOVIE);

  /* DNM 1/28/02: change from drawing xyz to general drawing */
  imodDraw(App->cvi, IMOD_DRAW_MOD);
  return;
}

void imodInfoCtrlPress(int pressed)
{
  ctrlPressed = pressed;
}

void imodInfoQuit()
{
  imod_quit();
  return;
}


/****************************************************************************/
/* support functions for setting controls                                   */
/****************************************************************************/

/*
 * Set the object color in the controls
 */
void imod_info_setobjcolor(void)
{
  int red, green, blue;
  double clev, th = 32.;
  Iobj *obj;

  obj = imodel_object_get(Model);
  if (!obj){
    red = green = blue = 128;
  }else{
    red   = (int)(255 * obj->red);
    green = (int)(255 * obj->green);
    blue  = (int)(255 * obj->blue);
  }
  QColor backColor(red, green, blue);

  // Observations on 1/12/03: black is hard to see on pure red below 240,
  // and on pure green below 175.
  // Pure blue at 255 is as hard to see as pure green at 90, so its threshold
  // is 255*175/90 = 495
  // But then need to subtract a threshold because dim colors tend not to
  // add correctly
  clev = (red > th ? (red - th) / (240. - th) : 0) + 
    (green > th ? (green - th) / (175. - th) : 0) + 
    (blue > th ? (blue - th) / (495. - th) : 0);
  red = clev > 1.0 ? 0 : 255;

  QColor foreColor(red, red, red);

  ImodInfoWidget->setObjectColor(foreColor, backColor);

}

/*
 * Set the object, contour, and point.
 * InfoControls figures out if things have changed
 */
void imod_info_setocp(void)
{
  Imod *imod = Model;
  int val[3], max[3];
  Iobj *obj;
  Icont *cont;
     
  obj = imodel_object_get(imod);
  cont = imodContourGet(imod);
     
  // Object is either present or not
  max[0] = imod->objsize;
  val[0] = obj ? imod->cindex.object + 1 : 0;

  // If object present, set up contour values; otherwise set for blanks
  if (obj) {
    max[1] = obj->contsize;
    val[1] = cont ? imod->cindex.contour + 1 : 0;
  } else {
    max[1] = -1;
    val[1] = -1;
  }

  // If contour present, set up point values; otherwise set for blanks
  if (cont) {
    max[2] = cont->psize;
    val[2] = cont->psize ? imod->cindex.point + 1 : 0;
  } else {
    max[2] = -1;
    val[2] = -1;
  }
 
  // Send values
  ImodInfoWidget->updateOCP(val, max);
  
  // Update color if object has changed
  if ((Imod_obj_cnum != imod->cindex.object)
      && (imod->cindex.object != -1)){

    Imod_obj_cnum = imod->cindex.object;
    imod_info_setobjcolor();

  }

  // Update dialog boxes if they are open
  imod_object_edit_draw();
  imodContEditSurfShow();
  imodContEditMoveDialogUpdate();
}

/*
 * Set X, Y, Z
 * InfoControls figures out if things have changed
 */
void imod_info_setxyz(void)
{
  int xyz[3], xyzs[3];

  ivwBindMouse(App->cvi);
  xyz[0] = (int)(App->cvi->xmouse + 1);
  xyz[1] = (int)(App->cvi->ymouse + 1);
  xyz[2] = (int)(App->cvi->zmouse + 1.5);
  xyzs[0] = App->cvi->xsize;
  xyzs[1] = App->cvi->ysize;
  xyzs[2] = App->cvi->zsize;

  ImodInfoWidget->updateXYZ(xyz, xyzs);

  /* 12/28/03: with multifile stack in Z, need to notify image scale window */
  if (App->cvi->multiFileZ)
    imodImageScaleUpdate(App->cvi);
}

// A structure to store means and SD's of areas
typedef struct {
  float mean;
  float sd;
  int ixStart;
  int iyStart;
  int nxUse;
  int nyUse;
} MeanSDData;

/* Static variables for keeping track of floating */
static float ref_black = 0.;
static float ref_white = 255.;
static int last_section = 0;
static int last_time = 0;
static MeanSDData *secData = NULL;
static int table_size = 0;
static int tdim = 0;


/*
 * Set the black/white sliders, draw if necessary
 */
void imod_info_setbw(int black, int white)
{
  static int oblack = 0;
  static int owhite = 255;
  int changed = FALSE;

  if (oblack != black || owhite != white){
    oblack = black;
    owhite = white;
    ImodInfoWidget->setBWSliders(black, white);
    changed = TRUE;
  }

  /* if we are using a colormap that isn't
   * mutable then we need to redraw all image data, unless float is being done
   * use the IMOD_DRAW_IMAGE flag to redraw all image
   * data and clear all image caches, and IMOD_DRAW_NOSYNC to prevent
   * panning the zap window to the current model point
   */
  if (changed && App->rgba && !doingFloat)
    imodDraw(App->cvi, IMOD_DRAW_IMAGE | IMOD_DRAW_NOSYNC);

  /* But for color index mode, just do a draw that sets colormaps */
  else if (changed && !App->rgba)
    imodDraw(App->cvi, IMOD_DRAW_COLORMAP);

  /* DNM: set this information as values for a new reference section for
     floated intensities */
  ref_black = black;
  ref_white = white;
  return;
}

// Get the limits for sampling: if we are not using subset floating or if
// there is no useful data from a zap window, use the whole image
static void getSampleLimits(ViewInfo *vw, int &ixStart, int &iyStart, 
                            int &nxUse, int &nyUse, float &sample)
{
  float matt = 0.05;
  if (!float_subsets || zapSubsetLimits(vw, ixStart, iyStart, nxUse, nyUse)) {
    ixStart = matt * vw->xsize;
    nxUse = vw->xsize - 2 * ixStart;
    iyStart = matt * vw->ysize;
    nyUse = vw->ysize - 2 * iyStart;
  }
  sample = 10000.0/(nxUse * nyUse);
  if (sample > 1.0)
    sample = 1.0;
}

/* Implements floating; i.e. adjusting of sliders according to changes in the
   mean and SD between images 
   Returns 0 if nothing was changed, or 1 if black/white levels changed */
int imod_info_bwfloat(ImodView *vw, int section, int time)
{
  float sample;
  int i, newwhite, newblack, err1;
  int needsize, iref, isec;
  int ixStart, iyStart, nxUse, nyUse;
  float sloperatio, tmp_black, tmp_white;
  unsigned char **image;
  int retval = 0;

  if (float_on) {

    /* Make sure table exists and is the right size */
    tdim = ivwGetMaxTime(vw) + 1;
    needsize = vw->zsize * tdim;
    if (table_size == 0)
      secData = (MeanSDData *)malloc(needsize * sizeof(MeanSDData));
    else if (table_size != needsize)
      secData = (MeanSDData *)realloc(secData, needsize * sizeof(MeanSDData));

    if (!secData) {
      imod_info_float_clear(-1, -1);
      return 0;
    }
	  
    /* Clear out any new entries */
    if (table_size < needsize)
      for (i = table_size; i < needsize; i++)
        secData[i].mean = secData[i].sd = -1;
    table_size = needsize;

    if ((last_section + 1) * (last_time + 1) > table_size ||
        (section + 1) * (time + 1) > table_size)
      return 0;

    if (time > 0 && last_time == 0)
      last_time = 1;

    // Get the limits for sampling and if they do not match the last limits
    // then set up to recompute the mean/sd
    getSampleLimits(vw, ixStart, iyStart, nxUse, nyUse, sample);
    iref = tdim * last_section + last_time;
    isec = tdim * section + time;

    /* Get information about reference and current sections if necessary;
     i.e. if there is no SD already or if area doesn't match*/
    err1 = 0;
    if (secData[iref].sd < 0. || ixStart != secData[iref].ixStart || 
        iyStart != secData[iref].iyStart || 
        nxUse != secData[iref].nxUse || nyUse != secData[iref].nyUse) {
      image = ivwGetZSectionTime(vw, last_section, last_time);
      err1 = sampleMeanSD(image, 0, vw->xsize, vw->ysize, sample,
                          ixStart, iyStart, nxUse, nyUse, 
                          &secData[iref].mean, &secData[iref].sd);
      if (!err1 && secData[iref].sd < 0.1)
        secData[iref].sd = 0.1;

      /* Adjust for compressed data in 8-bit CI mode */
      if (!err1 && App->depth == 8)
	secData[iref].mean = (secData[iref].mean - vw->rampbase) * 256. /
          vw->rampsize;
    }
	       
    if (!err1 && (secData[isec].sd < 0. || ixStart != secData[isec].ixStart || 
        iyStart != secData[isec].iyStart || 
        nxUse != secData[isec].nxUse || nyUse != secData[isec].nyUse)) {
      image = ivwGetZSectionTime(vw, section, time);
      err1 = sampleMeanSD(image, 0, vw->xsize, vw->ysize, sample,
                          ixStart, iyStart, nxUse, nyUse, 
                          &secData[isec].mean, &secData[isec].sd);
      if (!err1 && secData[isec].sd < 0.1)
        secData[isec].sd = 0.1;
      if (!err1 && App->depth == 8)
	secData[isec].mean = (secData[isec].mean - vw->rampbase) * 256. /
          vw->rampsize;
    }
	       
    if (!err1) {
      secData[iref].ixStart = secData[isec].ixStart = ixStart;
      secData[iref].nxUse = secData[isec].nxUse = nxUse;
      secData[iref].iyStart = secData[isec].iyStart = iyStart;
      secData[iref].nyUse = secData[isec].nyUse = nyUse;

      /*printf("ref %.2f %.2f  sec %.2f %.2f\n", secData[iref].mean,
        secData[iref].sd, secData[isec].mean, secData[isec].sd); */

      /* Compute new black and white sliders; keep floating values */
      sloperatio = secData[isec].sd / secData[iref].sd;

      tmp_black = (secData[isec].mean - 
        (secData[iref].mean - ref_black) * sloperatio);
      tmp_white = (tmp_black + sloperatio * (ref_white - ref_black));
		    
      /* printf("ref_bw %.2f %.2f  tmp_bw %.2f %.2f\n", ref_black, ref_white,
         tmp_black, tmp_white); */
      if (tmp_black < 0)
        tmp_black = 0.;
      if (tmp_white > 255.)
        tmp_white = 255.;

      newblack = (int)(tmp_black + 0.5);
      newwhite = (int)(tmp_white + 0.5);
      /* int meanmap = 255 * (secData[isec].mean - newblack) / (newwhite - newblack);
      float sdmap = 255 * (secData[isec].sd) / (newwhite - newblack);
      printf("mean = %d  sd = %.2f\n", meanmap, sdmap); */


      /* Set the sliders and the ramp if the integer values changed*/
      if (newwhite != vw->white || newblack != vw->black) {
        vw->black = newblack;
        vw->white = newwhite;
        xcramp_setlevels(vw->cramp, vw->black, vw->white);
	doingFloat = 1;
        imod_info_setbw(vw->black, vw->white);
	doingFloat = 0;
        retval = 1;
      }
      ref_black = tmp_black;
      ref_white = tmp_white;
    }
  }

  last_section = section;
  last_time = time;
  return retval;
}

/* Clear the information for floating sections - for one section or all 
   section < 0 and time < 0: clear entire table
   section = - number of sections and time >=0; clear all at this time
   section >= 0, time >= 0; just clear this section
*/
void imod_info_float_clear(int section, int time)
{
  int i;

  if (section < 0 && time < 0) {
    if (secData)
      free(secData);
    secData = NULL;
    table_size = 0;
  } else if (section < 0) {
    /* DNM 11/14/03: fix to test actual highest index */
    if ((-section - 1) * tdim + time >= table_size)
      return;
    for (i = 0; i < -section; i++) {
      secData[i * tdim + time].mean = -1;
      secData[i * tdim + time].sd = -1;
    }
  } else if (section * tdim + time < table_size) {
    secData[section * tdim + time].mean = -1;
    secData[section * tdim + time].sd = -1;
  }
  return;
}

// Change the contrast to meet the target mean and SD
void imodInfoAutoContrast(int targetMean, int targetSD)
{
  float mean, sd;
  int black, white, floatSave;
  if (imodInfoCurrentMeanSD(mean, sd))
    return;
  black = (int)(mean - sd * targetMean / targetSD + 0.5);
  white = (int)(mean + sd * (255 - targetMean) / targetSD + 0.5);
  if (black < 0)
    black = 0;
  if (white > 255)
    white = 255;
  App->cvi->black = black;
  App->cvi->white = white;
  xcramp_setlevels(App->cvi->cramp, black, white);
  floatSave = float_on;
  float_on = 0;
  imod_info_setbw(black, white);
  float_on = floatSave;
}

// Return the mean and Sd of the current image, potentially a subarea
int imodInfoCurrentMeanSD(float &mean, float &sd)
{
  float sample;
  int ixStart, iyStart, nxUse, nyUse;
  unsigned char **image;
  ViewInfo *vi = App->cvi;

  // Get the limits to compute within, get images, get the mean & sd
  getSampleLimits(vi, ixStart, iyStart, nxUse, nyUse, sample);
  image = ivwGetZSectionTime(vi, (int)floor(vi->zmouse + 0.5), vi->ct);
  if (!image)
    return 1;
  if (sampleMeanSD(image, 0, vi->xsize, vi->ysize, sample,
                    ixStart, iyStart, nxUse, nyUse, &mean, &sd))
    return 1;
  /* printf("%d %d %d %d %.2f %.2f\n", ixStart, iyStart, nxUse, nyUse, mean,
     sd); */

  /* Adjust for compressed data in 8-bit CI mode */
  if (App->depth == 8)
    mean = (mean - vi->rampbase) * 256. / vi->rampsize;
  return 0;
}

/****************************************************************************/
/*  Imod link functions */

int imod_info_input(void)
{
  // This replaces an XFlush and loop to process X events
  QApplication::flush();
  qApp->processEvents();
  return(0);
}

void show_status(char *info)
{
  if (!info)
    return;

  imod_info_msg(info, " ");
  return;
}

// Unused
void imod_show_info(char *info, int line)
{
  if (!info)
    return;
     
  if (line == 1)
    imod_info_msg(info, NULL);
  if (line == 2)
    imod_info_msg(NULL, info);
  return;
}

void imod_info_msg(char *top, char *bot)
{
  if (top){
    wprint("%s\n", top);
  }

  if (bot){
    wprint("%s\n", bot);
  }
}

void imod_info_forbid(void)
{
  ImodForbidLevel++;
}

void imod_info_enable(void)
{
  ImodForbidLevel--;
  if (ImodForbidLevel < 0)
    ImodForbidLevel = 0;
}

/* DNM 6/8/01: changed so that it gets called with the actual mode to be
   changed to, or with a different value to toggle the mode */
void imod_set_mmode(int mode)
{
  if (Model){
    if (mode == IMOD_MM_TOGGLE) {
      if (Model->mousemode == IMOD_MMOVIE)
        mode = IMOD_MMODEL;
      else
        mode = IMOD_MMOVIE;
    }

    Model->mousemode = mode;
    ImodInfoWidget->setMovieModel(mode == IMOD_MMOVIE ? 0 : 1);

    if (mode == IMOD_MMODEL) {
      App->cvi->xmovie = App->cvi->ymovie = App->cvi->zmovie = 
        App->cvi->tmovie = 0;
    }
  }
  imodDraw(App->cvi, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC);
}

void imod_draw_window(void)
{
  if (Model){
    imod_info_setocp();
  }
  if (App->cvi){
    imod_info_setxyz();
  }
}

void imod_imgcnt(char *string)
{
  wprint("%s\r", string);
  imod_info_input();
  if (App->exiting)
    exit(0);
}

/*
$Log$
Revision 4.9  2003/11/18 19:33:21  mast
fix bug in clearing float tables when reload

Revision 4.8  2003/09/18 05:58:54  mast
Added ability to do floating on a subarea and to set contrast automatically

Revision 4.7  2003/09/16 02:55:14  mast
Changed to access image data using new line pointers

Revision 4.6  2003/04/18 20:14:57  mast
In function that reports loading, add test for exiting

Revision 4.5  2003/03/26 23:23:15  mast
switched from hotslider.h to preferences.h

Revision 4.4  2003/02/27 19:35:39  mast
Removed unneeded imod_open function

Revision 4.3  2003/02/20 16:01:13  mast
Set control inactive to sync to model point when changing contour

Revision 4.2  2003/02/13 22:19:20  mast
round zmouse value for display

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

Revision 1.1.2.5  2003/01/29 17:51:36  mast
New floating procedure implemented to avoid zap snake eating tail

Revision 1.1.2.4  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.3  2003/01/23 20:03:54  mast
rationalizing object edit update

Revision 1.1.2.2  2003/01/13 01:00:49  mast
Qt version

Revision 1.1.2.1  2003/01/06 15:52:16  mast
changes for Qt version of slicer

Revision 3.5.2.3  2002/12/19 04:37:12  mast
Cleanup of unused global variables and defines

Revision 3.5.2.2  2002/12/09 17:42:32  mast
remove include of zap

Revision 3.5.2.1  2002/12/05 16:29:32  mast
add include of imod_object_edit.h

Revision 3.5  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.4  2002/11/25 19:21:40  mast
In imod_info_setxyz, elimiated call to redraw pixelview; this is now
in the control list for redrawing

Revision 3.3  2002/09/27 19:54:14  rickg
Reverted calls to LoadModel to match changes to imod_io
Removed or commented out unreferenced variables.

Revision 3.2  2002/09/13 21:08:42  mast
Changed call to LoadModel to add NULL filename argument

Revision 3.1  2002/01/29 03:10:00  mast
Call imodDraw instead of xyz_draw after changing model/movie mode

*/
