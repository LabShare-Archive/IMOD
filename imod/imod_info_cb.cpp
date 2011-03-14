/*
 *  imod_info_cb.c -- Callback functions for the Information Window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdlib.h>
#include <math.h>
#include <qapplication.h>
#include <QTime>
#include "b3dutil.h"
#include "imod_object_edit.h"
#include "form_info.h"

#include "imod_info.h"
#include "imod.h"
#include "imod_display.h"
#include "imod_edit.h"
#include "imod_input.h"
#include "imod_cont_edit.h"
#include "imod_cont_copy.h"
#include "imod_io.h"
#include "imodplug.h"
#include "imod_iscale.h"
#include "imod_info_cb.h"
#include "preferences.h"
#include "xcramp.h"
#include "xzap.h"
#include "b3dgfx.h"
#include "iproc.h"
#include "control.h"
#include "finegrain.h"

static void getSampleLimits(ViewInfo *vi, int &ixStart, int &iyStart, 
                            int &nxUse, int &nyUse, float &sample, int section, int time);
static int infoSliderInRangeToLevel(int slider, int low, int high);
static int infoLevelToSliderInRange(int level, int low, int high);
static int infoLevelToSlider(ImodView *vi, int level);

/* Global variable: the forbid level, hope to eliminate */
int ImodForbidLevel = 0;

static int ctrlPressed = 0;
static int Imod_obj_cnum = -1;
static int float_on = 0;
static int doingFloat = 0;
static int float_subsets = 0;
static int last_subsets = 0;

static int dumpCache = 0;
static int startDump = 0;
static int updateInfoOnly = 0;

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
  Iindex indOld = imod->cindex;

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
      inputRestorePointIndex(App->cvi, &indOld);
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
  ImodView *vi = App->cvi;
  int float_save = float_on;

  // THIS IS REALLY SCARY UNLESS WE ASSERT THE SLIDERS BACK
  if (ImodForbidLevel) {
    ImodInfoWidget->setBWSliders(vi->blackInRange, vi->whiteInRange);
    return;
  }

  // Exit if RGBA and there is not a hot slider active
  if (App->rgba && dragging && !ImodPrefs->hotSliderActive(ctrlPressed))
    return;

  // Keep the sliders from crossing
  if (which) {
    vi->whiteInRange = value;
    white = imodInfoSliderToLevel(vi, value);
    black = vi->black;
    if (black > white || vi->blackInRange > vi->whiteInRange) {
      black = white;
      vi->blackInRange = vi->whiteInRange;
      ImodInfoWidget->setBWSliders(vi->blackInRange, vi->whiteInRange);
    }
  } else {
    vi->blackInRange = value;
    white = vi->white;
    black = imodInfoSliderToLevel(vi, value);
    if (black > white || vi->blackInRange > vi->whiteInRange) {
      white = black;
      vi->whiteInRange = vi->blackInRange;
      ImodInfoWidget->setBWSliders(vi->blackInRange, vi->whiteInRange);
    }
  }

  //imodPrintStderr("Setting black %d white  %d\n", black, white);
  xcramp_setlevels(vi->cramp,black,white);
  vi->black = black;
  vi->white = white;

  /* Set the float flag to false to prevent this change from being 
     undone in a redraw */
  float_on = FALSE;
  imod_info_setbw(black, white);
  float_on = float_save;
}

void imodInfoNewLH(int which, int value, int dragging)
{
  int low, high;
  ImodView *vi = App->cvi;
  int float_save = float_on;

  // Exit if there is not a hot slider active
  if (dragging && !ImodPrefs->hotSliderActive(ctrlPressed))
    return;

  // Keep the sliders from crossing
  if (which) {
    high = value;
    low = vi->rangeLow;
    if (high <= low) {
      low = high - 1;
      ImodInfoWidget->setLHSliders(low, high);
    }
  } else {
    low = value;
    high = vi->rangeHigh;
    if (high <= low) {
      high = low + 1;
      ImodInfoWidget->setLHSliders(low, high);
    }
  }

  vi->rangeLow = low;
  vi->rangeHigh = high;
  vi->black = imodInfoSliderToLevel(vi, vi->blackInRange);
  vi->white = imodInfoSliderToLevel(vi, vi->whiteInRange);
  //imodPrintStderr("Setting black %d white  %d\n", vi->black, vi->white);
  xcramp_setlevels(vi->cramp,vi->black,vi->white);
  float_on = FALSE;
  imod_info_setbw(vi->black, vi->white);
  float_on = float_save;
}

/* 
 * Functions for getting from black/white slider to true black and white values with
 * current or given low and high range settings
 */
int infoSliderInRangeToLevel(int slider, int low, int high)
{
  return B3DNINT(slider * (high - low) / 255. + low);
}

int infoLevelToSliderInRange(int level, int low, int high)
{
  return B3DNINT((255. * (level - low)) / B3DMAX(1,(high - low)));
}

int imodInfoSliderToLevel(ImodView *vi, int slider)
{
  if (!vi->ushortStore)
    return slider;
  return infoSliderInRangeToLevel(slider, vi->rangeLow, vi->rangeHigh);
}

int infoLevelToSlider(ImodView *vi, int level)
{
  if (!vi->ushortStore)
    return level;
  int slider = infoLevelToSliderInRange(level, vi->rangeLow, vi->rangeHigh);
  return B3DMAX(0, B3DMIN(255, slider));
}


/*
 * Float button, movie-model mode, ctrl key, and quit
 */
void imodInfoFloat(int state)
{
  if (state) {
    float_on = 1;
    imod_info_bwfloat(App->cvi, (int)(App->cvi->zmouse + 0.5f), App->cvi->ct);
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

  obj = imodObjectGet(Model);
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
     
  obj = imodObjectGet(imod);
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

  // Clear the selection list if no current contour or it is not on list
  if (!cont || imodSelectionListQuery(App->cvi, imod->cindex.object,
                                     imod->cindex.contour) < -1)
    imodSelectionListClear(App->cvi);

  // Update dialog boxes if they are open and not engaged in continuous draw
  if (updateInfoOnly)
    return;
  imod_object_edit_draw();
  imodContEditSurfShow();
  imodContEditMoveDialogUpdate();
  imodContCopyUpdate();
  fineGrainUpdate();
  imodPlugCall(App->cvi, 0, IMOD_REASON_MODUPDATE);
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
  iprocUpdate();
}

/* Set or clear flag to skip updating other dialogs */
void imodInfoUpdateOnly(int value)
{
  updateInfoOnly = value;
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
static int last_section = -1;
static int last_time = 0;
static MeanSDData *secData = NULL;
static int table_size = 0;
static int tdim = 0;
static int singleCleared = 0;
static int saveNextClear = 0;
static int clearedSection, clearedTime;
static float clearedMean, clearedSD = -1., clearedBlack, clearedWhite;


/*
 * Set the black/white sliders, draw if necessary
 */
void imod_info_setbw(int black, int white)
{
  static int oblack = -1;
  static int owhite = -1;
  static int oLow = -1;
  static int oHigh = -1;
  ImodView *vi = App->cvi;
  int changed = FALSE;
  int spread = 0;
  bool rangeChanged = vi->ushortStore && (oLow != vi->rangeLow || oHigh != vi->rangeHigh);

  if (oblack != black || owhite != white || rangeChanged) {
    oblack = black;
    owhite = white;
    if (vi->ushortStore) {
      if (black < vi->rangeLow) {
        spread = 1;
        vi->rangeLow = black;
      }
      if (white > vi->rangeHigh) {
        spread = 1;
        vi->rangeHigh = white;
      }
      if (spread)
        if (imodDebug('i'))
          imodPrintStderr("imod_info_setbw spread low high %d %d\n", vi->rangeLow,
                          vi->rangeHigh);
      if (spread || rangeChanged)
        ImodInfoWidget->setLHSliders(vi->rangeLow, vi->rangeHigh);
      oLow = vi->rangeLow;
      oHigh = vi->rangeHigh;
    }
    vi->blackInRange = infoLevelToSlider(vi, black);
    vi->whiteInRange = infoLevelToSlider(vi, white);
    if (imodDebug('i'))
      imodPrintStderr("imod_info_setbw bl %d wh %d binr %d winr %d\n", black, white,
                      vi->blackInRange, vi->whiteInRange);
    ImodInfoWidget->setBWSliders(vi->blackInRange, vi->whiteInRange);
    changed = TRUE;
  }

  /* DNM: set this information as values for a new reference section for
     floated intensities - 6/15/05, needed to do this before the draw call */
  ref_black = black;
  ref_white = white;

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

  return;
}

// Get the limits for sampling: if we are not using subset floating or if
// there is no useful data from a zap window, use the whole image
static void getSampleLimits(ViewInfo *vi, int &ixStart, int &iyStart, 
                            int &nxUse, int &nyUse, float &sample, int section, int time)
{
  float matt = 0.05;
  int llX, leftXpad, rightXpad, llY, leftYpad, rightYpad, llZ, leftZpad, rightZpad;

  if (!float_subsets || zapSubsetLimits(vi, ixStart, iyStart, nxUse, nyUse)) {
    ixStart = (int)(matt * vi->xsize);
    nxUse = vi->xsize - 2 * ixStart;
    iyStart = (int)(matt * vi->ysize);
    nyUse = vi->ysize - 2 * iyStart;
  }
  sample = 10000.0/(((double)nxUse) * nyUse);
  if (sample > 1.0)
    sample = 1.0;

  // Constrain to real image if there are multiple images and caller wants it
  if (time < 0 || (!vi->multiFileZ && time <= 0))
    return;

  // Get extent of padded region in image
  if (ivwGetImagePadding(vi, -1, section, time, llX, leftXpad, rightXpad, llY, leftYpad,
                         rightYpad, llZ, leftZpad, rightZpad))
    return;

  if (imodDebug('i'))
    imodPrintStderr("lxpad %d rxpad %d lypad %d rypad %d\n", leftXpad, rightXpad,
                    leftYpad,rightYpad);
  // Change a limit in X or Y to remove padding if new limit has some pixels
  if (leftXpad || leftYpad || rightXpad || rightYpad)
    imodInfoLimitSubarea(leftXpad, vi->xsize - rightXpad, leftYpad, vi->ysize - rightYpad,
                         ixStart, iyStart, nxUse, nyUse);
}

// Limits a subarea in X or Y by padding coordinates if it leaves some pixels
void imodInfoLimitSubarea(int leftXpad, int rightX, int leftYpad, int rightY,
                         int &ixStart, int &iyStart, int &nxUse, int &nyUse)
{
  int iEnd;
  iEnd = B3DMIN(nxUse + ixStart, rightX);
  if (B3DMAX(ixStart, leftXpad) < iEnd - 4) {
    ixStart = B3DMAX(ixStart, leftXpad);
    nxUse = iEnd - ixStart;
  }
  iEnd = B3DMIN(nyUse + iyStart, rightY);
  if (B3DMAX(iyStart, leftYpad) < iEnd - 4) {
    iyStart = B3DMAX(iyStart, leftYpad);
    nyUse = iEnd - iyStart;
  }
}


/* Implements floating; i.e. adjusting of sliders according to changes in the
   mean and SD between images 
   Returns 0 if nothing was changed, or 1 if black/white levels changed */
int imod_info_bwfloat(ImodView *vi, int section, int time)
{
  float sample;
  int i, newwhite, newblack, err1;
  int needsize, iref, isec;
  int ixStart, iyStart, nxUse, nyUse, jxStart, jyStart, mxUse, myUse;
  float sloperatio, tmp_black, tmp_white, refMean, refSD;
  unsigned char **image;
  int retval = 0;
  int sampleType = vi->ushortStore ? 2 : 0;
  int rangeMax = vi->ushortStore ? 65535 : 255;

  // Skip through if this is the first call; there is no reference
  if (float_on && last_section >= 0 && !vi->fakeImage && !vi->rgbStore) {

    /* Make sure table exists and is the right size */
    tdim = ivwGetMaxTime(vi) + 1;
    needsize = vi->zsize * tdim;
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

    // Get generic limits for sampling and if they do not match the last limits
    // then set up to recompute the mean/sd
    getSampleLimits(vi, ixStart, iyStart, nxUse, nyUse, sample, 0, -1);
    iref = tdim * last_section + last_time;
    isec = tdim * section + time;

    /* Get information about reference and current sections if necessary;
     i.e. if there is no SD already or if area doesn't match*/
    err1 = 0;
    if (imodDebug('i'))
      imodPrintStderr("sc %d iref %d isec %d lastsec %d clearedsec %d sd %f\n",
                      singleCleared, iref, isec, last_section, clearedSection, 
                      secData[iref].sd);
    if (singleCleared && iref == isec && last_section == clearedSection && 
        last_time == clearedTime && secData[iref].sd < 0.) {

      // If this is a section that was cleared, use the saved values
      refMean = clearedMean;
      refSD = clearedSD;
      ref_black = clearedBlack;
      ref_white = clearedWhite;

    } else {

      // Get new data now if there is no valid data for the reference section
      // or if the area has changed, but only if something else has changed,
      // namely section, time, or subset versus non-subset
      if (secData[iref].sd < 0. || 
          ((ixStart != secData[iref].ixStart ||
            iyStart != secData[iref].iyStart || 
            nxUse != secData[iref].nxUse || nyUse != secData[iref].nyUse) &&
           (section != last_section || time != last_time || 
            float_subsets != last_subsets))) {
        image = ivwGetZSectionTime(vi, last_section, last_time);
        getSampleLimits(vi, jxStart, jyStart, mxUse, myUse, sample, last_section, 
                        last_time);
        err1 = sampleMeanSD(image, sampleType, vi->xsize, vi->ysize, sample,
                            jxStart, jyStart, mxUse, myUse, 
                            &secData[iref].mean, &secData[iref].sd);
        if (!err1 && secData[iref].sd < 0.1)
          secData[iref].sd = 0.1;
        
        /* Adjust for compressed data in 8-bit CI mode */
        if (!err1 && App->depth == 8)
          secData[iref].mean = (secData[iref].mean - vi->rampbase) * 256. /
            vi->rampsize;
      }

      // Otherwise, take existing data as the reference - allows change
      // for new subset within a section to be tracked
      refMean = secData[iref].mean;
      refSD = secData[iref].sd;
    }
	       
    if (!err1 && (secData[isec].sd < 0. || ixStart != secData[isec].ixStart || 
        iyStart != secData[isec].iyStart || 
        nxUse != secData[isec].nxUse || nyUse != secData[isec].nyUse)) {
      image = ivwGetZSectionTime(vi, section, time);
      getSampleLimits(vi, jxStart, jyStart, mxUse, myUse, sample, section, time);
      err1 = sampleMeanSD(image, sampleType, vi->xsize, vi->ysize, sample,
                          jxStart, jyStart, mxUse, myUse, 
                          &secData[isec].mean, &secData[isec].sd);
      if (!err1 && secData[isec].sd < 0.1)
        secData[isec].sd = 0.1;
      if (!err1 && App->depth == 8)
	secData[isec].mean = (secData[isec].mean - vi->rampbase) * 256. /
          vi->rampsize;
    }
	       
    if (!err1) {
      secData[iref].ixStart = secData[isec].ixStart = ixStart;
      secData[iref].nxUse = secData[isec].nxUse = nxUse;
      secData[iref].iyStart = secData[isec].iyStart = iyStart;
      secData[iref].nyUse = secData[isec].nyUse = nyUse;

      if (imodDebug('i'))
        imodPrintStderr("ref %.2f %.2f  sec %.2f %.2f\n", refMean,
                        refSD, secData[isec].mean, secData[isec].sd);

      /* Compute new black and white sliders; keep floating values */
      sloperatio = secData[isec].sd / refSD;

      tmp_black = (secData[isec].mean - (refMean - ref_black) * sloperatio);
      tmp_white = (tmp_black + sloperatio * (ref_white - ref_black));
		    
      if (imodDebug('i'))
        imodPrintStderr("ref_bw %.2f %.2f  tmp_bw %.2f %.2f\n", ref_black,
                        ref_white, tmp_black, tmp_white);

      /* DNM 3/23/05: needed to truncate only new values, not the tmp_ values
         that will be used to keep track of consistent contrast setting */
      newblack = (int)floor(tmp_black + 0.5);
      newwhite = (int)floor(tmp_white + 0.5);
      newblack = B3DMAX(0, B3DMIN(newblack, rangeMax));
      newwhite = B3DMAX(newblack, B3DMIN(newwhite, rangeMax));
      if (imodDebug('i')) {
        int meanmap = (int)(rangeMax * (secData[isec].mean - newblack) / 
                            B3DMAX(1, newwhite - newblack));
        float sdmap = rangeMax * (secData[isec].sd) / B3DMAX(1,newwhite - newblack);
        imodPrintStderr("mean = %d  sd = %.2f\n", meanmap, sdmap);
      }

      /* Set the sliders and the ramp if the integer values changed*/
      if (newwhite != vi->white || newblack != vi->black) {
        vi->black = newblack;
        vi->white = newwhite;
        xcramp_setlevels(vi->cramp, vi->black, vi->white);
	doingFloat = 1;
        imod_info_setbw(vi->black, vi->white);
	doingFloat = 0;
        retval = 1;
      }
      ref_black = tmp_black;
      ref_white = tmp_white;
      last_subsets = float_subsets;
    }
  }

  last_section = section;
  last_time = time;
  return retval;
}

/* Prepare to save the next float clear call */
void imodInfoSaveNextClear()
{
  saveNextClear = 1;
  if (imodDebug('i'))
    imodPuts("saving next clear");
}

/* Clear the information for floating sections - for one section or all 
   section < 0 and time < 0: clear entire table
   section = - number of sections and time >=0; clear all at this time
   section >= 0, time >= 0; just clear this section
*/
void imod_info_float_clear(int section, int time)
{
  int i;
  int index = section * tdim + time;
  singleCleared = 0;
  if (section < 0 && time < 0) {
    if (secData)
      free(secData);
    secData = NULL;
    table_size = 0;
    last_section = -1;
  } else if (section < 0) {
    /* DNM 11/14/03: fix to test actual highest index */
    if ((-section - 1) * tdim + time >= table_size)
      return;
    for (i = 0; i < -section; i++) {
      secData[i * tdim + time].mean = -1;
      secData[i * tdim + time].sd = -1;
    }
  } else if (index < table_size) {

    // If a single section is being cleared, keep track of its values if flag
    // set
    if (saveNextClear) {
      if (imodDebug('i'))
        imodPuts("saving clear");
      clearedSection = section;
      clearedTime = time;
      clearedMean = secData[index].mean;
      clearedSD = secData[index].sd;
      clearedBlack = ref_black;
      clearedWhite = ref_white;
      saveNextClear = 0;
    }
    secData[index].mean = -1;
    secData[index].sd = -1;
    if (clearedSD >= 0.)
      singleCleared = 1;
  }
  if (imodDebug('i'))
    imodPrintStderr("clear called, sc = %d\n",singleCleared);
  return;
}

// Change the contrast to meet the target mean and SD
void imodInfoAutoContrast(int targetMean, int targetSD)
{
  float mean, sd, scaleLo, scaleHi;
  int black, white, floatSave, loop, nloop, low, high;
  B3dCIImage *image = NULL;
  float sample, wbdiff;
  int ixStart, iyStart, nxUse, nyUse, nxim, nyim, ierr;
  unsigned char **lines = NULL;
  ImodView *vi = App->cvi;
  int rampMax = vi->ushortStore ? 65535 : 255;
  ZapFuncs *zap = getTopZapWindow(false);
  nloop = 1;

  if (zap)
    image = zap->zoomedDownImage(float_subsets, nxim, nyim, ixStart, iyStart, nxUse,
                                 nyUse);

  // If there is a top zap in HQ mode using zoomdown filters, analyze the RGBA image
  // for its mean/sd and change sliders to get the actual display to the target
  // But first do a standard analysis to make sure we have appropriate contrast there
  if (image && nxUse > 0 && nyUse > 0 && nxUse * nyUse > 32 && image->buf > 0)
    nloop = 2;
  for (loop = 0; loop < nloop; loop++) {
    if (loop) {
      /*imodPrintStderr("Got image %d %d %d %d %d %d\n", nxim, nyim, ixStart, iyStart, 
        nxUse, nyUse); */
      
      if (image->buf == 1)
        lines = makeLinePointers(image->id1, nxim, nyim, 4);
      else
        lines = makeLinePointers(image->id2, nxim, nyim, 4);
      if (!lines)
        return;

      sample = B3DMIN(1., 10000.0/(((double)nxUse) * nyUse));
      ierr = sampleMeanSD(lines, 9, nxim, nyim, sample, ixStart, iyStart, nxUse, nyUse,
                          &mean, &sd);
      //imodPrintStderr("%d %f %f %f\n", nxim, sample, mean, sd);
      free(lines);
      if (ierr)
        return;

      wbdiff = (vi->white - vi->black) * sd / targetSD;
      if (wbdiff) {
        black = B3DNINT((wbdiff / 255.) * 
                        (((255. * vi->black) / (vi->white - vi->black) 
                          + mean) * targetSD / sd - targetMean));
        white = black + B3DNINT(wbdiff);
      } else {
        black = white = B3DNINT(mean);
      }
    } else {

      // Otherwise get the mean of the current image
      if (imodInfoCurrentMeanSD(mean, sd, scaleLo, scaleHi))
        return;
      black = B3DNINT(mean - sd * targetMean / targetSD);
      white = B3DNINT(mean + sd * (255 - targetMean) / targetSD);
      if (vi->ushortStore) {

        // Just change range values without setting them because setbw will take care
        // of them, or not, as needed
        vi->rangeLow = scaleLo;
        vi->rangeHigh = scaleHi;
        if (imodDebug('i'))
          imodPrintStderr("low high from pctstretch %d %d\n", vi->rangeLow,vi->rangeHigh);
      }
    }

    black = B3DMIN(rampMax, B3DMAX(0, black));
    white = B3DMIN(rampMax, B3DMAX(0, white));
    if (white - black < 4) {
      ierr = (white + black) / 2;
      black = B3DMAX(0, ierr - 2);
      white = B3DMIN(rampMax, ierr + 2);
    }
    vi->black = black;
    vi->white = white;
    if (vi->ushortStore) {
      if (vi->rangeLow > B3DMAX(0, black - 1024) || 
          vi->rangeHigh < B3DMIN(65535, white + 1024)) {
        vi->rangeLow = B3DMIN(vi->rangeLow, B3DMAX(0, black - 1024));
        vi->rangeHigh = B3DMAX(vi->rangeHigh, B3DMIN(65535, white + 1024));
        if (imodDebug('i'))
          imodPrintStderr("low high spread to exceed black/white %d %d\n",  vi->rangeLow,
                          vi->rangeHigh);
      }
      low = black - (white - black) / 2;
      high = low + 2 * (white - black);
      low = B3DMAX(0, low);
      high = B3DMIN(65536, high);
      if (low < high && high - low < vi->rangeHigh - vi->rangeLow) {
        vi->rangeLow = low;
        vi->rangeHigh = high;
        if (imodDebug('i'))
          imodPrintStderr("low high to stretch bw sliders %d %d\n", low, high);
      }
    }
    if (imodDebug('i'))
      imodPrintStderr("Setting %d %d\n", black, white);
    xcramp_setlevels(vi->cramp, black, white);
    floatSave = float_on;
    float_on = 0;
    imod_info_setbw(black, white);
    float_on = floatSave;
  }
}

// Return the mean and Sd of the current image, potentially a subarea
int imodInfoCurrentMeanSD(float &mean, float &sd, float &scaleLo, float &scaleHi)
{
  float sample;
  int ixStart, iyStart, nxUse, nyUse;
  unsigned char **image;
  ViewInfo *vi = App->cvi;
  float pctLo = 0.1f, pctHi = 0.1f;  // Take fixed limits for now

  // Get the limits to compute within, get images, get the mean & sd
  getSampleLimits(vi, ixStart, iyStart, nxUse, nyUse, sample, B3DNINT(vi->zmouse),
                  vi->ct);
  image = ivwGetZSectionTime(vi, B3DNINT(vi->zmouse), vi->ct);
  if (!image)
    return 1;
  if (sampleMeanSD(image, vi->ushortStore ? 2 : 0, vi->xsize, vi->ysize, sample,
                    ixStart, iyStart, nxUse, nyUse, &mean, &sd))
    return 1;

  scaleLo = mean - 3. * sd;
  scaleHi = mean * 3. * sd;
  if (vi->ushortStore) {
    percentileStretch(image, SLICE_MODE_USHORT, vi->xsize, vi->ysize, sample,
                      ixStart, iyStart, nxUse, nyUse, pctLo, pctHi, &scaleLo, &scaleHi);
    scaleLo = B3DMAX(0., scaleLo);
    scaleHi = B3DMIN(65535., scaleHi);
    if ((int)scaleLo >= (int)scaleHi - 1) {
      scaleLo = 0.5 * (scaleLo + scaleHi) - 1.;
      scaleLo = B3DMAX(0, B3DMIN(65533., scaleLo));
      scaleHi = scaleLo + 2.;
    }
  }
  /* imodPrintStderr("%d %d %d %d %.2f %.2f\n", ixStart, iyStart, nxUse,
     nyUse, mean, sd); */

  /* Adjust for compressed data in 8-bit CI mode */
  if (App->depth == 8)
    mean = (mean - vi->rampbase) * 256. / vi->rampsize;
  return 0;
}

// External calls to get and set the float flags from preferences and info init
void imodInfoSetFloatFlags(int inFloat, int inSubset)
{
  float_on = inFloat;
  float_subsets = inSubset;
}

void imodInfoGetFloatFlags(int &outFloat, int &outSubset)
{
  outFloat = float_on;
  outSubset = float_subsets;
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

// Initiate dumping of file system cache after each section is loaded
void imodStartAutoDumpCache()
{
  dumpCache = 1;
  startDump = 2;
}

void imod_imgcnt(const char *string)
{
  static int started = 0;
  static QTime timer;
  if (!started)
    timer.start();
  if (started < 3 || timer.elapsed() > 100 || string[0] == '\n' || 
      string[0] == 0x00) {

    // Callers in 3dmod should be putting their \r on the end, but the call
    // from mrcfiles has it on the front, so we need to add it to end
    // in this context
    wprint("%s%s", string, (string[0] == '\r' || string[0] == '\n') ?
           "\r" : "");
    imod_info_input();
    timer.restart();
    started++;
  }
  if (App->exiting)
    exit(0);

  // If dumping cache, do so after initial calls; get starting position for
  // next time; stop when empty string is passed
  if (dumpCache) {
    if (!startDump)
      ivwDumpFileSysCache(App->cvi->image);
    ivwGetFileStartPos(App->cvi->image);
    if (startDump)
      startDump--;
    if (string[0] == 0x00)
      dumpCache = 0;
  }     
}

/*

$Log$
Revision 4.44  2011/03/08 05:36:02  mast
turn off float for RGB images

Revision 4.43  2011/03/04 23:02:03  mast
Prevented autocontrast from producing wild or very narrow ranges, stabilized
the zoomdown autocontrast by running first with loaded data

Revision 4.42  2011/02/14 18:32:17  mast
Fixed floating after flipping volume (reset last_section so it's not out of range)

Revision 4.41  2011/02/12 22:19:07  mast
Removed debugging output

Revision 4.40  2011/02/12 04:59:59  mast
Made autocontrast measure actual contrast of filtered image when zoomed down

Revision 4.39  2010/04/19 23:52:37  mast
Wipe out loading lines better

Revision 4.38  2010/04/01 03:08:30  mast
Put out at leat 3 status strings to make sure one with \r goes out

Revision 4.37  2010/04/01 02:26:20  mast
Update contour copy dialog, adjust status strings for new wprint

Revision 4.36  2009/11/23 19:30:35  mast
Fixed problems with it trying to float with no image

Revision 4.35  2009/09/21 23:24:34  mast
Made it update image reading display every 100 ms

Revision 4.34  2009/02/25 05:36:43  mast
Function for turning off updating of any dialogs but info

Revision 4.33  2009/01/02 16:07:38  mast
Change to const char for Qt4 port

Revision 4.32  2008/12/01 15:38:52  mast
Fix float on startup to middle Z, and set current point index better

Revision 4.31  2008/11/07 05:31:22  mast
Prevented new black level from being > 255

Revision 4.30  2008/05/27 05:54:21  mast
Update image process window on xyz change

Revision 4.29  2008/05/23 19:23:32  xiongq
Use multithreads to compute isosurface. Move the calling of imodvIsosurfaceUpdate() from imod_info_cb.cpp to imod_display.cpp.

Revision 4.28  2008/04/29 18:10:40  xiongq
add isosurface dialog

Revision 4.27  2008/03/06 00:12:46  mast
Changes to allow settings of float and subarea checkboxes to be saved

Revision 4.26  2008/01/17 22:34:17  mast
Call plugins to update on model change

Revision 4.25  2007/07/08 16:04:49  mast
Used new hot slider function

Revision 4.24  2006/10/02 15:33:13  mast
Fixed for > 2 Gpixel image

Revision 4.23  2006/06/26 15:22:25  mast
Added b3dutil include for samplemeansd

Revision 4.22  2005/06/26 19:37:24  mast
Added fine-grain entries

Revision 4.21  2005/06/15 23:00:46  mast
Set reference black/white before instead of after draws so changes in
black/white do not get undone by bwfloat

Revision 4.20  2005/04/12 14:37:17  mast
Changes to allow float for changing subarea within a section

Revision 4.19  2005/03/23 18:49:24  mast
Saved floating b/w values as reference values before truncating to range to
retain state after going to a section with truncated range

Revision 4.18  2005/03/20 19:55:36  mast
Eliminating duplicate functions

Revision 4.17  2005/02/12 01:35:31  mast
Initialized clearedSd properly, prevented b/w values out of range in
float, and called bwfloat when float is turned on to get things going

Revision 4.16  2004/11/07 23:03:42  mast
Fixed saving of cleared float info by saving only once and saving b/w also

Revision 4.15  2004/11/01 23:24:23  mast
Clear selection list when updating window if current cont not on list

Revision 4.14  2004/10/27 20:38:05  mast
Changed calls to cache dumper to take image, not fp

Revision 4.13  2004/10/22 22:16:59  mast
Added logic to dump file system cache after each call to imod_imgcnt

Revision 4.12  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.11  2004/01/22 19:12:43  mast
changed from pressed() to clicked() or accomodated change to actionClicked

Revision 4.10  2003/12/30 06:46:06  mast
Made SD be at least 0.1 to prevent crash on floating to blank image,
and fixed initialization of float_subsets

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
