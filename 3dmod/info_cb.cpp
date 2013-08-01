/*
 *  info_cb.cpp -- Callback functions for the Information Window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <math.h>
#include <qapplication.h>
#include <QTime>
#include "b3dutil.h"
#include "object_edit.h"
#include "form_info.h"

#include "info_setup.h"
#include "imod.h"
#include "display.h"
#include "imod_edit.h"
#include "imod_input.h"
#include "cont_edit.h"
#include "cont_copy.h"
#include "imod_io.h"
#include "imodplug.h"
#include "rescale.h"
#include "info_cb.h"
#include "pyramidcache.h"
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

// A structure to store means and SD's of areas
typedef struct {
  float mean;
  float sd;
  int ixStart;
  int iyStart;
  int nxUse;
  int nyUse;
  int cacheSum;
} MeanSDData;

/* Static variables for keeping track of floating */
static float sRefBlack = 0.;
static float sRefWhite = 255.;
static int sLastSection = -1;
static int sLastTime = 0;
static MeanSDData *sSecData = NULL;
static int sTableSize = 0;
static int sTdim = 0;
static int sSingleCleared = 0;
static int sSaveNextClear = 0;
static int sClearedSection, sClearedTime;
static float sClearedMean, sClearedSD = -1., sClearedBlack, sClearedWhite;

static int sCtrlPressed = 0;
static int sImodObjCnum = -1;
static int sFloatOn = 0;
static int sDoingFloat = 0;
static int sFloatSubsets = 0;
static int sLastSubsets = 0;
static int sLastReverse = -1;
static float sFloatMatt = 0.05f;

static int sDumpCache = 0;
static int sStartDump = 0;
static int sUpdateInfoOnly = 0;

/*
 * FUNCTIONS FOR THE CONTROLS TO REPORT CHANGES
 *
 * New object, contour, or point
 */
void imodInfoNewOCP(int which, int value, int noShow)
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
    // Object change.  If show button is off, detach from contour/point
    if (noShow) {
      imod->cindex.object = value;
      imod->cindex.contour =  - 1;
      imod->cindex.point =  - 1;
    } else {
      imodSetIndex(imod, value, co, pt);
      inputKeepContourAtSameTime(App->cvi);
    }
    break;

  case 1:
    // Contour change.  Stay attached regardless of show button (9/11/11) but draw here
    imodSetIndex(imod, ob, value, pt);
    inputRestorePointIndex(App->cvi, &indOld);
    break;

  case 2:
    // Point change
    imodSetIndex(imod, ob, co, value);
    break;
  }
  
  if (noShow && imod->cindex.point > 0) {
    imodDraw(App->cvi, IMOD_DRAW_ALL | IMOD_DRAW_NOSYNC);
    return;
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
  int float_save = sFloatOn;

  // THIS IS REALLY SCARY UNLESS WE ASSERT THE SLIDERS BACK
  if (ImodForbidLevel) {
    ImodInfoWidget->setBWSliders(vi->blackInRange, vi->whiteInRange);
    return;
  }

  // Exit if RGBA and there is not a hot slider active
  if (App->rgba && dragging && !ImodPrefs->hotSliderActive(sCtrlPressed))
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
  sFloatOn = FALSE;
  imod_info_setbw(black, white);
  sFloatOn = float_save;
}

void imodInfoNewLH(int which, int value, int dragging)
{
  int low, high;
  ImodView *vi = App->cvi;
  int float_save = sFloatOn;

  // Exit if there is not a hot slider active
  if (dragging && !ImodPrefs->hotSliderActive(sCtrlPressed))
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
  sFloatOn = FALSE;
  imod_info_setbw(vi->black, vi->white);
  sFloatOn = float_save;
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
    sFloatOn = 1;
    imod_info_bwfloat(App->cvi, (int)(App->cvi->zmouse + 0.5f), App->cvi->curTime);
    imod_info_setbw(App->cvi->black, App->cvi->white);
  } else
    sFloatOn = 0;
}

void imodInfoSubset(int state)
{
  sFloatSubsets = state;
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
  sCtrlPressed = pressed;
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
  bool meshOnly;
     
  obj = imodObjectGet(imod);
  cont = imodContourGet(imod);
     
  // Object is either present or not
  max[0] = imod->objsize;
  val[0] = obj ? imod->cindex.object + 1 : 0;
  if (!imod->objsize)
    sImodObjCnum = -1;

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
  if ((sImodObjCnum != imod->cindex.object)
      && (imod->cindex.object != -1)){

    sImodObjCnum = imod->cindex.object;
    imod_info_setobjcolor();

  }

  // Clear the selection list if no current contour and not a mesh-only object,
  // or it is not on list
  meshOnly = sImodObjCnum >= 0 && imod->obj[sImodObjCnum].meshsize && 
    !imod->obj[sImodObjCnum].contsize;
  if ((!meshOnly && !cont) || imodSelectionListQuery(App->cvi, imod->cindex.object,
                                     imod->cindex.contour) < -1)
    imodSelectionListClear(App->cvi);

  // Update dialog boxes if they are open and not engaged in continuous draw
  if (sUpdateInfoOnly)
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
  sUpdateInfoOnly = value;
}

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
  sRefBlack = black;
  sRefWhite = white;

  /* if we are using a colormap that isn't
   * mutable then we need to redraw all image data, unless float is being done
   * use the IMOD_DRAW_IMAGE flag to redraw all image
   * data and clear all image caches, and IMOD_DRAW_NOSYNC to prevent
   * panning the zap window to the current model point
   */
  if (changed && App->rgba && !sDoingFloat)
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
  int llX, leftXpad, rightXpad, llY, leftYpad, rightYpad, llZ, leftZpad, rightZpad;

  if (!sFloatSubsets || zapSubsetLimits(vi, ixStart, iyStart, nxUse, nyUse)) {
    ixStart = (int)(sFloatMatt * vi->xsize);
    nxUse = vi->xsize - 2 * ixStart;
    iyStart = (int)(sFloatMatt * vi->ysize);
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
  int ixStart, iyStart, nxUse, nyUse, jxStart, jyStart, mxUse, myUse, cacheSum;
  float sloperatio, tmp_black, tmp_white, refMean, refSD, newdiff;
  unsigned char **image;
  bool needMean;
  int retval = 0;
  int sampleType = vi->ushortStore ? 2 : 0;
  int rangeMax = vi->ushortStore ? 65535 : 255;
  int cacheInd = vi->pyrCache ? vi->pyrCache->getBufCacheInd() : -1;

  // Skip through if this is the first call; there is no reference
  if (sFloatOn && sLastSection >= 0 && !vi->fakeImage && !vi->rgbStore) {

    /* Make sure table exists and is the right size */
    sTdim = ivwGetMaxTime(vi) + 1;
    needsize = vi->zsize * sTdim;
    if (sTableSize == 0)
      sSecData = B3DMALLOC(MeanSDData, needsize);
    else if (sTableSize != needsize)
      B3DREALLOC(sSecData, MeanSDData, needsize);

    if (!sSecData) {
      imod_info_float_clear(-1, -1);
      return 0;
    }
	  
    /* Clear out any new entries */
    if (sTableSize < needsize)
      for (i = sTableSize; i < needsize; i++)
        sSecData[i].mean = sSecData[i].sd = sSecData[i].cacheSum = -1;
    sTableSize = needsize;

    if ((sLastSection + 1) * (sLastTime + 1) > sTableSize ||
        (section + 1) * (time + 1) > sTableSize)
      return 0;

    if (time > 0 && sLastTime == 0)
      sLastTime = 1;

    // Get generic limits for sampling and if they do not match the last limits
    // then set up to recompute the mean/sd
    getSampleLimits(vi, ixStart, iyStart, nxUse, nyUse, sample, 0, -1);
    iref = sTdim * sLastSection + sLastTime;
    isec = sTdim * section + time;

    /* Get information about reference and current sections if necessary;
     i.e. if there is no SD already or if area doesn't match*/
    err1 = 0;
    if (imodDebug('i'))
      imodPrintStderr("sc %d iref %d isec %d lastsec %d clearedsec %d sd %f\n",
                      sSingleCleared, iref, isec, sLastSection, sClearedSection, 
                      sSecData[iref].sd);
    if (sSingleCleared && iref == isec && sLastSection == sClearedSection && 
        sLastTime == sClearedTime && sSecData[iref].sd < 0.) {

      // If this is a section that was cleared, use the saved values
      refMean = sClearedMean;
      refSD = sClearedSD;
      sRefBlack = sClearedBlack;
      sRefWhite = sClearedWhite;

    } else {

      // Get new data now if there is no valid data for the reference section
      // or if the area has changed, but only if something else has changed,
      // namely section, time, or subset versus non-subset
      needMean = sSecData[iref].sd < 0. ||
          ((ixStart != sSecData[iref].ixStart || iyStart != sSecData[iref].iyStart || 
            nxUse != sSecData[iref].nxUse || nyUse != sSecData[iref].nyUse) &&
           (section != sLastSection || time != sLastTime || 
            sFloatSubsets != sLastSubsets));
      if (needMean) {
        getSampleLimits(vi, jxStart, jyStart, mxUse, myUse, sample, sLastSection, 
                        sLastTime);

        // Get data from cache tiles or from Z slice
        if (cacheInd >= 0) {
          err1 = vi->pyrCache->loadedMeanSD(cacheInd, sLastSection, sample,
                                            jxStart, jyStart, mxUse, myUse, 
                                            &sSecData[iref].mean, &sSecData[iref].sd,
                                            cacheSum, -1);
          if (!err1)
            sSecData[iref].cacheSum = cacheSum;
        } else {
          image = ivwGetZSectionTime(vi, sLastSection, sLastTime);
          err1 = sampleMeanSD(image, sampleType, vi->xsize, vi->ysize, sample,
                              jxStart, jyStart, mxUse, myUse, 
                              &sSecData[iref].mean, &sSecData[iref].sd);
        }
        if (!err1 && sSecData[iref].sd < 0.1)
          sSecData[iref].sd = 0.1;
        
        /* Adjust for compressed data in 8-bit CI mode */
        if (!err1 && App->depth == 8)
          sSecData[iref].mean = (sSecData[iref].mean - vi->rampbase) * 256. /
            vi->rampsize;
      }

      // Otherwise, take existing data as the reference - allows change for new subset 
      // within a section to be tracked, and also allows change as additional tiles are
      // loaded
      refMean = sSecData[iref].mean;
      refSD = sSecData[iref].sd;
    }
	       
    // Need a new mean from current section if there was not one before, or if the
    // subarea changed; but also may need new mean if more tiles have been loaded
    needMean = sSecData[isec].sd < 0. || ixStart != sSecData[isec].ixStart || 
                iyStart != sSecData[isec].iyStart || 
                nxUse != sSecData[isec].nxUse || nyUse != sSecData[isec].nyUse;
    if (!err1 && (needMean || (cacheInd >= 0 && sSecData[isec].cacheSum > 0))) {
      getSampleLimits(vi, jxStart, jyStart, mxUse, myUse, sample, section, time);
      
      // Get mean from loaded tiles; if we needed one anyway, be sure to get it by setting
      // old cache sum to -1; otherwise send it old cache sum and it can skip computing
      // if it matches
      if (cacheInd >= 0) {
        err1 = vi->pyrCache->loadedMeanSD(cacheInd, section, sample,
                                          jxStart, jyStart, mxUse, myUse, 
                                          &sSecData[isec].mean, &sSecData[isec].sd,
                                          cacheSum, 
                                          needMean ? -1 : sSecData[isec].cacheSum);

        // If this succeeded, save the cache sum; if there was nothing there, just return
        // to keep the existing reference section
        if (!err1)
          sSecData[isec].cacheSum = cacheSum;
        else
          return 0;

        // Or get data from Z slice
      } else {
        image = ivwGetZSectionTime(vi, section, time);
        err1 = sampleMeanSD(image, sampleType, vi->xsize, vi->ysize, sample,
                            jxStart, jyStart, mxUse, myUse, 
                            &sSecData[isec].mean, &sSecData[isec].sd);
      }
      if (!err1 && sSecData[isec].sd < 0.1)
        sSecData[isec].sd = 0.1;
      if (!err1 && App->depth == 8)
        sSecData[isec].mean = (sSecData[isec].mean - vi->rampbase) * 256. / vi->rampsize;
    }
	       
    // If all that succeeded, save the analysis area for reference and section and proceed
    if (!err1) {
      sSecData[iref].ixStart = sSecData[isec].ixStart = ixStart;
      sSecData[iref].nxUse = sSecData[isec].nxUse = nxUse;
      sSecData[iref].iyStart = sSecData[isec].iyStart = iyStart;
      sSecData[iref].nyUse = sSecData[isec].nyUse = nyUse;

      if (imodDebug('i'))
        imodPrintStderr("ref %.2f %.2f  sec %.2f %.2f\n", refMean,
                        refSD, sSecData[isec].mean, sSecData[isec].sd);

      /* Compute new black and white sliders; keep floating values */
      // If contrast reversal is unknown or the same, use basic formula for matching
      // map directions
      sloperatio = sSecData[isec].sd / refSD;
      newdiff = sloperatio * (sRefWhite - sRefBlack);
      if (sLastReverse < 0 || sLastReverse == vi->cramp->reverse) {
        tmp_black = sSecData[isec].mean - (refMean - sRefBlack) * sloperatio;
        tmp_white = tmp_black + newdiff;
      } else if (sLastReverse) {

        // If last time was reversed and this is not, base change on different mappings
        tmp_black = sSecData[isec].mean - (sRefWhite - refMean) * sloperatio;
        tmp_white = tmp_black + newdiff;
      } else {

        // Or if this is reversed and last was not...
        tmp_white = sSecData[isec].mean + (refMean - sRefBlack) * sloperatio;
        tmp_black = tmp_white - newdiff;
      }
      if (imodDebug('i'))
        imodPrintStderr("ref_bw %.2f %.2f  tmp_bw %.2f %.2f\n", sRefBlack,
                        sRefWhite, tmp_black, tmp_white);

      /* DNM 3/23/05: needed to truncate only new values, not the tmp_ values
         that will be used to keep track of consistent contrast setting */
      newblack = B3DNINT(tmp_black);
      newwhite = B3DNINT(tmp_white);
      newblack = B3DMAX(0, B3DMIN(newblack, rangeMax));
      newwhite = B3DMAX(newblack, B3DMIN(newwhite, rangeMax));

      // Back off the sliders to prevent all white/black images
      if (newwhite < newblack + 2) {
        newblack = B3DMAX(0, newblack - 1);
        newwhite = B3DMIN(newwhite + 1, rangeMax);
      }
      if (imodDebug('i')) {
        int meanmap = (int)(rangeMax * (sSecData[isec].mean - newblack) / 
                            B3DMAX(1, newwhite - newblack));
        float sdmap = rangeMax * (sSecData[isec].sd) / B3DMAX(1,newwhite - newblack);
        imodPrintStderr("mean = %d  sd = %.2f\n", meanmap, sdmap);
      }

      // Set the sliders and the ramp if the integer values changed
      if (newwhite != vi->white || newblack != vi->black) {
        vi->black = newblack;
        vi->white = newwhite;
        xcramp_setlevels(vi->cramp, vi->black, vi->white);
        sDoingFloat = 1;
        imod_info_setbw(vi->black, vi->white);
        sDoingFloat = 0;
        retval = 1;
      }

      // Keep track of things for next time
      sRefBlack = tmp_black;
      sRefWhite = tmp_white;
      sLastSubsets = sFloatSubsets;
      sLastReverse = vi->cramp->reverse;
    }
  }

  sLastSection = section;
  sLastTime = time;
  return retval;
}

/* Prepare to save the next float clear call */
void imodInfoSaveNextClear()
{
  sSaveNextClear = 1;
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
  int index = section * sTdim + time;
  sSingleCleared = 0;
  if (section < 0 && time < 0) {
    if (sSecData)
      free(sSecData);
    sSecData = NULL;
    sTableSize = 0;
    sLastSection = -1;
  } else if (section < 0) {
    /* DNM 11/14/03: fix to test actual highest index */
    if ((-section - 1) * sTdim + time >= sTableSize)
      return;
    for (i = 0; i < -section; i++) {
      sSecData[i * sTdim + time].mean = -1;
      sSecData[i * sTdim + time].sd = -1;
    }
  } else if (index < sTableSize) {

    // If a single section is being cleared, keep track of its values if flag
    // set
    if (sSaveNextClear) {
      if (imodDebug('i'))
        imodPuts("saving clear");
      sClearedSection = section;
      sClearedTime = time;
      sClearedMean = sSecData[index].mean;
      sClearedSD = sSecData[index].sd;
      sClearedBlack = sRefBlack;
      sClearedWhite = sRefWhite;
      sSaveNextClear = 0;
    }
    sSecData[index].mean = -1;
    sSecData[index].sd = -1;
    if (sClearedSD >= 0.)
      sSingleCleared = 1;
  }
  if (imodDebug('i'))
    imodPrintStderr("clear called, sc = %d\n",sSingleCleared);
  return;
}

/*
 * Change the contrast to meet the target mean and SD
 * NOTE: When contrast is normal, the mapping is
 * displayValue = (byteValue - black) * 255 / (white - black)
 * When contrast is reversed this changes to
 * displayValue = (white - byteValue) * 255 / (white - black)
 * Equations for changes here and in floating derive from these relations
 * Here are some more equations from the last time this was scribbled on paper:
 * A scaling is defined by F * I + A and by black and white values B and W that map to 
 * 0 and 255 in mormal contrast or 255 and 0 in inverted contrast
 * Normal contrast given F, A:    B = -A / F           W = (255 - A) / F
 * Normal contrast given B, W:    F = 255 / (W - B)    A = -255 * B / (W - B)
 * Reverse contrast given F, A:   W = -A / F           B = (255 - A) / F
 * Reverse contrast given B, W:   F = -255 / (W - B)   A = 255 * W / (W - B)
 * Product of two scalings:  F' = F1 * F2    A' = F2 * A1 + A2
 * If I have data at mean M, SD S and I want to bring it to a target Mtarg, Starg:
 *    F = Starg / S     A = Mtarg - M * Starg / S
 */
void imodInfoAutoContrast(int targetMean, int targetSD)
{
  float mean, sd, meanSub, sdSub, meanRGB, sdRGB, scaleLo, scaleHi, temp;
  int black, white, floatSave, loop, nloop, low, high, uzXstart, uzYstart, uzXuse, uzYuse;
  B3dCIImage *image = NULL;
  float sample, wbdiff, ftargMean = targetMean, ftargSD = targetSD;
  int ixStart, iyStart, nxUse, nyUse, nxim, nyim, ierr;
  unsigned char **lines = NULL;
  ImodView *vi = App->cvi;
  int rampMax = vi->ushortStore ? 65535 : 255;
  int izsec = B3DNINT(vi->zmouse);
  ZapFuncs *zap = getTopZapWindow(false);
  nloop = 1;

  if (zap && !vi->pyrCache)
    image = zap->zoomedDownImage(sFloatSubsets, nxim, nyim, ixStart, iyStart,
                                 nxUse, nyUse, uzXstart, uzYstart, uzXuse, uzYuse);

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
                          &meanRGB, &sdRGB);
      imodTrace('a', "RGB: %d %d %d %d %d %f %.2f %.2f", nxim, ixStart, iyStart, nxUse, 
                nyUse, sample, meanRGB, sdRGB);
      free(lines);
      if (ierr)
        return;

      // Get mean of corresponding unzoomed image region and adjust the target mean/SD by
      // to what this subarea was mapped to by current B/W values
      if (uzXuse * uzYuse > 32) {
        sample = 10000.0/(((double)uzXuse) * uzYuse);
        if (sample > 1.0)
          sample = 1.0;
        lines = ivwGetZSectionTime(vi, izsec, vi->curTime);
        if (!sampleMeanSD(lines, vi->ushortStore ? 2 : 0, vi->xsize, vi->ysize, sample,
                          uzXstart, uzYstart, uzXuse, uzYuse, &meanSub, &sdSub)) {
          /*
           * Change the target to whatever the subarea was mapped to by current B and W
           */
          ftargSD = sdSub * 255. / (vi->white - vi->black);
          if (vi->cramp->reverse)
            ftargMean = (vi->white - meanSub) * 255. / (vi->white - vi->black);
          else
            ftargMean = (meanSub - vi->black) * 255. / (vi->white - vi->black);
          /* Alternate method: assume full area does map to Starg, Mtarg, this implies
             a scaling that maps the subarea in this way:
             ftargMean += (vi->cramp->reverse ? -1 : 1) * (meanSub - mean) * ftargSD / sd;
            ftargSD *= sdSub / sd; */
          imodTrace('a', "unzoomed: %d %d %d %d subarea %.2f %.2f  adjusted %.2f %.2f",
                    uzXstart, uzYstart, uzXuse, uzYuse, meanSub, sdSub, ftargMean, 
                    ftargSD);
        }
      }

      /* To determine the B, W needed to bring these data to target, need the product of
       * existing B, W scaling and scaling to get from Srgb, Mrgb to Starg, Mtarg:
       * F' = (Starg / Srgb) * 255 / (W - B)
       * A' = (Starg / Srgb) * 255 * B / (W - B) + Mtarg - Mrgb * Starg / Srgb
       * W' - B' = 255 / F' = (W - B) * Srgb / Starg
       * B' = -A' / F' = B = Mtarg * (Srgb / Starg) * (W - B) / 255 - Mrgb * (W - B) / 255
       * and similarly for reverse...
       */
      wbdiff = (vi->white - vi->black) * sdRGB / ftargSD;
      if (wbdiff) {
        if (vi->cramp->reverse) {
          temp = vi->white - ((vi->white - vi->black) * meanRGB -
                              wbdiff * ftargMean) / 255.;
          white = B3DNINT(temp);
          black = B3DNINT(temp - wbdiff);
        } else {
          temp = vi->black + ((vi->white - vi->black) * meanRGB -
                              wbdiff * ftargMean) / 255.;
          black = B3DNINT(temp);
          white = B3DNINT(temp + wbdiff);
        }
      } else {
        black = white = B3DNINT(meanRGB);
      }
    } else {

      // Otherwise get the mean of the current image
      if (imodInfoCurrentMeanSD(mean, sd, scaleLo, scaleHi))
        return;
      if (vi->cramp->reverse) {;
        white = B3DNINT(mean + sd * targetMean / targetSD);
        black = B3DNINT(mean - sd * (255 - targetMean) / targetSD);
      } else {
        black = B3DNINT(mean - sd * targetMean / targetSD);
        white = B3DNINT(mean + sd * (255 - targetMean) / targetSD);
      }
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
    floatSave = sFloatOn;
    sFloatOn = 0;
    imod_info_setbw(black, white);
    sFloatOn = floatSave;
  }
}

// Return the mean and Sd of the current image, potentially a subarea
int imodInfoCurrentMeanSD(float &mean, float &sd, float &scaleLo, float &scaleHi)
{
  float sample;
  int ixStart, iyStart, nxUse, nyUse, cacheSum;
  unsigned char **image;
  ViewInfo *vi = App->cvi;
  float pctLo = 0.1f, pctHi = 0.1f;  // Take fixed limits for now
  int izsec = B3DNINT(vi->zmouse);
  float *scaleLoPtr = NULL;

  // Get the limits to compute within, get images, get the mean & sd
  getSampleLimits(vi, ixStart, iyStart, nxUse, nyUse, sample, izsec, vi->curTime);
  if (vi->pyrCache) {
    if (vi->ushortStore)
      scaleLoPtr = &scaleLo;
    if (vi->pyrCache->loadedMeanSD(vi->pyrCache->getBufCacheInd(), izsec, sample, ixStart,
                                   iyStart, nxUse, nyUse, &mean, &sd, cacheSum, -1,
                                   pctLo, pctHi, scaleLoPtr, &scaleHi))
      return 1;
  } else {
    image = ivwGetZSectionTime(vi, izsec, vi->curTime);
    if (sampleMeanSD(image, vi->ushortStore ? 2 : 0, vi->xsize, vi->ysize, sample,
                     ixStart, iyStart, nxUse, nyUse, &mean, &sd))
      return 1;
  }

  scaleLo = mean - 3. * sd;
  scaleHi = mean * 3. * sd;
  if (vi->ushortStore) {
    if (!vi->pyrCache)
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
  imodTrace('a', "imodInfoCurrentMeanSD: %d %d %d %d %.2f %.2f", ixStart, iyStart,
                  nxUse, nyUse, mean, sd); 

  /* Adjust for compressed data in 8-bit CI mode */
  if (App->depth == 8)
    mean = (mean - vi->rampbase) * 256. / vi->rampsize;
  return 0;
}

// External calls to get and set the float flags from preferences and info init
void imodInfoSetFloatFlags(int inFloat, int inSubset)
{
  sFloatOn = inFloat;
  sFloatSubsets = inSubset;
}

void imodInfoGetFloatFlags(int &outFloat, int &outSubset)
{
  outFloat = sFloatOn;
  outSubset = sFloatSubsets;
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

void show_status(const char *info)
{
  if (!info)
    return;

  imod_info_msg(info, " ");
  return;
}

// Unused
void imod_show_info(const char *info, int line)
{
  if (!info)
    return;
     
  if (line == 1)
    imod_info_msg(info, NULL);
  if (line == 2)
    imod_info_msg(NULL, info);
  return;
}

void imod_info_msg(const char *top, const char *bot)
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
  sDumpCache = 1;
  sStartDump = 2;
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
  if (sDumpCache) {
    if (!sStartDump)
      ivwDumpFileSysCache(App->cvi->image);
    ivwGetFileStartPos(App->cvi->image);
    if (sStartDump)
      sStartDump--;
    if (string[0] == 0x00)
      sDumpCache = 0;
  }     
}
