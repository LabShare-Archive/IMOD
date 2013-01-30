/*
 *  autox.cpp --  Automatic contour generation for imod.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include "form_autox.h"
#include "imod.h"
#include "display.h"
#include "dia_qtutils.h"
#include "xcramp.h"
#include "autox.h"
#include "iproc.h"
#include "info_cb.h"
#include "pyramidcache.h"
#include "control.h"
#include "undoredo.h"

/* Local functions */
static int autox_flood(Autox *ax);
static void autox_clear(Autox *ax, unsigned char bit);
static int allocate_arrays(ImodView *vw, Autox *ax);
static void continueNext(void);

/* The current data that is being contoured. */
static unsigned char **autoImage = NULL;
static AutoxWindow *autoWindow = NULL;

/*
 * FUNCTIONS CALLED BY THE FORM CLASS
 *
 * Put up a help dialog for the auto contour window.
 */
void autoxHelp()
{
  imodShowHelpPage("autox.html#TOP");
}

void autoxSlider(int which, int value)
{
  Autox *ax = App->cvi->ax;

  if (which)
    ax->shave = value * 0.01;
  else {
    ax->threshold = value;
    if (ax->contrast)
      autoxContrastSelected(1);
  }
}

void autoxContrastSelected(int which)
{
  Autox *ax = App->cvi->ax;
     
  if (ax->contrast != which)
    xcrampSelectIndex(ax->vw->cramp, which);
  ax->contrast = which;
  if (which) {
    if (ax->vw->ushortStore) {
      ax->vw->black = imodInfoSliderToLevel(ax->vw, ax->threshold);
      ax->vw->white = imodInfoSliderToLevel(ax->vw, ax->threshold + 1);
    } else {
      ax->vw->black = ax->threshold;
      ax->vw->white = ax->threshold + 1;
    }
    xcramp_setlevels(ax->vw->cramp, ax->vw->black, ax->vw->white);
  } else {
    xcramp_ramp(ax->vw->cramp);
    xcramp_getlevels(ax->vw->cramp, &(ax->vw->black), &(ax->vw->white));
  }

  imod_info_setbw(ax->vw->black, ax->vw->white);
}

void autoxAltmouse(int state)
{
  App->cvi->ax->altmouse = state;
}

void autoxFollowDiagonals(int state)
{
  App->cvi->ax->diagonal = state;
}

void autoxClosing()
{
  Autox  *ax = App->cvi->ax;

  xcrampSelectIndex(ax->vw->cramp, 0);
  xcramp_ramp(ax->vw->cramp);
  xcramp_getlevels(ax->vw->cramp, &(ax->vw->black), &(ax->vw->white));

  ax->vw->imod->thresh = ax->threshold;
  autox_clear(ax, AUTOX_ALL);
  imod_info_setbw(ax->vw->black, ax->vw->white);

  B3DFREE(ax->xlist);
  B3DFREE(ax->ylist);
  if (ax->vw->pyrCache)
    ax->vw->pyrCache->freeFullSection();

  imodDraw(ax->vw, IMOD_DRAW_IMAGE);
  B3DFREE(ax->data);
  B3DFREE(ax->byteSlice);
  free(ax);
  App->cvi->ax = NULL;
  imodDialogManager.remove((QWidget *)autoWindow);
  autoWindow = NULL;
}


void autoxFill()
{
  ImodView *vw = App->cvi;

  if (vw->ax->cz == vw->zmouse)
    autox_clear(vw->ax, AUTOX_FILL);
  else
    autox_clear(vw->ax, AUTOX_ALL);
  autox_flood(vw->ax);
  imodAutoPatch(vw->ax->data, vw->ax->xlist, vw->ax->ylist, vw->ax->listsize, vw->xsize,
                vw->ysize);
  vw->ax->filled = TRUE;
  vw->ax->cz = (int)(vw->zmouse + 0.5);
  imodDraw(vw, IMOD_DRAW_IMAGE);
}

/* DNM 1/25/01: recoded to use new function that finds and returns array of
   contours from the defined points */
void autoxBuild()
{
  ImodView *vw = App->cvi;
  Icont *cont;
  Icont *newconts;
  Iobj *obj = imodObjectGet(vw->imod);
  int ncont, i, surf;

  if (!vw->ax->filled || !obj)
    return;

  /* Refresh the line pointers */
  if (vw->ushortStore)
    autoImage = ivwMakeLinePointers(vw, vw->ax->byteSlice, vw->xsize, vw->ysize, 0);
  else if (vw->pyrCache)
    autoImage = vw->pyrCache->getFullSection(vw->ax->cz);
  else
    autoImage = ivwGetZSection(vw, vw->ax->cz);

  /* First have to turn off any pixels that are marked as black */
  for(i = 0; i < vw->xysize; i++)
    if (vw->ax->data[i] & AUTOX_BLACK)
      vw->ax->data[i] &= ~AUTOX_FLOOD;

  /*  get contours from the edges */
  newconts = imodContoursFromImagePoints
    (vw->ax->data, autoImage, vw->xsize, vw->ysize, vw->ax->cz, 
     AUTOX_FLOOD | AUTOX_WHITE, vw->ax->diagonal, 
     vw->ax->threshUsed, vw->ax->reverse, &ncont);

  /* DNM: return if no points found */
  if (!newconts)
    return;

  /* add each contour to the model */
  for (i = 0; i < ncont; i++) {

    // If there is no current contour, set to last one; get current surface
    surf = 0;
    if (vw->imod->cindex.contour < 0)
      vw->imod->cindex.contour = obj->contsize - 1;
    cont = imodContourGet(vw->imod);
    if (cont)
      surf = cont->surf;

    // If contour is non-empty or nonexistent, get a new one
    if (!cont || cont->psize) {
      vw->undo->contourAddition(obj->contsize);
      imodNewContour( vw->imod );
      cont = imodContourGet(vw->imod);
      if (!cont) {
        vw->undo->flushUnit();
        break;
      }
    } else {
      vw->undo->contourDataChg();
    }

    // Copy structure, set surface and fix time setting
    imodContourCopy(&newconts[i], cont);
    cont->surf = surf;
    ivwSetNewContourTime(vw, obj, cont);

    /* DNM: switch to ContourReduce method, but keep the Strip call as a
       quick pre-filter - it eliminates points ON the lines */
    imodContourStrip(cont);
    if (vw->ax->shave > 0.0) 
      imodContourReduce(cont, vw->ax->shave);  
  }
  free (newconts);

  /* DNM: better make the clear unconditional, or it will keep adding
     the same points over and over */
  autox_clear(vw->ax, AUTOX_ALL);
  vw->ax->filled = 0;
  vw->undo->finishUnit();
  imodDraw(vw, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
}

void autoxClear()
{
  ImodView *vw = App->cvi;
  autox_clear(vw->ax, AUTOX_ALL);
  imodDraw(vw, IMOD_DRAW_IMAGE);
}  

void autoxShrink()
{
  ImodView *vw = App->cvi;
  imodAutoShrink(vw->ax->data, vw->xsize, vw->ysize);
  vw->ax->threshUsed = -1;
  imodDraw(vw, IMOD_DRAW_IMAGE);
}

void autoxExpand()
{
  ImodView *vw = App->cvi;
  imodAutoExpand(vw->ax->data, vw->xsize, vw->ysize);
  imodAutoPatch(vw->ax->data, vw->ax->xlist, vw->ax->ylist, vw->ax->listsize, vw->xsize, 
                vw->ysize);
  vw->ax->threshUsed = -1;
  imodDraw(vw, IMOD_DRAW_IMAGE);
}

void autoxSmooth()
{
  ImodView *vw = App->cvi;
  imodAutoExpand(vw->ax->data, vw->xsize, vw->ysize);
  imodAutoShrink(vw->ax->data, vw->xsize, vw->ysize);
  imodAutoPatch(vw->ax->data, vw->ax->xlist, vw->ax->ylist, vw->ax->listsize, vw->xsize,
                vw->ysize);
  vw->ax->threshUsed = -1;
  imodDraw(vw, IMOD_DRAW_IMAGE);
}

/* Go to next section. */
void autoxNext()
{
  ImodView *vw = App->cvi;
  if (iprocBusy()) {
    wprint("\a'Next' operation aborted because image processing is busy.\n");
    return;
  }

  if (!vw->insertmode){
    if (vw->zmouse < (vw->zsize - 1)){
      vw->zmouse++;
    }
  }else{
    if (vw->zmouse > 0){
      vw->zmouse--;
    }
  }
  iprocUpdate();
  iprocCallWhenFree(continueNext);
}

static void continueNext(void)
{
  ImodView *vw = App->cvi;
  int i, j;
  int mi = vw->xsize;
  int mj = vw->ysize;
  float ci = 0.0f, cj = 0.0f, cp = 0.0f;
  unsigned char *data = vw->ax->data;
  
  /* Find the new X and Y coords. */
  for(j = 0; j < mj; j++)
    for(i = 0; i < mi; i++){
      if (data[i + (j*mi)] & AUTOX_FILL){
        ci += i;
        cj += j;
        cp += 1.0f;
      }
    }
  if (cp){
    vw->xmouse = ci/cp;
    vw->ymouse = cj/cp;
  }else{
    Ipoint cm;
    Icont *cont = imodContourGet(vw->imod);
    if (cont){
      imodContourCenterOfMass(cont, &cm);
      vw->xmouse = cm.x;
      vw->ymouse = cm.y;
    }
  }

  autoxFill();
  imodDraw(vw, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
}

/* Allocate the array given the current image size */
static int allocate_arrays(ImodView *vw, Autox *ax)
{
  if (vw->ushortStore) {
    ax->byteSlice = (unsigned char *)malloc(vw->xysize);
    if (!ax->byteSlice)
      return -1;
  }

  /* data used to store segmentation data. */
  ax->data = (unsigned char *)malloc(vw->xysize);
  ax->xysize = vw->xysize;

  /* list of pixels to check: make it twice perimeter size */
  ax->listsize = 4 * (vw->xsize + vw->ysize);
  ax->xlist = (int *)malloc(ax->listsize * sizeof(int));
  ax->ylist = (int *)malloc(ax->listsize * sizeof(int));
  if (!ax->data || !ax->xlist || !ax->ylist){
    B3DFREE(ax->data);
    B3DFREE(ax->xlist);
    B3DFREE(ax->ylist);
    B3DFREE(ax->byteSlice);
    return(-1);
  }
  return (0);
}

/*
 * ROUTINES CALLED FROM ELSEWHERE
 */

/* If image is flipped, call this - it dismisses existing memory and gets new
   memory; quits window if failed */
void autox_newsize(ImodView *vw)
{
  Autox *ax = vw->ax;

  if (!ax)
    return;
  B3DFREE(ax->data);
  B3DFREE(ax->xlist);
  B3DFREE(ax->ylist);
  B3DFREE(ax->byteSlice);
  if (allocate_arrays(vw, ax)) {
    ax->data = NULL;
    ax->xlist = NULL;
    ax->ylist = NULL;
    wprint("\aAutoContour failed to get new image memory.");
    autoWindow->close();
  } else
    autox_clear(ax, AUTOX_ALL);
  ax->filled    = 0;
}


int autox_open(ImodView *vw)
{
  Autox *ax = vw->ax;

  if (vw->fakeImage) {
    wprint("\aAutoContour cannot be used with no image.");
    return(-1);
  }

  if (ax && autoWindow){
    autoWindow->raise();
    return(-1);
  }

  ax = (Autox *)malloc(sizeof(Autox));
  if (!ax){
    wprint("AutoContour Open failed: No memory.");
    return(-1);
  }

  ax->threshold = vw->imod->thresh;
  ax->contrast  = 0;
  ax->reverse   = 0;
  ax->shave     = 0.25;
  ax->vw        = vw;
  ax->filled    = 0;
  ax->altmouse  = 0;
  ax->cz        = (int)(vw->zmouse + 0.5);
  ax->diagonal  = 0;
  ax->byteSlice = NULL;

  autoWindow = new AutoxWindow(imodDialogManager.parent(IMOD_DIALOG),
                               Qt::Window);
                               
  if (!autoWindow){
    free(ax);
    wprint("AutoContour Open failed: No window available.");
    return(-1);
  }

  if (allocate_arrays(vw, ax)) {
    delete autoWindow;
    free(ax);
    wprint("AutoContour Open failed: No memory.");
    return(-1);
  }

  autoWindow->setStates(ax->contrast, ax->threshold, (int)(100. * ax->shave),
                        ax->altmouse, ax->diagonal);
  autox_clear(ax, AUTOX_ALL);
  imodDialogManager.add((QWidget *)autoWindow, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)autoWindow, IMOD_DIALOG);

  vw->ax = ax;
  return(0);
}

// If the user changed color ramps with F9/F10, make radio button reflect it
void autoxCrampSelected(ImodView *vw)
{
  if (!vw->ax)
    return;
  vw->ax->contrast = vw->cramp->clevel == 1 ? 1 : 0;
  autoWindow->setStates(vw->ax->contrast, vw->ax->threshold, 
                        (int)(100. * vw->ax->shave),
                        vw->ax->altmouse, vw->ax->diagonal);
}

int autox_setlow(ImodView *vw, int x, int y)
{
  /* DNM 2/1/01: test for within bounds */
  if (x < 0 || y < 0 || x >= vw->xsize || y >= vw->ysize)
    return 1;
  vw->ax->data[x + (y * vw->xsize)] &= ~AUTOX_WHITE;
  vw->ax->data[x + (y * vw->xsize)] |= AUTOX_BLACK;
  vw->ax->filled = 1;
  imodDraw(vw, IMOD_DRAW_IMAGE);
  return(0);
}

int autox_sethigh(ImodView *vw, int x, int y)
{
  /* DNM 2/1/01: test for within bounds */
  if (x < 0 || y < 0 || x >= vw->xsize || y >= vw->ysize)
    return 1;
  vw->ax->data[x + (y * vw->xsize)] &= ~AUTOX_BLACK;
  vw->ax->data[x + (y * vw->xsize)] |= AUTOX_WHITE;
  vw->ax->filled = 1;
  imodDraw(vw, IMOD_DRAW_IMAGE);
  return(0);
}

int autox_fillmouse(ImodView *vw, int xm, int ym)
{
  vw->xmouse = xm;
  vw->ymouse = ym;
  autoxFill();
  return(0);
}

int autox_build(Autox *ax)
{
  if (!ax)
    return(-1);
  autoxBuild();
  return(0);
}

int autox_next(Autox *ax)
{
  if (!ax)
    return(-1);
  autoxNext();
  return(0);
}

int autox_smooth(Autox *ax)
{
  if (!ax)
    return(-1);
  autoxSmooth();
  return(0);
}

/* DNM 1/17/01: get_contour_edge_points was replaced by call to 
   imodGetImagePoints in library, after adding argument for the flood mask */


/*****************************************************************************
 * FUNCTION: autox_flood
 * Floods an area around the current point
 *
 * RETURNS: 0 if nothing was filled.
 *          1 if somthing was filled.
 *****************************************************************************/
static int autox_flood(Autox *ax)
{
  int threshold;
  int x, y;
  unsigned char *data = ax->data;
  int *xlist = ax->xlist;
  int *ylist = ax->ylist;
  int diagonal = ax->diagonal;
  int ringnext = 0;
  int ringfree = 1;
  int pixind;
  int xsize = ax->vw->xsize;
  int ysize = ax->vw->ysize;
  unsigned char neighflag;
  int test;
     
  if (ax->vw->pyrCache)
    autoImage = ax->vw->pyrCache->getFullSection(B3DNINT(ax->vw->zmouse));
  else
    autoImage = ivwGetCurrentSection(ax->vw);
  if (autoImage && ax->vw->ushortStore) {
    if (ivwCopyImageToByteBuffer(ax->vw, autoImage, ax->byteSlice))
      return 0;
    autoImage = ivwMakeLinePointers(ax->vw, ax->byteSlice, xsize, ysize, 0);
  }
  if (!autoImage)
    return(0);

  x = (int)ax->vw->xmouse;
  y = (int)ax->vw->ymouse;

  /* DNM: incrementing threshold by 1, then testing for less than threshold
     or >= threshold, makes areas match what shows up as black and white */
  threshold = ax->threshold + 1;

  /* DNM: exclude rgba visual */
  if (App->depth == 8 && !App->rgba){
    threshold = (int)((((float)ax->vw->rampsize/256.0f)
                       * threshold) + ax->vw->rampbase);
  }
  ax->threshUsed = threshold - 0.5;

  if (autoImage[y][x] < threshold)
    ax->reverse = 1;
  else
    ax->reverse = 0;


  /* initialize the ring buffer */
  xlist[0] = x;
  ylist[0] = y;
  data[x + y * xsize] |= AUTOX_CHECK;
  neighflag  = AUTOX_FLOOD | AUTOX_CHECK;

  while (ringnext != ringfree) {

    /* check next point on list */
    x = xlist[ringnext];
    y = ylist[ringnext];
    pixind = x + y * xsize;
    if (ax->reverse)
      test = (autoImage[y][x] < threshold) ||
        (data[pixind] & AUTOX_BLACK);
    else
      test = ((autoImage[y][x] >= threshold) ||
              (data[pixind] & AUTOX_WHITE)) &&
        (~data[pixind] & AUTOX_BLACK);
    if (test) {

      /* If point passes test, mark as flood */ 
      data[pixind] |= AUTOX_FLOOD;

      /* add each of four neighbors on list if coordinate is legal
         and they are not already on list or in flood */
      if (x > 0 && !(data[pixind - 1] & neighflag)) {
        xlist[ringfree] = x - 1;
        ylist[ringfree++] = y;
        ringfree %= ax->listsize;
        data[pixind - 1] |= AUTOX_CHECK;
      }
      if (x < xsize - 1 && !(data[pixind + 1] & neighflag)) {
        xlist[ringfree] = x + 1;
        ylist[ringfree++] = y;
        ringfree %= ax->listsize;
        data[pixind + 1] |= AUTOX_CHECK;
      }
      if (y > 0 && !(data[pixind - xsize] & neighflag)) {
        xlist[ringfree] = x;
        ylist[ringfree++] = y - 1;
        ringfree %= ax->listsize;
        data[pixind - xsize] |= AUTOX_CHECK;
      }
      if (y < ysize - 1 && !(data[pixind + xsize] & neighflag)) {
        xlist[ringfree] = x;
        ylist[ringfree++] = y + 1;
        ringfree %= ax->listsize;
        data[pixind + xsize] |= AUTOX_CHECK;
      }

      if (diagonal) {
        if (x > 0 && y > 0 && 
            !(data[pixind - 1 - xsize] & neighflag)) {
          xlist[ringfree] = x - 1;
          ylist[ringfree++] = y - 1;
          ringfree %= ax->listsize;
          data[pixind - 1 - xsize] |= AUTOX_CHECK;
        }
        if (x < xsize - 1 && y > 0 && 
            !(data[pixind + 1 - xsize] & neighflag)) {
          xlist[ringfree] = x + 1;
          ylist[ringfree++] = y - 1;
          ringfree %= ax->listsize;
          data[pixind + 1 - xsize] |= AUTOX_CHECK;
        }
        if (x > 0 && y < ysize - 1 && 
            !(data[pixind - 1 + xsize] & neighflag)) {
          xlist[ringfree] = x - 1;
          ylist[ringfree++] = y + 1;
          ringfree %= ax->listsize;
          data[pixind - 1 + xsize] |= AUTOX_CHECK;
        }
        if (x < xsize - 1 && y < ysize - 1 && 
            !(data[pixind + 1 + xsize] & neighflag)) {
          xlist[ringfree] = x + 1;
          ylist[ringfree++] = y + 1;
          ringfree %= ax->listsize;
          data[pixind + 1 + xsize] |= AUTOX_CHECK;
        }
      }
    }

    /* Take point off list, advance next pointer */
    data[pixind] &= ~AUTOX_CHECK;
    ringnext++;
    ringnext %= ax->listsize;
  }

  return(1);
}

/* DNM: removed recursive functions for flood fill and patch fill */
/* Moved lots of stuff to autocont.c in libimod */

static void autox_clear(Autox *ax, unsigned char bit)
{
  int i;
  if (ax->data)
    for(i = 0; i < ax->vw->xysize; i++)
      ax->data[i] &= ~bit;
}
