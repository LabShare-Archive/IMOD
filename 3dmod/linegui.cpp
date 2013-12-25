/*
 *  linegui.c -- Special plugin for line finding
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */


/* include needed Qt headers.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <qpushbutton.h>
#include <qsignalmapper.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qtooltip.h>

/* Since we are in the IMOD source tree, take includes from
   the imod directory and have  full capabilities, rather than using
   limited imodplug.h and model.h from include directory */

#include "dia_qtutils.h"
#include "tooledit.h"
#ifdef LINE_PLUGIN
#include "../../imod/imod.h"
#include "../../imod/imodplug.h"
#include "../../imod/control.h"
#include "../../imod/preferences.h"
#else
#include "imod.h"
#include "control.h"
#endif
#include "linegui.h"
#include "preferences.h"
#include "undoredo.h"

#ifndef LINE_PLUGIN
static const char *imodPlugInfo(int *type);
static int imodPlugKeys(ImodView *vw, QKeyEvent *event);
static void imodPlugExecute(ImodView *inImodView);

LineTrackModule::LineTrackModule()
{
  mInfo = imodPlugInfo;
  mExecuteType = NULL;
  mExecute = imodPlugExecute;
  mKeys = imodPlugKeys;
}
#endif

/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
  ImodView    *view;
  LineTrack   *window;

  Iobj  *obj;
  Icont *cont;
  Icont *undoCont, *tmpCont;
  int    ob, co, pt;
  int    csection;
  int    xsize, ysize, zsize;

  unsigned char *idata;    /* Image array for contiguous slice */
  int   idataXsize;        /* Current sizes for array */
  int   idataYsize;  
  int   idataSec;          /* Current section in array */
  int   idataFlipped;      /* Flag for whether model was flipped */

  int   cmax;
  int   ksize;   /* Kernal size   */
  int   knum;    /* Kernal number */
  float sigma; 
  float h;
  int   ifdark;
  float stepsize;
  float redtol;
  int   ifreplace;
  float offset;
  int   closecont;
  int   copytol;
  int   docopy;
  int   copypool;
  int   copyfit;
  int   copiedco;
  int   copysize;
  int   left;
  int   top;

}PlugData;

static void setDefaults();

#define CONTOUR_POINT_MAX 1000

static PlugData thisPlug = { 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

// A function to set or reset the interface defaults
static void setDefaults()
{
  PlugData *plug = &thisPlug;
  plug->ksize    = 7;
  plug->knum     = 30;
  plug->sigma    = 2.5f;
  plug->h        = 7.0f;
  plug->ifdark   = 1;
  plug->stepsize = 3.0f;
  plug->redtol   = 0.3f;
  plug->offset   = 0.0f;
  plug->copytol  = 3;
  plug->copypool = 5;
  plug->copyfit  = 5;
}

/*
 * Called by the imod plugin load function. 
 */
const char *imodPlugInfo(int *type)
{
  if (type)
    *type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS;
  return("Line Track");
}

/*
 *  Grab hotkey input. return 1 if we handle the key.
 */
int imodPlugKeys(ImodView *vw, QKeyEvent *event)
{
  PlugData *plug = &thisPlug;
  int keysym;
  int    keyhandled = 0;
  int    ctrl;
  int    shift;

  /*
   * Don't grab keys if plug window isn't open.
   */
  if (!plug->view)
    return 0;
    
  /* The keysym values are Key_A ...
   * Key_Space, Key_Comma...
   */
  keysym = event->key();

  /*
   * Modifier key mask.  Set ctrl and shift to true
   * if the coresponding key is pressed.
   */
  ctrl   = event->modifiers() & Qt::ControlModifier;
  shift  = event->modifiers() & Qt::ShiftModifier;
    
  switch(keysym){
  case Qt::Key_Apostrophe: 

    plug->docopy = plug->copytol;
        
  case Qt::Key_Space:
    if(ctrl || shift)
      plug->closecont = 1;
    plug->window->track(0);
    keyhandled = 1;
    break;
  case Qt::Key_Semicolon: 
  case Qt::Key_U:
    plug->window->undo();
    keyhandled = 1;
    break;
  }
  return keyhandled;
}

/*
 *  Execute any function or program you wish with this function.
 *  Here we open up a window for user interaction.
 *  see imodplug.h for a list of support functions.
 */

#define MAX_SETTINGS 13
void imodPlugExecute(ImodView *inImodView)
{
  PlugData *plug;
  static int first = 1;
  int numVals;
  plug = &thisPlug;
  double values[MAX_SETTINGS];

  if (inImodView->rgbStore || inImodView->fakeImage) {
    wprint("\aLine tracker will not work on RGB or blank data\n");
    return;
  }

  if (plug->window){
    plug->window->raise();
    return;
  }

  plug->view = inImodView;
  ivwGetImageSize(inImodView, &plug->xsize, &plug->ysize, &plug->zsize);

  /* 
   * Initialize data. 
   */
  plug->obj       = NULL;
  plug->cont      = NULL;
  plug->undoCont  = NULL;
  plug->tmpCont   = imodContourNew();
  plug->idata     = NULL;
  plug->idataXsize = 0;
  plug->idataYsize = 0;

  plug->cmax     = CONTOUR_POINT_MAX;
  plug->ifreplace= 1;
  plug->closecont= 0;
  plug->docopy   = 0;
  plug->copiedco = -1;

  if (first) {
    setDefaults();

    // Get window position and values from settings the first time
    numVals = ImodPrefs->getGenericSettings("LineTracker", values, 
                                            MAX_SETTINGS);
    if (numVals > 12 ) {
      plug->left = (int)values[0];
      plug->top = (int)values[1];
      plug->ksize    = (int)values[2];
      plug->knum     = (int)values[3];
      plug->sigma    = (float)values[4];
      plug->h        = (float)values[5];
      plug->ifdark   = (int)values[6];
      plug->stepsize = (float)values[7];
      plug->redtol   = (float)values[8];
      plug->offset   = (float)values[9];
      plug->copytol  = (int)values[10];
      plug->copypool = (int)values[11];
      plug->copyfit  = (int)values[12];
      // Future values test on numVals
    }
  }

  /*
   * This creates the plug window.
   */
  plug->window  = new LineTrack(imodDialogManager.parent(IMOD_DIALOG),
                                "line tracker");

  imodDialogManager.add((QWidget *)plug->window, IMOD_DIALOG);

  if (!first || numVals > 12) {
    diaLimitWindowPos(plug->window->width(), plug->window->height(), 
                      plug->left, plug->top);
    plug->window->move(plug->left, plug->top);
  }
  first = 0;

  adjustGeometryAndShow((QWidget *)plug->window, IMOD_DIALOG);
}


// The constructor for the window class

const char *buttonLabels[] = {"Track", "Copy", "Undo", "Reset", "Done", "Help"};
const char *buttonTips[] = 
  {"Track from last to current model point - Hot key: Spacebar",
   "Copy and adjust contour from adjacent section - Hot key: Apostrophe",
   "Undo last modeling operation - Hot key: Semicolon or Shift+U", 
   "Reset parameters to built-in default values", "Close line tracker",
   "Open help window"};

LineTrack::LineTrack(QWidget *parent, const char *name)
  : DialogFrame(parent, 6, 2, buttonLabels, buttonTips, true, 
                ImodPrefs->getRoundedStyle(), "Line Tracker", "", name)
{
  PlugData *plug = &thisPlug;

  mGrid = new QGridLayout();
  mLayout->addLayout(mGrid);
  mMapper = new QSignalMapper(this);
  connect(mMapper, SIGNAL(mapped(int)), this, SLOT(valueEntered(int)));

  makeEditRow(0, "0 Light, 1 Dark Lines", &plug->ifdark, NULL, true, 0., 1.,
              "0 or 1 if lines are lighter or darker than background");
  makeEditRow(1, "Offset from Line", NULL, &plug->offset, false, -5.0, 5.0,
              "Offset from center to place contour on edge of feature");
  makeEditRow(2, "Sigma (Line width)", NULL, &plug->sigma, false, 0.5, 7.0,
              "Half-width of smoothing gaussian across the line");
  makeEditRow(3, "H (half-length)", NULL, &plug->h, false, 1.0, 20.0,
              "Half-length of smoothing gaussian along the line");
  makeEditRow(4, "Kernel Half-size", &plug->ksize, NULL, true, 1., 10.,
              "Determines total size of kernels");
  makeEditRow(5, "Reduction Tolerance", NULL, &plug->redtol, false, 0.0, 5.0,
              "Maximum error in removing points along path");
  makeEditRow(6, "Copy Tolerance", &plug->copytol, NULL, true, 0., 10.,
              "Maximum shift of points when copying");
  makeEditRow(7, "Copy Pooling", &plug->copypool, NULL, true, -5., 5.,
              "# of points to consider when shifting after a copy");
  makeEditRow(8, "Copy Smoothing", &plug->copyfit, NULL, true, 0., 9.,
              "# of points to smooth over after a copy");
  makeEditRow(9, "Number of Kernels", &plug->knum, NULL, true, 9., 40.,
              "# of directions to analyze");
  makeEditRow(10, "Step Size", NULL, &plug->stepsize, false, 1., 20.0,
              "Pixels to move between points when tracking");

  mButtons[2]->setEnabled(false);    
  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));
}

// Make one edit row and store information about it
void LineTrack::makeEditRow(int row, const char *label, int *iPtr, float *fPtr,
                            bool isInt, float min, float max, const char *tip)
{
  // Save the value pointers and limits
  mMinVal[row] = min;
  mMaxVal[row] = max;
  mIsInt[row] = isInt;
  mIntPtr[row] = iPtr;
  mFloatPtr[row] = fPtr;
  
  // Make the label
  QString str = label;
  QLabel *qlab = new QLabel(str, this);
  mGrid->addWidget(qlab, row, 0);
  qlab->setAlignment(Qt::AlignRight | Qt::AlignVCenter);

  // Make the tool edit and connect to mapper
  mEdit[row] = new ToolEdit(this, 8);
  mGrid->addWidget(mEdit[row], row, 1);
  mEdit[row]->setAlignment(Qt::AlignRight);
  mEdit[row]->setFocusPolicy(Qt::ClickFocus);
  mMapper->setMapping(mEdit[row], row);
  connect(mEdit[row], SIGNAL(returnPressed()), mMapper, SLOT(map()));
  connect(mEdit[row], SIGNAL(returnPressed()), this, SLOT(setFocus()));
  connect(mEdit[row], SIGNAL(focusLost()), mMapper, SLOT(map()));
  str = tip;
  mEdit[row]->setToolTip(str);

  fillinValue(row);
}

// Put the current value into a row's edit box , using the right kind of value
void LineTrack::fillinValue(int row)
{
  QString str;
  if (mIsInt[row])
    str.sprintf("%d", *mIntPtr[row]);
  else
    str.sprintf("%g", *mFloatPtr[row]);
  mEdit[row]->setText(str);
}

// Process a value entered in an edit box
void LineTrack::valueEntered(int which)
{
  int ival;
  float fval;
  QString str;

  // 1/25/03: QString's toInt is unforgiving of stray characters after the
  // number and produces a 0
  if (mIsInt[which]) {
    ival = atoi(LATIN1(mEdit[which]->text()));
    if (ival < mMinVal[which])
      ival = (int)floor(mMinVal[which] + 0.5);
    if (ival > mMaxVal[which])
      ival = (int)floor(mMaxVal[which] + 0.5);
    str.sprintf("%d", ival);
    *mIntPtr[which] = ival;
  } else {

    fval = mEdit[which]->text().toFloat();
    if (fval < mMinVal[which])
      fval = mMinVal[which];
    if (fval > mMaxVal[which])
      fval = mMaxVal[which];
    str.sprintf("%g", fval);
    *mFloatPtr[which] = fval;
  }
  mEdit[which]->setText(str);
}

// Track or copy operation
void LineTrack::track(int client)
{
  unsigned char **image;
  int npoint, maxpoint, curpt, iffail, closecont;
  Ipoint *pts;
  int copytol, curx, cury, curz, i, flipped, zdiff;
  Icont *tmpcont;
  float *p_copy;
    
  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);
  ivwGetImageSize(plug->view, &plug->xsize, &plug->ysize, &plug->zsize);

  closecont = plug->closecont;
  plug->closecont = 0;

  copytol = plug->docopy;
  plug->docopy = 0;
  if (client > 1)
    copytol = plug->copytol;

  plug->cont = imodContourGet(theModel);
  if (!plug->cont){
    wprint("\aLine Track Error:\n"
           "  No Contour Selected.\n");
    return;
  }
  maxpoint = imodContourGetMaxPoint(plug->cont);
  if (maxpoint < 2){
    wprint("\aLine Track Error:\n"
           "  Contour must have at least 2 points.\n");
    return;
  }
  ivwGetLocation(plug->view, &curx, &cury, &curz);
  if (ivwDataInTileOrStripCache(plug->view))
    image = ivwGetTileCachedSection(plug->view, curz);
  else
    image = ivwGetCurrentZSection(plug->view);
  if (!image){
    wprint("\aLine Track Error:\n"
           "  No current image data.\n");
    return;
  }

  // Create data array if size does not match
  if (plug->xsize != plug->idataXsize || plug->ysize != plug->idataYsize) {
    if (plug->idata)
      free(plug->idata);
    plug->idata = (unsigned char *)malloc(plug->xsize * plug->ysize);
    if (!plug->idata) {
      plug->idataXsize = 0;
      wprint("\aLine Track failed to get memory for image data.\n");
      return;
    }
    plug->idataXsize = plug->xsize;
    plug->idataYsize = plug->ysize;
    plug->idataSec = -1;
  }

  // Fill the data array if the section or flipping has changed
  flipped = imodGetFlipped(theModel);
  
  if (curz != plug->idataSec || flipped != plug->idataFlipped) {
    if (ivwCopyImageToByteBuffer(plug->view, image, plug->idata)) {
      wprint("\aLine Track failed to get memory for short to byte map.\n");
      return;
    }
  }
  plug->idataSec = curz;
  plug->idataFlipped = flipped;

  if (copytol != 0){
    zdiff = (int)floor(plug->cont->pts->z + 0.5) - curz;
    if (zdiff != 1 && zdiff != -1) {
      wprint("\aContour Copy Error:\n"
             "  Copy must be to adjacent section\n");
      return;
    }
    if (maxpoint < 4){
      wprint("\aContour Copy Error:\n"
             "  Contour must have at least 4 points.\n");
      return;
    }
    tmpcont = imodContourDup(plug->cont);
    imodGetIndex(theModel, &plug->ob, &plug->copiedco, &plug->pt);

    plug->obj = imodObjectGet(theModel);
    plug->view->undo->contourAddition(imodObjectGetMaxContour(plug->obj));
    imodNewContour(theModel);
    plug->cont = imodContourGet(theModel);
    imodContourCopy(tmpcont, plug->cont);
    free(tmpcont);
    ivwSetNewContourTime(plug->view, imodObjectGet(theModel), plug->cont);
    pts = plug->cont->pts;
    for (i = 0; i < maxpoint; i++)
      pts++->z = curz;
    imodGetIndex(theModel, &plug->ob, &plug->co, &curpt);
    curpt = plug->pt;

    // Copy any contour properties to new contour
    if (istoreCountContSurfItems(plug->obj->store, plug->copiedco, 0)) {
      plug->view->undo->objectPropChg(plug->ob);
      istoreCopyContSurfItems(plug->obj->store, &plug->obj->store,
                              plug->copiedco,
                              imodObjectGetMaxContour(plug->obj) - 1, 0);
    }
  }

  /* Save contour for undo. */
  if (plug->undoCont){
    imodContourDelete(plug->undoCont);
    plug->undoCont = NULL;
  }

  if (copytol == 0) {
    plug->undoCont = imodContourDup(plug->cont);

    /* DNM 2/25/01: make flag for no copied contour be -1, not 0, since
       0 could be a copied contour */
    plug->copiedco = -1;
    imodGetIndex(theModel, &plug->ob, &plug->co, &plug->pt);
    curpt = plug->pt;
  }

  mButtons[2]->setEnabled(true);    

  npoint = maxpoint + CONTOUR_POINT_MAX;

  pts = (Ipoint *)malloc(npoint * sizeof(Ipoint));
  if (!pts){
    wprint("\aLineTrack memory allocation error!\n");
    plug->view->undo->flushUnit();
    return;
  }

  memcpy(pts, plug->cont->pts, sizeof(Ipoint) * maxpoint);
  free(plug->cont->pts);
  plug->cont->pts = pts;
  if(copytol == 0) {
    plug->view->undo->contourDataChg();

    linetrack_(plug->idata, &plug->xsize, &plug->ysize,
               (float *)pts, &maxpoint, &curpt , &npoint,
               &plug->ksize,
               &plug->knum,
               &plug->sigma,
               &plug->h,
               &plug->ifdark,
               &plug->stepsize,
               &plug->redtol,
               &plug->ifreplace,
               &plug->offset,
               &closecont,
               &iffail);
    if(iffail != 0){
      wprint("\aLineTrack failed to find path\n");
      plug->view->undo->flushUnit();
    }
  } else  {

    p_copy = (float *)malloc(npoint * sizeof(Ipoint));
    if (!p_copy){
      wprint("\aLineTrack memory allocation error!\n");
      plug->view->undo->flushUnit();
      return;
    }

    conttrack_(plug->idata, &plug->xsize, &plug->ysize,
               (float *)pts, &maxpoint, &curpt, p_copy, &npoint,
               &plug->ksize,
               &plug->knum,
               &plug->sigma,
               &plug->h,
               &plug->ifdark,
               &plug->stepsize,
               &plug->redtol,
               &plug->offset,
               &copytol,
               &plug->copypool,
               &plug->copyfit);

    free(p_copy);
  }
  plug->cont->psize = maxpoint;
  plug -> copysize = maxpoint;
  imodSetIndex(theModel, plug->ob, plug->co, curpt);
  plug->view->undo->finishUnit();
  ivwDraw(plug->view, IMOD_DRAW_MOD);
  return;
}

// Undo the last operation
void LineTrack::undo()
{
  PlugData *plug = &thisPlug;
  plug->view->undo->undo();
  return;

#ifdef OLD_UNDO
  Imod *theModel = ivwGetModel(plug->view);
  Icont *cont;
  int ob, co, pt;


  imodGetIndex(theModel, &ob, &co, &pt);
  cont = imodContourGet(theModel);
    
  if (plug->copiedco < 0) {

    if (!plug->undoCont) return;
    
    if (cont != 0 && ob == plug->ob && co == plug->co){
      imodContourCopy(cont, plug->tmpCont);
      imodContourCopy(plug->undoCont, cont);
      imodContourCopy(plug->tmpCont, plug->undoCont);
      imodSetIndex(theModel, plug->ob, plug->co, plug->pt);
      plug->ob = ob;
      plug->co = co;
      plug->pt = pt;
      ivwDraw(plug->view, IMOD_DRAW_MOD);
    }
  } else {
         
    if (cont != 0 && ob == plug->ob && co == plug->co){
      if (cont->psize == plug->copysize) {
        /* DNM 2/25/01: change from imodContourDelete to this */
        imodDelCurrentContour(theModel);
        imodSetIndex(theModel, plug->ob, plug->copiedco, plug->pt);
        plug->copiedco = -1;
        ivwDraw(plug->view, IMOD_DRAW_MOD);
      }
    }
  }
#endif
}


// Respond to an action button
void LineTrack::buttonPressed(int which)
{
  int i;
  switch (which) {
  case 0:
    track(1);
    break;
  case 1:
    track(2);
    break;
  case 2:
    undo();
    break;

  case 3:
    setDefaults();
    for (i = 0; i < 10; i++)
      fillinValue(i);
    break;

  case 4:
    close();
    break;
  case 5:
    imodShowHelpPage("lineTracker.html#TOP");
    break;
  }
}

// The window is closing, remove from manager and clean up
void LineTrack::closeEvent ( QCloseEvent * e )
{
  PlugData *plug = &thisPlug;
  double values[MAX_SETTINGS];
  QRect pos = ivwRestorableGeometry(plug->window);
  values[0] = pos.left();
  values[1] = pos.top();
  plug->top = pos.top();
  plug->left = pos.left();
  values[2] = plug->ksize;
  values[3] = plug->knum;
  values[4] = plug->sigma;
  values[5] = plug->h;
  values[6] = plug->ifdark;
  values[7] = plug->stepsize;
  values[8] = plug->redtol;
  values[9] = plug->offset;
  values[10] = plug->copytol;
  values[11] = plug->copypool;
  values[12] = plug->copyfit;
  ImodPrefs->saveGenericSettings("LineTracker", MAX_SETTINGS, values);

  imodDialogManager.remove((QWidget *)plug->window);
  if (ivwDataInTileOrStripCache(plug->view))
    ivwFreeTileCachedSection(plug->view);
  plug->view = NULL;
  plug->window = NULL;
  if (plug->undoCont)
    imodContourDelete(plug->undoCont);
  if (plug->idata)
    free(plug->idata);
  e->accept();
}

void LineTrack::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
}

// Close on escape, pass on keys
void LineTrack::keyPressEvent ( QKeyEvent * e )
{
  if (utilCloseKey(e))
    close();
  else
    ivwControlKey(0, e);
}

void LineTrack::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}
