/*
 *  linegui.c -- Special plugin for line finding
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

Log at end of file
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
#include "../../imod/xzap.h"
#include "../../imod/preferences.h"
#else
#include "imod.h"
#include "control.h"
#endif
#include "linegui.h"
#include "xzap.h"
#include "preferences.h"
#include "undoredo.h"

#ifndef LINE_PLUGIN
static char *imodPlugInfo(int *type);
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

#ifdef F77FUNCAP
#define linetrack_ LINETRACK
#define conttrack_ CONTTRACK
#endif
#define CONTOUR_POINT_MAX 1000
extern "C" {
void linetrack_(unsigned char *image, int *nx, int *ny,
                float *points, int *csize, int *cpnt, int *cmax,
                int   *ksize,
                int   *knum,
                float *sigma,
                float *h,
                int   *ifdark,
                float *stepsize,
                float *redtol,
                int   *ifreplace,
                float *offset,
                int   *closecont,
                int   *iffail);

void conttrack_(unsigned char *image, int *nx, int *ny,
                float *points, int *csize, int *cpnt, float *p_copy, int *cmax,
                int   *ksize,
                int   *knum,
                float *sigma,
                float *h,
                int   *ifdark,
                float *stepsize,
                float *redtol,
                float *offset,
                int   *copytol,
                int   *copypool,
                int   *copyfit);
}

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
char *imodPlugInfo(int *type)
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
  ctrl   = event->state() & Qt::ControlButton;
  shift  = event->state() & Qt::ShiftButton;
    
  switch(keysym){
  case Qt::Key_Apostrophe: 

    plug->docopy = plug->copytol;
        
  case Qt::Key_Space:
    if(ctrl)
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
    zapLimitWindowPos(plug->window->width(), plug->window->height(), 
                      plug->left, plug->top);
    plug->window->move(plug->left, plug->top);
  }
  first = 0;

  plug->window->show();
}


// The constructor for the window class

char *buttonLabels[] = {"Track", "Copy", "Undo", "Reset", "Done", "Help"};
char *buttonTips[] = 
  {"Track from last to current model point - Hot key: Spacebar",
   "Copy and adjust contour from adjacent section - Hot key: Apostrophe",
   "Undo last modeling operation - Hot key: Semicolon or U", 
   "Reset parameters to built-in default values", "Close line tracker",
   "Open help window"};

LineTrack::LineTrack(QWidget *parent, const char *name)
  : DialogFrame(parent, 6, 2, buttonLabels, buttonTips, true, 
                ImodPrefs->getRoundedStyle(), "Line Tracker", "", name)
{
  PlugData *plug = &thisPlug;

  mGrid = new QGridLayout(mLayout);
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
void LineTrack::makeEditRow(int row, char *label, int *iPtr, float *fPtr,
                            bool isInt, float min, float max, char *tip)
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
  qlab->setAlignment(AlignRight | AlignVCenter);

  // Make the tool edit and connect to mapper
  mEdit[row] = new ToolEdit(this, 8);
  mGrid->addWidget(mEdit[row], row, 1);
  mEdit[row]->setAlignment(AlignRight);
  mEdit[row]->setFocusPolicy(ClickFocus);
  mMapper->setMapping(mEdit[row], row);
  connect(mEdit[row], SIGNAL(returnPressed()), mMapper, SLOT(map()));
  connect(mEdit[row], SIGNAL(returnPressed()), this, SLOT(setFocus()));
  connect(mEdit[row], SIGNAL(focusLost()), mMapper, SLOT(map()));
  str = tip;
  QToolTip::add(mEdit[row], str);

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
    ival = atoi(mEdit[which]->text().latin1());
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
  unsigned char *datap;
  int npoint, maxpoint, curpt, iffail, closecont;
  Ipoint *pts;
  int copytol, curx, cury, curz, i, j, flipped, zdiff;
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
  ivwGetLocation(plug->view, &curx, &cury, &curz);
  flipped = imodGetFlipped(theModel);
  
  if (curz != plug->idataSec || flipped != plug->idataFlipped) {
    datap = plug->idata;
    for (j = 0; j < plug->ysize; j++)
      for (i = 0; i < plug->xsize; i++)
        *datap++ = image[j][i];
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
    dia_vasmsg
      ("Line Track Plugin Help\n\n",
       "Line Track will create points along a line between "
       "the current selected model point and the previous "
       "point.  It does this by filtering with a collection "
       "of elongated kernels at a range of orientations.\n",
       "\nThis Plugin also has a Contour Copying function which will "
       "copy the current contour onto an adjacent section (the "
       "section being displayed) then adjust the position of the "
       "contour to match the image on that section.  It uses kernels "
       "that are shaped like the contour.\n\n",
       "Hot Keys: Space bar to add points between the last and the "
       "current point\n",
       "          Ctrl-Space to track from the current point "
       "and close the contour\n",
       "          Apostrophe to copy contour to current section\n",
       "          u or Semicolon to undo last modeling action\n",
       "\nParameters:\n\n",
       "0 Light, 1 Dark Lines: Set to 0 or 1 to follow bright "
       "or dark lines.\n\n",
       "Offset from line: Amount to offset model points from the "
       "center of the line.  Set positive for a positive offset "
       "when modeling to the right.\n\n"
       "Sigma (Line width): Sigma is actually the half-width of "
       "the central part of the kernel, but should be set to somewhat "
       "more than half the width of the lines you are trying to "
       "track.\n\n",
       "H (half-length): The half-length of the kernel in its "
       "elongated direction.\n\n",
       "Kernel Half-size: Should be at least H and 3 * Sigma "
       "but could be reduced to run faster.\n\n",
       "Reduction Tolerance: After finding a path, the program will "
       "remove any points that are within this distance of lines "
       "between the remaining points.\n\n",
       "Copy Tolerance: Maximum distance that the program will search "
       "for best position when copying a contour to a new section.\n\n"
       "Copy Pooling: Number of points that will be considered "
       "together when trying to shift a particular point.  Bigger "
       "numbers will give more smoothing.  With a positive value, the "
       " point being shifted will be given the most weight; with a "
       "negative value, all points will be given equal weight, "
       "resulting in even more smoothing.\n\n"
       "Copy Smoothing: Number of points to fit to in the final "
       "smoothing of the contour (0 for no smoothing, higher values "
       "for more smoothing).\n\n"
       "Number of Kernels: Could be reduced to run faster.\n\n",
       "Step Size: Size of step to move between points; could be "
       "reduced to run faster.\n",
       NULL);
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

  plug->view = NULL;
  plug->window = NULL;
  if (plug->undoCont)
    imodContourDelete(plug->undoCont);
  if (plug->idata)
    free(plug->idata);
  e->accept();
}

void LineTrack::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::fontChange(oldFont);
}

// Close on escape, pass on keys
void LineTrack::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

void LineTrack::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

/*
$Log$
Revision 1.10  2005/03/20 19:55:37  mast
Eliminating duplicate functions

Revision 1.9  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 1.8  2004/11/09 01:19:16  mast
Fixed problem in not allocating big enough copy array in contour copy

Revision 1.7  2004/11/05 19:08:12  mast
Include local files with quotes, not brackets

Revision 1.6  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 1.5  2004/07/11 18:26:30  mast
Made it set contour time correctly when copying

Revision 1.4  2004/06/23 03:34:14  mast
Added ability to save and restore all settings, and a button to restore
defaults

Revision 1.3  2004/01/22 19:12:43  mast
changed from pressed() to clicked() or accomodated change to actionClicked

Revision 1.2  2003/11/27 06:08:01  mast
Fixed bug in copying image that made it work only for square images

Revision 1.1  2003/10/25 16:16:06  mast
convert from plugin to internal module

Revision 3.5  2003/09/16 02:07:24  mast
Changed to copy image data into a buffer using new line pointers

Revision 3.4  2003/05/12 19:13:06  mast
Fix hot key spelling

Revision 3.3  2003/04/17 18:34:29  mast
adding parent to window creation

Revision 3.2  2003/03/26 15:56:46  mast
Change lostFocus to focusLost

Revision 3.1  2003/02/10 20:55:40  mast
Merge Qt source

Revision 1.1.2.1  2003/01/27 00:35:53  mast
Qt version

Revision 3.1  2002/08/22 05:54:47  mast
Allocated and passed a working array to conttrack to prevent crashes,
added a Copy button.

*/
