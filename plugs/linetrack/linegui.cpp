/*
 *  linegui.c -- Special plugin for line finding
 *
 */

/*****************************************************************************
 *   Copyright (C) 1997-2002 by Boulder Laboratory for 3-Dimensional Fine    *
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


/* include needed Qt headers.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <qpushbutton.h>
#include <qsignalmapper.h>
#include <qlayout.h>
#include <tooledit.h>
#include <qlabel.h>
#include <qtooltip.h>

/* Since we are in the IMOD source tree, take includes from
   the imod directory and have  full capabilities, rather than using
   limited imodplug.h and model.h from include directory */

#include "dia_qtutils.h"
#include "../../imod/imod.h"
#include "../../imod/imodplug.h"
#include "../../imod/control.h"
#include "linegui.h"

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

}PlugData;

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

static PlugData thisPlug = { 0, 0 };


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

void imodPlugExecute(ImodView *inImodView)
{
  PlugData *plug;
  static int first = 1;
  plug = &thisPlug;

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
  first = 0;

  /*
   * This creates the plug window.
   */
  plug->window  = new LineTrack(imodDialogManager.parent(IMOD_DIALOG),
                                "line tracker");

  imodDialogManager.add((QWidget *)plug->window, IMOD_DIALOG);
  plug->window->show();
}


// The constructor for the window class

char *buttonLabels[] = {"Track", "Copy", "Undo", "Done", "Help"};
char *buttonTips[] = 
  {"Track from last to current model point - Hot key: Spacebar",
   "Copy and adjust contour from adjacent section - Hot key: Apostrophe",
   "Undo or redo last tracking operation - Hot key: Semicolon or U", 
   "Close line tracker", "Open help window"};

LineTrack::LineTrack(QWidget *parent, const char *name)
  : DialogFrame(parent, 5, buttonLabels, buttonTips, true, "Line Tracker", "",
                name)
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
  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
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

  // Fill the edit with the right kind of value
  if (isInt)
    str.sprintf("%d", *iPtr);
  else
    str.sprintf("%g", *fPtr);
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
  int copytol, curx, cury, curz, i, j, flipped;
  float zdiff;
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
      for (i = 0; i < plug->ysize; i++)
        *datap++ = image[j][i];
  }
  plug->idataSec = curz;
  plug->idataFlipped = flipped;

  if (copytol != 0){
    zdiff = plug->cont->pts->z - curz;
    if (zdiff < 0) zdiff = -zdiff;
    if (zdiff > 1.01 || zdiff < 0.99) {
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
    imodNewContour(theModel);
    plug->cont = imodContourGet(theModel);
    imodContourCopy(tmpcont, plug->cont);
    free(tmpcont);
    pts = plug->cont->pts;
    for (i = 0; i < maxpoint; i++) pts++->z = curz;
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
    return;
  }

  memcpy(pts, plug->cont->pts, sizeof(Ipoint) * maxpoint);
  free(plug->cont->pts);
  plug->cont->pts = pts;
  if(copytol == 0) {
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
    }
  } else  {

    p_copy = (float *)malloc(2 * maxpoint * sizeof(Ipoint));
    if (!p_copy){
      wprint("\aLineTrack memory allocation error!\n");
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
  ivwDraw(plug->view, IMOD_DRAW_MOD);
  return;
}

// Undo the last operation
void LineTrack::undo()
{
  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);
  Icont *cont;
  int x, y, z;
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
        imodDeleteContour(theModel);
        imodSetIndex(theModel, plug->ob, plug->copiedco, plug->pt);
        plug->copiedco = -1;
        ivwDraw(plug->view, IMOD_DRAW_MOD);
      }
    }
  }
}


// Respond to an action button
void LineTrack::buttonPressed(int which)
{
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
    close();
    break;
  case 4:
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
       "          u or Semicolon to undo last action\n",
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
  imodDialogManager.remove((QWidget *)plug->window);

  plug->view = NULL;
  plug->window = NULL;
  if (plug->undoCont)
    imodContourDelete(plug->undoCont);
  if (plug->idata)
    free(plug->idata);
  e->accept();
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
