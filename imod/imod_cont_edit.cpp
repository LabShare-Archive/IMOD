/*  IMOD VERSION 2.50
 *
 *  imod_cont_edit.cpp -- Edit contour data in imod.
 *                        Sets up contour join, break and move dialog boxes
 *                        with ContourJoin, ContourBreak, and ContourMove
 *                        classes declared in imod_cont_edit.h and implemented
 *                        here.
 *                        Opens the Surf/Cont/Point dialog box with the
 *                        ContSurfPoint class implemented in form_cont_edit.ui
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

#include <qspinbox.h>
#include <qcheckbox.h>
#include <qlabel.h>
#include <qtooltip.h>
#include <qlayout.h>
#include "dia_qtutils.h"
#include <qhbox.h>
#include <qpushbutton.h>
#include <qradiobutton.h>
#include <qbuttongroup.h>
#include "form_cont_edit.h"
#include "imod.h"
#include "imod_display.h"
#include "imod_edit.h"
#include "imod_input.h"
#include "imod_cont_edit.h"
#include "control.h"
#include "preferences.h"
#include "undoredo.h"

static void setlabel(QLabel *label, Iindex ind);
static bool indexGood(Iindex ind);

struct contour_edit_struct{
  ImodView  *vw;
  ContSurfPoint *dia;
};

struct contour_move_struct{
  ImodView  *vw;
  ContourMove *dia;
  int       wholeSurf;
  int       movetosurf;
  int       surf_moveto;
  int       replace;
  int       expand;
  int       enabled;
  int       keepsize;
  int       moveUpDown;
  int       upOrDown;
};

struct contour_join_struct{
  ImodView  *vw;
  ContourJoin *dia;
  Iindex    i1, i2;
};

struct contour_break_struct{
  ImodView  *vw;
  ContourBreak *dia;
  Iindex    i1, i2;
};

static char *applyDoneHelp[] = {"Apply", "Done", "Help"};

static Iindex nullIndex = {-1, -1, -1};

static bool indexGood(Iindex ind)
{
  return (ind.object >=0 && ind.contour >= 0 && ind.point >= 0);
}


/***************************************************************************/
/*                  CONTOUR BREAKING                                       */
/***************************************************************************/

static struct contour_break_struct cobrk = {NULL, NULL, {0,0,0}, {0,0,0}};

void imodContEditBreak(ImodView *vw)
{
  if (cobrk.dia){
    cobrk.dia->raise();
    return;
  }
  cobrk.vw = vw;
  cobrk.i1 = vw->imod->cindex;
  cobrk.i2 = nullIndex;

  cobrk.dia = new ContourBreak(imodDialogManager.parent(IMOD_DIALOG), 
                               "contour break");

  cobrk.dia->show();
}

static void setlabel(QLabel *label, Iindex ind)
{
  QString str;
  if (indexGood(ind))
    str.sprintf("Object %d, Contour %d, Point %d",
                ind.object+1, ind.contour+1, ind.point+1);
  else
    str.sprintf("Object None, Contour None, Point None ");
  label->setText(str);
}

/*
 * THE CONTOUR BREAK CLASS IMPLEMENTATION
 */
static char *breakTips[] = {"Break contour at the selected point(s)",
                           "Close dialog box", "Open help window"};

ContourBreak::ContourBreak(QWidget *parent, const char *name)
  : ContourFrame(parent, 3, applyDoneHelp, breakTips, name)
{
  diaLabel("Set contour break point(s):", this, mLayout);

  mObjContLabel = diaLabel("Obj", this, mLayout);

  QGridLayout *grid = new QGridLayout(mLayout, 2, 2);

  mButton1 = new QPushButton("Set 1", this);
  grid->addWidget(mButton1, 0, 0);
  mButton1->setFocusPolicy(NoFocus);
  connect(mButton1, SIGNAL(clicked()), this, SLOT(set1Pressed()));
  QToolTip::add(mButton1, "Set first or only break point in contour");

  mSet1Label = new QLabel(" ", this);
  grid->addWidget(mSet1Label, 0, 1);

  mButton2 = new QPushButton("Set 2", this);
  grid->addWidget(mButton2, 1, 0);
  mButton2->setFocusPolicy(NoFocus);
  connect(mButton2, SIGNAL(clicked()), this, SLOT(set2Pressed()));
  QToolTip::add(mButton2, "Set second break point in contour");

  mSet2Label = new QLabel(" ", this);
  grid->addWidget(mSet2Label, 1, 1);

  setCaption(imodCaption("3dmod Break Contours"));
  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));

  setFontDependentWidths();
  setLabels();
  show();
}

void ContourBreak::setFontDependentWidths()
{
  int width = diaSetButtonWidth(mButton1, mRoundedStyle, 1.2, "Set 2");
  mButton2->setFixedWidth(width);
}

void ContourBreak::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  setFontDependentWidths();
  DialogFrame::fontChange(oldFont);
}

// Set the labels based on current indexes
void ContourBreak::setLabels()
{
  QString str;
  int ob = -1, co = -1;

  if (indexGood(cobrk.i1)) {
    str.sprintf("Point %d", cobrk.i1.point+1);
    ob = cobrk.i1.object;
    co = cobrk.i1.contour;
  } else
    str = "Point None ";

  mSet1Label->setText(str);

  if (indexGood(cobrk.i2)) {
    str.sprintf("Point %d", cobrk.i2.point+1);
    ob = cobrk.i2.object;
    co = cobrk.i2.contour;
  } else
    str = "Point None ";
  mSet2Label->setText(str);

  if (co < 0 || ob < 0)
    str = "Object None, Contour None";
  else
    str.sprintf("Object %d, Contour %d", ob + 1, co + 1);
  mObjContLabel->setText(str);
}

// Set the current index for either set, but if the contours don't
// match, null out the other one
void ContourBreak::set1Pressed()
{
  cobrk.i1 = cobrk.vw->imod->cindex;
  if (indexGood(cobrk.i1) && indexGood(cobrk.i2) &&
      (cobrk.i2.object != cobrk.i1.object || 
       cobrk.i2.contour != cobrk.i1.contour))
    cobrk.i2 = nullIndex;
  setLabels();
}

void ContourBreak::set2Pressed()
{
  cobrk.i2 = cobrk.vw->imod->cindex;
  if (indexGood(cobrk.i1) && indexGood(cobrk.i2) &&
      (cobrk.i2.object != cobrk.i1.object || 
       cobrk.i2.contour != cobrk.i1.contour))
    cobrk.i1 = nullIndex;
  setLabels();
}

/*
 * The breaking routine
 */
void ContourBreak::breakCont()
{
  ImodView *vw = cobrk.vw;
  Iindex *i1p = &cobrk.i1;
  Iindex *i2p = &cobrk.i2;

  Icont *cont1, *cont2, *cont;
  Iobj *obj;
  int ob, co, pt;
  int i, ni, pt1, pt2;
  int breakPoints = 1;

  /*
   * Check that at least the first break point is set.
   */
  if (i1p->point < 0){
    wprint("\a\nContour Break Error:\n"
           "\tFirst break point not set.\n");
    return;
  }

  if ((i2p->object >= 0) && 
      (i2p->contour >= 0) && 
      (i2p->point >= 0 )){
          
    if ( ( i2p->object != i1p->object) ||
         (i2p->contour != i1p->contour)){
      wprint("\a\nContour Break Error:\n"
             "\tBoth break points must be on the same contour.\n");
      return;
    }
    pt1 = i1p->point;
    pt2 = i2p->point;
    breakPoints = 2;
  }else{
    pt1 = 0;
    pt2 = i1p->point;
  }
     
  if ((pt1<0) || (pt2 < 0)){
    wprint("\a\nContour Break Error:\n"
           "\tInvalid break points set.\n");
    return;
  }
  if (pt1 == pt2){
    wprint("\a\nContour Break Error:\n"
           "\tBreak points can't be the same.\n");
    return;
  }

  /* DNM: make sure object number is valid before accessing it */
  if (i1p->object >= (int)vw->imod->objsize) {
    wprint("\a\nContour Break Error:\n"
           "\tObject number no longer valid.\n");
    return;
  }

  /* DNM: now set up to save and restore current location upon error */
  imodGetIndex(vw->imod, &ob, &co, &pt);
  imodSetIndex(vw->imod,
               i1p->object, i1p->contour, i1p->point);
  obj = imodObjectGet(vw->imod);

  /* DNM 2/12/01: need to test that the contour number is still legal */
  if (i1p->contour >= obj->contsize){
    wprint("\a\nContour Break Error:\n"
           "\tContour number is no longer valid.\n");
    imodSetIndex(vw->imod, ob, co, pt);
    return;
  }

  cont1 = imodContourGet(vw->imod);

  if ((!obj) || (!cont1)){
    wprint("\a\nContour Break Error:\n"
           "\tInvalid Model data or no break point selected.\n");
    imodSetIndex(vw->imod, ob, co, pt);
    return;
  }

  if (!cont1->psize){
    wprint("\a\nContour Break Error:\n"
           "\tCurrent contour has no points.\n");
    imodSetIndex(vw->imod, ob, co, pt);
    return;
  }

  if ((pt1 >= cont1->psize) || (pt2 >= cont1->psize)){
    wprint("\a\nContour Break Error:\n"
           "\tInvalid break points set.\n");
    imodSetIndex(vw->imod, ob, co, pt);
    return;
  }

  vw->undo->contourDataChg();
  vw->undo->contourAddition(obj->contsize);
  cont = imodContourDup(cont1);     
  cont2 = cont;

  if (pt1 > pt2){
    int tpt;
    tpt = pt1;
    pt1 = pt2;
    pt2 = tpt;
  }
     
  /* DNM: handle sizes properly below */
  /* Build up our new contour. */
  ni = 0;
  if (breakPoints == 1){
    for(i = pt2; i < cont1->psize; i++, ni++) {
      cont2->pts[ni] = cont1->pts[i];
      if (cont1->sizes)
        cont2->sizes[ni] = cont1->sizes[i];
    }
    cont2->psize = ni;
  }else{
    for(i = pt1; i <= pt2; i++, ni++) {
      cont2->pts[ni] = cont1->pts[i];
      if (cont1->sizes)
        cont2->sizes[ni] = cont1->sizes[i];
    }
    cont2->psize = ni;
  }


  /* Resize our old contour. */
  if (breakPoints == 1){
    cont1->psize = pt2;
  }else{
    ni = pt1;
    for(i = pt2+1; i < cont1->psize; i++, ni++) {
      cont1->pts[ni] = cont1->pts[i];
      if (cont1->sizes)
        cont1->sizes[ni] = cont1->sizes[i];
    }
    cont1->psize -= pt2-pt1+1;
  }

  i1p->point = i2p->point = -1;

  setLabels();

  /* Set wild flag for each resulting contour */
  imodel_contour_check_wild(cont1);
  imodel_contour_check_wild(cont2);

  imodObjectAddContour(obj, cont);
  vw->undo->finishUnit();
  imodDraw(vw, IMOD_DRAW_MOD);
  return;
}

// Action buttons
void ContourBreak::buttonPressed(int which)
{
  switch (which) {
  case 0:  // Apply
    breakCont();
    break;

  case 1:  // Done
    close();
    break;

  case 2: 
    dia_vasmsg
      ("Contour Break Help:\n\n"
       "Break a single contour into two contours.  "
       "The break will occur at one or two set points.  "
       "To set a point, first select the current point then "
       "select the [Set 1] or [Set 2] button.\n\n",
       "If only one set point is set the original contour will "
       "contain all the points before the set point and a new "
       "contour will contain the set point and all the points "
       "after the set point.\n\n",
       "If both points are set the second contour will contain "
       "points between and including the two set points "
       " and the first contour "
       "will contain the remaining points.",
       NULL);
    break;
  }
}

// The window is closing, clean up and remove from manager
void ContourBreak::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)cobrk.dia);
  cobrk.dia = NULL;
  e->accept();
}


/****************************************************************************/
/*               CONTOUR JOINING                                            */
/***************************************************************************/

static struct contour_join_struct cojoin = {NULL, NULL, {-1, -1, -1}, 
                                            {-1, -1, -1}};

void imodContEditJoinOpen(ImodView *vw)
{
  if (cojoin.dia){
    cojoin.dia->raise();
    return;
  }
  cojoin.vw = vw;
  cojoin.i1 = vw->imod->cindex;
  cojoin.i2 = nullIndex;

  cojoin.dia = new ContourJoin(imodDialogManager.parent(IMOD_DIALOG), 
                               "contour join");

  cojoin.dia->show();
}


/*
 * THE CONTOUR JOIN CLASS IMPLEMENTATION 
 */

static char *joinTips[] = {"Join two contours at the selected points",
                           "Close dialog box", "Open help window"};

ContourJoin::ContourJoin(QWidget *parent, const char *name)
  : ContourFrame(parent, 3, applyDoneHelp, joinTips, name)
{
  diaLabel("Select contours to join:", this, mLayout);
  QGridLayout *grid = new QGridLayout(mLayout, 2, 2);

  mButton1 = new QPushButton("Set 1", this);
  grid->addWidget(mButton1, 0, 0);
  mButton1->setFocusPolicy(NoFocus);
  connect(mButton1, SIGNAL(clicked()), this, SLOT(set1Pressed()));
  QToolTip::add(mButton1, "Set join point in first contour");

  mSet1Label = new QLabel(" ", this);
  grid->addWidget(mSet1Label, 0, 1);

  mButton2 = new QPushButton("Set 2", this);
  grid->addWidget(mButton2, 1, 0);
  mButton2->setFocusPolicy(NoFocus);
  connect(mButton2, SIGNAL(clicked()), this, SLOT(set2Pressed()));
  QToolTip::add(mButton2, "Set join point in second contour");

  mSet2Label = new QLabel(" ", this);
  grid->addWidget(mSet2Label, 1, 1);

  setCaption(imodCaption("3dmod Join Contours"));
  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));

  setFontDependentWidths();
  setlabel(mSet1Label, cojoin.i1);
  setlabel(mSet2Label, cojoin.i2);
  show();
}

void ContourJoin::setFontDependentWidths()
{
  int width = diaSetButtonWidth(mButton1, mRoundedStyle, 1.2, "Set 2");
  mButton2->setFixedWidth(width);
}

void ContourJoin::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  setFontDependentWidths();
  DialogFrame::fontChange(oldFont);
}

// Set the current index for either set, but if the objects don't
// match, null out the other one
void ContourJoin::set1Pressed()
{
  cojoin.i1 = cojoin.vw->imod->cindex;
  setlabel(mSet1Label, cojoin.i1);
  if (cojoin.i2.object >= 0 && cojoin.i2.object != cojoin.i1.object) {
    cojoin.i2 = nullIndex;
    setlabel(mSet2Label, cojoin.i2);
  }
}

void ContourJoin::set2Pressed()
{
  cojoin.i2 = cojoin.vw->imod->cindex;
  setlabel(mSet2Label, cojoin.i2);
  if (cojoin.i1.object >= 0 && cojoin.i1.object != cojoin.i2.object) {
    cojoin.i1 = nullIndex;
    setlabel(mSet1Label, cojoin.i1);
  }
}

/*
 * The main join routine - can be called externally
 */
void imodContEditJoin(ImodView *vw)
{
  Iindex *i1p = &cojoin.i1;
  Iindex *i2p = &cojoin.i2;
  Iindex *indp;
  int ob, co, pt;
  Icont *cont1, *cont2, *jcont;
  Iobj *obj;


  /* DNM 2/12/01: put in initial test for a "None" selection */
  if (!(indexGood(*i1p) && indexGood(*i2p))) {

    // If two set points are not good, first see if two selection points
    // and take them; then if one point is set take the current point as 
    // the other point as long as it is a different contour
    if (ilistSize(vw->selectionList) == 2) {
      indp = (Iindex *)ilistItem(vw->selectionList, 0);
      *i1p = *indp;
      indp = (Iindex *)ilistItem(vw->selectionList, 1);
      *i2p = *indp;
    } else if (indexGood(*i1p) & i1p->contour != vw->imod->cindex.contour)
      *i2p = vw->imod->cindex;
    else if (indexGood(*i2p) & i2p->contour != vw->imod->cindex.contour)
      *i1p = vw->imod->cindex;
      
    if (!(indexGood(*i1p) && indexGood(*i2p))) {
      wprint("\a\nContour Join Error: Two contours not selected.\n");
      return;
    }
  }

  /*  DNM: separate objects are not allowed because the way joining is
      treated depends on object type.  Move this test up to top */
  if (i2p->object != i1p->object){
    wprint("\a\nContour Join Error:\n"
           "\tJoin contours must belong to the same object.\n");
    return;
  }

  /* DNM 2/12/01: test for not being the same contour */
  if (i2p->contour == i1p->contour){
    wprint("\a\nContour Join Error:\n"
           "\tSet points must be in different contours.\n");
    return;
  }

  /* DNM 2/12/01: test for object still valid */
  if (i2p->object >= vw->imod->objsize){
    wprint("\a\nContour Join Error:\n"
           "\tObject number no longer valid.\n");
    return;
  }

  imodGetIndex(vw->imod, &ob, &co, &pt);
  imodSetIndex(vw->imod, 
               i1p->object, i1p->contour, i1p->point);

  obj   = imodObjectGet(vw->imod);

  /* DNM 2/12/01: need to test that the contour numbers are still legal */
  if (i1p->contour >= obj->contsize || i2p->contour >= obj->contsize){
    wprint("\a\nContour Join Error:\n"
           "\tContour number is no longer valid.\n");
    imodSetIndex(vw->imod, ob, co, pt);
    return;
  }

  cont1 = imodContourGet(vw->imod);
  if (cont1) 
    vw->undo->contourDataChg();

  imodSetIndex(vw->imod, 
               i2p->object, i2p->contour, i2p->point);
  cont2 = imodContourGet(vw->imod);
  vw->undo->contourRemoval();

  /* DNM: consolidate the identical tests on cont1 and cont2 */
  if (!cont1 || !cont2){
    wprint("\a\nContour Join Error:\n"
           "\tInvalid Model data or no contour selected.\n");
    imodSetIndex(vw->imod, ob, co, pt);
    vw->undo->flushUnit();
    return;
  }

  /* DNM: make sure the set points are still legal */
  if (i1p->point >= cont1->psize || i2p->point >= cont2->psize){
    wprint("\a\nContour Join Error:\n"
           "\tContour has changed since point was set.\n");
    imodSetIndex(vw->imod, ob, co, pt);
    vw->undo->flushUnit();
    return;
  }
     
  if (cont1->label || cont2->label)
    if (dia_choice("Joining will destroy point labels; do you really "
                   "want to join?", "Yes", "No", NULL) != 1) {
      imodSetIndex(vw->imod, ob, co, pt);
      vw->undo->flushUnit();
      return;
    }

  pt = i1p->point;
  if (iobjScat(obj->flags)){
    jcont = imodContourSplice(cont1, cont2, cont1->psize - 1, 0);
          
  } else if (iobjOpen(obj->flags)){
    if (i2p->point > 0 && i2p->point == cont2->psize - 1 &&
        i1p->point == 0 && cont1->psize > 1) {
      jcont = imodContourSplice(cont2, cont1, i2p->point, 
                                i1p->point);
      pt = i2p->point;
    } else
      jcont = imodContourSplice(cont1, cont2, i1p->point,
                                i2p->point);
  }else{
    jcont = imodContourJoin(cont1, cont2, -1, -1, FALSE, 0);
  }

  if (cont1->psize)
    free(cont1->pts);
  if (cont1->sizes)
    free(cont1->sizes);
          
  /* copy the structure elements into place in the contour array */
  imodContourCopy(jcont, cont1);

  /* DNM: set index past point in first contour, adjust contour # if 
     needed */
  co = i1p->contour;
  if (i1p->object == i2p->object && 
      i1p->contour > i2p->contour)
    co--;
  if (pt + 1 < cont1->psize)
    pt++;

  /* check wild flag of new resulting contour */
  imodel_contour_check_wild(cont1);

  /* delete the second contour, which may shift cont1 down */
  imodDeleteContour(vw->imod);
     
  imodSetIndex(vw->imod, 
               i1p->object, co, pt);

  /* Clear out the labels */
  i1p->point = -1;
  i2p->point = -1;
  if (cojoin.dia) {
    setlabel(cojoin.dia->mSet1Label, cojoin.i1);
    setlabel(cojoin.dia->mSet2Label, cojoin.i2);
  }

  vw->undo->finishUnit();
  imodSelectionListClear(vw);
  imodDraw(vw, IMOD_DRAW_MOD);
  return;
}

// Action buttons
void ContourJoin::buttonPressed(int which)
{
  switch (which) {
  case 0:  // Apply
    imodContEditJoin(cojoin.vw);
    break;

  case 1:  // Done
    close();
    break;

  case 2: 
    dia_vasmsg
      ("Contour Join Help:\n\n"
       "Make a single contour from two separate contours and "
       "remove the old contours.\n",

       "There are two ways to select contours to join.  One way is to "
       "select a model point within the first contour and press the "
       "[Set 1] button, then select a point within the second contour and "
       "press [Set 2].  The latter step is actually superfluous, because if "
       "only one join point is set and another point is selected as the "
       "current point, that point will be used as the second point.  The "
       "second way is to select a point in the first contour, then select "
       "a point in the second contour with "CTRL_STRING" and the first mouse "
       "button.  If two contours are selected in this way, they will be the "
       "ones joined if one or no join points are set in the dialog box.\n\n",
        
       "The hot key J can be used in place of the [Apply] button.  It can "
       "also be used when the join dialog is not open, when two contours are "
       "selected.\n\n"
        
       "For open type objects, the new contour will contain "
       "points from the first contour up to [Set 1] and will "
       "contain points from the second contour starting at "
       "[Set 2] through the end of the contour.  The exception to "
       "this is when [Set 1] is at the START of one contour and "
       "[Set 2] is at the END of the other contour.  In this case "
       "the new contour will contain all of the points from the "
       "original two contours, rather "
       "than just two points.\n\n",

       "For closed type objects, the contours will be joined at "
       "the closest points.\n\n",
       "For scattered type objects, ALL of the points from the second"
       " contour will be appended to the first contour, regardless of"
       " which points are selected.  No points will be discarded.\n",

       NULL);
    break;
  }
}

// The window is closing, clean up and remove from manager
void ContourJoin::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)cojoin.dia);
  cojoin.dia = NULL;
  cojoin.i1 = nullIndex;
  cojoin.i2 = nullIndex;
  e->accept();
}


/*****************************************************************************/
/*  CONTOUR MOVING                                                           */
/*****************************************************************************/
/*
 * Move a contour to a different object.
 *
 */
static struct contour_move_struct comv;
static int movefirst = 1;

/* Open a dialog box */
void imodContEditMoveDialog(ImodView *vw, int moveSurf)
{
  if (movefirst){
    comv.dia = NULL;
    comv.wholeSurf = moveSurf;
    comv.movetosurf = 0;
    comv.surf_moveto = 0;
    comv.replace = 0;
    comv.expand = 0;
    comv.enabled = 1;
    comv.keepsize = 0;
    comv.moveUpDown = 0;
    comv.upOrDown = 0;
    movefirst = 0;
  }

  if (comv.dia){
    comv.dia->raise();
    return;
  }

  comv.vw = vw;
  comv.wholeSurf = moveSurf;

  comv.dia = new ContourMove(imodDialogManager.parent(IMOD_DIALOG), 
                             "contour move");

  imodContEditMoveDialogUpdate();
  comv.dia->show();
}

/* Update the dialog box based on the current object and settings */
void imodContEditMoveDialogUpdate(void)
{
  int min, max, val;
  Iobj *obj;

  if (movefirst != 0 || comv.dia == NULL)
    return;

  obj = imodObjectGet(App->cvi->imod);

  if (comv.movetosurf) {
    min = 0; 
    if (obj)  
      max = obj->surfsize; 
    else
      max = 0;
    if (comv.surf_moveto > max)
      comv.surf_moveto = max;
    val = comv.surf_moveto;
  } else {
    min = 1;
    max = App->cvi->imod->objsize;
    if (App->cvi->obj_moveto > max)
      App->cvi->obj_moveto = max;
    val = App->cvi->obj_moveto;
  }
  if (max <= min) {
    val = min;
    max = min + 1;
    comv.enabled = 0;
  } else {
    comv.enabled = 1;
  }
  
  comv.dia->mObjSpinBox->setEnabled(comv.enabled != 0);
  diaSetSpinMMVal(comv.dia->mObjSpinBox, min, max, val);

  comv.dia->manageCheckBoxes();
  return;
}

#ifndef ICON_ONLIST
#define ICONT_ONLIST ICONT_CONNECT_TOP
#endif

/* move the current contour in the model to the selected moveto object. */
void imodContEditMove(void)
{
  Iobj *obj, *tobj;
  Icont *cont , *newCont;
  ImodView *vi = App->cvi;
  Imod *imod = vi->imod;
  int surf, ob, co, pt;
  int nsurf;
  float firstz, size, delz;
  double weight;
  Ipoint ccent;
  int conew, ptnew;
  double rad, dz, delAng, circRad;
  int dzLim, iz, izCen, npts;
  float xCen, yCen, zCen, zscale;
  char *surfLabel = NULL;

  /* Check that user has set up the move operation. */
  if (movefirst){
    wprint("\aError: Select Edit->Contour->Move to setup move.\n");
    return;
  }

  if (!comv.enabled) {
    wprint("\aError: Must have move than one object or surface to "
           "be able to move contours.\n");
    return;
  }

  imodGetIndex(imod, &ob, &co, &pt);
  /* object to move current contour to. */
  tobj = &(imod->obj[vi->obj_moveto - 1]);

  /* Get current object and contour. */
  obj = imodObjectGet(imod);
  if (!obj)
    return;
  cont = imodContourGet(imod);
  if (!cont) 
    return;

  /* DNM 3/29/01: set up values for new contour and point */
  co = imod->cindex.contour;
  conew = co;
  ptnew = pt;

  /* REPLACE CONTOUR BY POINT IS FIRST */

  if (iobjScat(tobj->flags) && comv.replace && !comv.movetosurf && 
      !iobjScat(obj->flags) && !comv.moveUpDown) {
    if (cont->psize < 3) {
      wprint("\aError: Contour must have at least 3 points.\n");
      return;
    }
    firstz = cont->pts[0].z;
    for (pt = 1; pt < cont->psize; pt++)
      if (cont->pts[pt].z != firstz) {
        wprint("\aError: Contour not all in one plane.\n");
        return;
      }

    /* Get the centroid and the area, use area to set the size */

    imodel_contour_centroid(cont, &ccent, &weight);
    ccent.x /= weight;
    ccent.y /= weight;
    ccent.z = firstz;
    size = sqrt((double)(imodContourArea(cont)/3.14159)) * vi->xybin;

    // Create contour if none there
    if(!tobj->contsize) {
      vi->undo->contourAddition(vi->obj_moveto - 1, tobj->contsize);
      cont = imodContourNew();
      imodObjectAddContour(tobj, cont);
    }

    // Get contour, register changes and add point
    cont = &(tobj->cont[tobj->contsize - 1]);
    vi->undo->pointAddition(vi->obj_moveto - 1, tobj->contsize - 1, 
                            cont->psize);
    vi->undo->contourRemoval();
    imodPointAppend(cont, &ccent);
    imodPointSetSize(cont, cont->psize - 1, size);
    imodObjectRemoveContour(obj, co);

    /* DNM 3/29/01: drop back to previous contour and set no current point */
    conew = co - 1;
    ptnew = -1;

  } else if (!iobjScat(tobj->flags) && comv.expand && !comv.movetosurf && 
      iobjScat(obj->flags) && !comv.moveUpDown) {

    /* EXPAND A SCATTERED POINT INTO CONTOURS IS NEXT */
    if (pt < 0)
      return;

    rad = imodPointGetSize(obj, cont, pt) / vi->xybin;
    if (rad < 1.) {
      wprint("\aError: Point radius must be at least 1.\n");
      return;
    }

    // Get range of Z values and center values
    zscale = ((imod->zscale ?  imod->zscale : 1.) * vi->zbin) / 
              vi->xybin;
    dzLim = (int)(rad / zscale + 1.);
    xCen = cont->pts[pt].x;
    yCen = cont->pts[pt].y;
    zCen = cont->pts[pt].z;
    izCen = (int)floor(zCen + 0.5);
    nsurf = -1;
    vi->undo->objectPropChg(vi->obj_moveto - 1);

    // Loop on the potential Z slices, skipping if not far enough into sphere
    for (iz = izCen - dzLim; iz <= izCen + dzLim; iz++) {
      dz = (iz - zCen) * zscale;
      if (fabs(dz) >= rad)
        continue;
      circRad = sqrt(rad * rad - dz * dz);
      if (circRad < 1.)
        continue;
      newCont = imodContourNew();
      if (!newCont) {
        wprint("\aError getting memory for new contour.\n");
        vi->undo->flushUnit();
        return;
      }

      // Get number of points, angular increment, and add points to contour
      npts = (int)((2. * 3.14159 * circRad * vi->xybin) / imod->res);
      if (npts < 6)
        npts = 6;
      delAng = 2. * 3.14159 / npts;
      for (pt = 0; pt < npts; pt++) {
        ccent.x = xCen + (float)(circRad * cos(pt * delAng));
        ccent.y = yCen + (float)(circRad * sin(pt * delAng));
        ccent.z = iz;
        imodPointAppend(newCont, &ccent);
      }

      // Get a new surface number for destination object the first time - this
      // can be done before the contour is added to the object
      if (nsurf < 0) {
        imodel_contour_newsurf(tobj, newCont);
        nsurf = newCont->surf;
      } else
        newCont->surf = nsurf;

      vi->undo->contourAddition(vi->obj_moveto - 1, tobj->contsize);
      imodObjectAddContour(tobj, newCont);
      if (iz == izCen)
        conew = tobj->contsize - 1;
    }

    vi->undo->pointRemoval();
    imodPointDelete(cont, ptnew);
    ptnew = 0;
    imod->cindex.object = vi->obj_moveto - 1;
    obj = tobj;

  } else if (comv.movetosurf) {

    /* MOVE CONTOURS TO DIFFERENT SURFACE */
    if (comv.wholeSurf){
      /* Move all contours with the same surface number. */
      surf = cont->surf;
      for(co = 0; co < obj->contsize; co++){
        if (obj->cont[co].surf == surf) {
          vi->undo->contourPropChg(ob, co);
          obj->cont[co].surf = comv.surf_moveto;
        }
      }
    } else {

      // Move single (or multiple selected) contours
      if (ilistSize(vi->selectionList)) {
        for (co = 0; co < obj->contsize; co++)
          if (imodSelectionListQuery(vi, ob, co) > -2) {
            vi->undo->contourPropChg(ob, co);
            obj->cont[co].surf = comv.surf_moveto;
          }
      } else {
        vi->undo->contourPropChg();
        cont->surf = comv.surf_moveto;
      }
    }
    if (obj->surfsize < comv.surf_moveto)
      obj->surfsize = comv.surf_moveto;
    vi->undo->objectPropChg();
    imodObjectCleanSurf(obj);
    imodContEditMoveDialogUpdate();

  } else if (comv.moveUpDown) {

    /* MOVE CONTOURS UP OR DOWN A SECTION */
    /* Do current contour, all in surface if that is selected, or all 
       selected contours if whole surface not selected */
    delz = comv.upOrDown ? -1. : 1.;
    for (co = 0; co < obj->contsize; co++) {
      if (co == conew || (comv.wholeSurf && cont->surf == obj->cont[co].surf)
          || (!comv.wholeSurf && imodSelectionListQuery(vi, ob, co) > -2)) {
        vi->undo->contourDataChg(ob, co);
        for (pt = 0; pt < obj->cont[co].psize; pt++)
          obj->cont[co].pts[pt].z += delz;
      }
    }

    // Go to adjacent section to show the moved contours, then return
    if (comv.upOrDown)
      inputPrevz(vi);
    else
      inputNextz(vi);

    vi->undo->finishUnit();
    return;

  } else {

    /* MOVE CONTOURS TO OTHER OBJECT: REQUIRE A DIFFERENT OBJECT */
    if (tobj == obj) {
      wprint("\aError: Trying to move contour to object it"
             " is already in.\n");
      return;
    }
               
    if (comv.wholeSurf){
      /* MOVE ALL CONTOURS WITH THE SAME SURFACE NUMBER. */

      vi->undo->objectPropChg();
      vi->undo->objectPropChg(vi->obj_moveto - 1);

      /* Assign them the first free surface # in destination object */
      surf = cont->surf;
      nsurf = imodel_unused_surface(tobj);
      if (tobj->surfsize < nsurf)
        tobj->surfsize = nsurf;

      /* DNM 9/20/04: move label from one object to the other */
      if (obj->label)
        surfLabel = imodLabelItemGet(obj->label, surf);

      for (co = 0; co < obj->contsize; co++) {
        if (obj->cont[co].surf == surf){
          cont = &(obj->cont[co]);
          cont->surf = nsurf;

          /* Set all the sizes before moving, if it is a
             scattered object and button is set for this */
          if (iobjScat(obj->flags) && comv.keepsize) {
            vi->undo->contourDataChg(ob, co);
            for (pt = 0; pt < cont->psize; pt++)
              imodPointSetSize(cont, pt, imodPointGetSize (obj, cont, pt));
          }

          vi->undo->contourMove(ob, co, vi->obj_moveto - 1, tobj->contsize);
          imodObjectAddContour(tobj, cont);
          if (!imodObjectRemoveContour(obj, co))
            co--;

          /* DNM 3/29/01: if move any contours, better set
             contour number undefined */
          conew = -1;
          ptnew = -1;
        }
      }

      if (surfLabel) {
        if (!tobj->label)
          tobj->label = imodLabelNew();
        imodLabelItemAdd(tobj->label, surfLabel, nsurf);
      }
          
    }else{
      /* MOVING ONE CONTOUR (OR MULTIPLE SELECTED CONTOURS)
         just keep the surface number as is and
         Adjust surface # limit if necessary; fix point sizes if
         scattered points and that option selected */

      // Set flags for contours to do
      if (ilistSize(vi->selectionList)) {
        for (co = 0; co < obj->contsize; co++)
          if (imodSelectionListQuery(vi, ob, co) > -2)
            obj->cont[co].flags |= ICONT_ONLIST; 

      } else
        cont->flags |= ICONT_ONLIST;

      // Move all contours with flags set
      for (co = 0; co < obj->contsize; co++) {
        cont = &obj->cont[co];
        if (cont->flags & ICONT_ONLIST) {
          cont->flags &= ~ICONT_ONLIST;
          if (cont->surf > tobj->surfsize) {
            tobj->surfsize = cont->surf;
            vi->undo->objectPropChg(vi->obj_moveto - 1);
          }

          if (iobjScat(obj->flags) && comv.keepsize) {
            vi->undo->contourDataChg(ob, co);
            for (pt = 0; pt < cont->psize; pt++)
              imodPointSetSize(cont, pt, imodPointGetSize(obj, cont, pt));
          }

          vi->undo->contourMove(ob, co, vi->obj_moveto - 1, tobj->contsize);
          imodObjectAddContour(tobj, cont);
          imodObjectRemoveContour(obj, co);

          /* DNM 3/29/01: drop back to previous contour and set no 
             current point */
          conew = co - 1;
          ptnew = -1;
        }
      }
    }
  }
     
  /* DNM 3/29/01: manage the new contour and point numbers appropriately
     given possible change of contour number.  Switch from using 
     imodSetIndex because that function re-attaches to a point. */
  if (conew >= obj->contsize)
    conew = obj->contsize - 1;
  if (conew < 0 && obj->contsize > 0)
    conew = 1;
  if (conew > -1) {
    if (ptnew >= (int)obj->cont[conew].psize)
      ptnew = obj->cont[conew].psize - 1;
  } else {
    ptnew = -1;
  }
  imod->cindex.contour = conew;
  imod->cindex.point = ptnew;
  vi->undo->finishUnit();
  imodSelectionListClear(vi);

  return;
}

/*
 * THE CONTOUR MOVE CLASS IMPLEMENTATION 
 */

static char *moveTips[] = {"Move current contour to selected place",
                           "Close dialog box", "Open help window"};

ContourMove::ContourMove(QWidget *parent, const char *name)
  : ContourFrame(parent, 3, applyDoneHelp, moveTips, name)
{
  // Set up top line layout
  QHBoxLayout *layout = new QHBoxLayout(mLayout);
  mObjSurfLabel = new QLabel(comv.movetosurf ? "Surface to move contour to:" :
                         "Object to move contour to:", this);
  layout->addWidget(mObjSurfLabel);
  
  // The spin box for object to move to
  mObjSpinBox = new QSpinBox(this);
  layout->addWidget(mObjSpinBox);
  mObjSpinBox->setFocusPolicy(ClickFocus);
  QToolTip::add(mObjSpinBox, 
                "Select object or surface to move current contour to");
  connect(mObjSpinBox, SIGNAL(valueChanged(int)), this, 
          SLOT(objSelected(int)));

  // A simple empty box keeps the spin box from stretching
  QHBox *hSpacer = new QHBox(this);
  layout->addWidget(hSpacer);

  // Set up the check boxes.  Only the keep-size on needs to be initialized
  // The rest get set by update
  mMoveAllBox = diaCheckBox("Move all contours with same surface #", this, 
                         mLayout);
  connect(mMoveAllBox, SIGNAL(toggled(bool)), this, SLOT(surfToggled(bool)));
  QToolTip::add(mMoveAllBox, "Move entire current surface");
  diaSetChecked(mMoveAllBox, comv.wholeSurf);

  mToSurfBox = diaCheckBox("Move contour to different surface, not object",
                           this, mLayout);
  connect(mToSurfBox, SIGNAL(toggled(bool)), this, SLOT(toSurfToggled(bool)));
  QToolTip::add(mToSurfBox, "Move to new surface within current object");
  diaSetChecked(mToSurfBox, comv.movetosurf);

  mReplaceBox = diaCheckBox("Replace contour by single point of same size",
                            this, mLayout);
  connect(mReplaceBox, SIGNAL(toggled(bool)), this, 
          SLOT(replaceToggled(bool)));
  QToolTip::add(mReplaceBox, "Make scattered point with size based on"
                " mean radius of contour");
  diaSetChecked(mReplaceBox, comv.replace != 0);

  mExpandBox = diaCheckBox("Replace spherical point with circular contours",
                            this, mLayout);
  connect(mExpandBox, SIGNAL(toggled(bool)), this, 
          SLOT(expandToggled(bool)));
  QToolTip::add(mExpandBox, "Replace scattered point with circular contour on"
                " each section where the point appears");
  diaSetChecked(mExpandBox, comv.expand != 0);

  mKeepSizeBox = diaCheckBox("Preserve sizes of points with default size",
                           this, mLayout);
  connect(mKeepSizeBox, SIGNAL(toggled(bool)), this, 
          SLOT(keepSizeToggled(bool)));
  QToolTip::add(mKeepSizeBox, "Assign fixed size to all scattered points that"
                " are moved");
  diaSetChecked(mKeepSizeBox, comv.keepsize != 0);

  // Make layout for moving up/down, and invisible radio group
  layout = new QHBoxLayout(mLayout);
  mMoveUpDownBox = diaCheckBox("Move contour one section", this, layout);
  connect(mMoveUpDownBox, SIGNAL(toggled(bool)), this, 
          SLOT(moveUpDownToggled(bool)));
  QToolTip::add(mMoveUpDownBox, "Shift contour(s) up or down in Z, not "
                "between objects/surfaces");
  mUpDownGroup = new QButtonGroup(1, Qt::Horizontal, this);
  mUpDownGroup->hide();
  connect(mUpDownGroup, SIGNAL(clicked(int)), this, 
          SLOT(upDownSelected(int))); 

  mUpButton = diaRadioButton("Up", this);
  mUpDownGroup->insert(mUpButton);
  layout->addWidget(mUpButton);
  mDownButton = diaRadioButton("Down", this);
  mUpDownGroup->insert(mDownButton);
  layout->addWidget(mDownButton);
  diaSetGroup(mUpDownGroup, comv.upOrDown);

  QPushButton *button = diaPushButton("Shift contour with first mouse button",
                                     this, mLayout);
  connect(button, SIGNAL(clicked()), this, SLOT(shiftContClicked()));
  QToolTip::add(button, "Activate shifting of current contour with mouse in "
                "Zap window (hot key P)");

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));
  setCaption(imodCaption("3dmod Move Contour"));
}

/* Manage the five check box sensitivities to reflect the mutual exclusivity 
   of the various options */
void ContourMove::manageCheckBoxes()
{
  Iobj *obj;
  unsigned int destScat, curScat = 0;
  bool replaceable, replaceOn, expandable, expandOn, upDownAble;
  obj = imodObjectGet(comv.vw->imod);
  if (obj)
    curScat = iobjScat(obj->flags);
  destScat = iobjScat(comv.vw->imod->obj[comv.vw->obj_moveto - 1].flags);
  replaceable = destScat && !comv.movetosurf && !curScat && !comv.moveUpDown;
  replaceOn = comv.replace && replaceable;

  mReplaceBox->setEnabled(replaceable);

  expandable = !destScat && !comv.movetosurf && curScat && !comv.moveUpDown;
  expandOn = comv.expand && expandable;

  mExpandBox->setEnabled(expandable);

  mMoveAllBox->setEnabled(!replaceOn && !expandOn);
  mToSurfBox->setEnabled(!replaceOn && !expandOn && !comv.moveUpDown);

  upDownAble = !replaceOn && !expandOn && !comv.movetosurf;
  mMoveUpDownBox->setEnabled(upDownAble);
  mUpButton->setEnabled(upDownAble);
  mDownButton->setEnabled(upDownAble);

  mKeepSizeBox->setEnabled(curScat && !comv.movetosurf && !comv.moveUpDown);
}

/* Respond to state changes */
void ContourMove::surfToggled(bool state)
{
    comv.wholeSurf = state ? 1 : 0;
}

void ContourMove::toSurfToggled(bool state)
{
  comv.movetosurf = state ? 1 : 0;
  mObjSurfLabel->setText(state ? "Surface to move contour to:" :
                         "Object to move contour to:");
  imodContEditMoveDialogUpdate();
}

void ContourMove::replaceToggled(bool state)
{
  comv.replace = state ? 1 : 0;
  manageCheckBoxes();
}

void ContourMove::expandToggled(bool state)
{
  comv.expand = state ? 1 : 0;
  manageCheckBoxes();
}

void ContourMove::keepSizeToggled(bool state)
{
    comv.keepsize = state ? 1 : 0;
}

void ContourMove::objSelected(int value)
{
  setFocus();
  if (comv.movetosurf)
    comv.surf_moveto = value;
  else {
    App->cvi->obj_moveto = value;
    manageCheckBoxes();
  }
}

void ContourMove::moveUpDownToggled(bool state)
{
  comv.moveUpDown = state ? 1 : 0;
  manageCheckBoxes();
}

void ContourMove::upDownSelected(int which)
{
  comv.upOrDown = which;
}

void ContourMove::shiftContClicked()
{
  QKeyEvent *e = new QKeyEvent(QEvent::KeyPress, Qt::Key_P, 'P', 
                               ShiftButton);
  ivwControlKey(0, e);
}


// Respond to action buttons
void ContourMove::buttonPressed(int which)
{
  switch (which) {
  case 0:  // Apply
      if (comv.vw->obj_moveto > comv.vw->imod->objsize)
        comv.vw->obj_moveto = comv.vw->imod->objsize;
      if (comv.vw->obj_moveto < 1)
        comv.vw->obj_moveto = 1;
      
      imodContEditMove();
      
      imod_setxyzmouse();
      break;

  case 1:  // Done
    close();
    break;

  case 2: 
    dia_vasmsg
      ("Contour Move Help\n",
       "---------------------------\n",
       "This dialog is used to move contours to a different object or to a "
       "different surface, or to activate shifting a contour with the mouse. ",
       " Use the spin box to select the object or surface # to move "
       "the current contour to.  ",
       "3dmod remembers the object #, so the hot key 'M' in the ",
       "Zap window can be used to move contours quickly.\n\n",
       "Ordinarily, a single contour will be moved, the current contour.  "
       "If you have selected multiple contours using "CTRL_STRING" and the "
       "first mouse button, all selected contours will be moved to the other "
       "object or surface, unless you have checked the option to "
       "move all contours in a surface.\n\n"
       "When \"Move all contours with same surface #\" is selected, all "
       "contours with the same surface number as the current contour will "
       "also be moved to the selected object or surface.  If no surfaces have "
       "been assigned in the current object, then all contours in the object "
       "will be moved.\n\n",
       "When \"Move contour to different surface, not object\" is selected, "
       "contours are assigned to a different surface in the same object "
       "rather than moved to a new object.\n\n",
       "\"Replace contour by single point of same size\" can be "
       "used when the type of the selected target object is scattered "
       "points and the current object is not scattered points.  "
       "Moving a contour will convert the current "
       "contour into a single point centered on the contour and with "
       "a size that corresponds to the area of the contour.  The new "
       "point will be added to the end of the last contour in the "
       "target object.  The "
       "current contour will be deleted.\n\n",
       "\"Replace spherical point with circular contours\" can be "
       "used when the type of the current object is scattered points and the "
       "selected target object is not scattered points.  "
       "Moving will convert just the current point to a set of circular "
       "contours, one on each of the sections where the sphere of the current "
       "point appears.  The radius of the contours, and the range in Z over "
       "which they appear, will thus depend on the model's Z-scale, just as "
       "the appearance of a spherical point does.  The current point will be "
       "deleted.  The contours will be given a unique surface number in the "
       "target object so that they can be manipulated together or deleted "
       "easily.\n\n"
       "When the current object consists of scattered points, the "
       "\"Preserve sizes of points with default size\" button can be used to "
       "specify whether the sizes of "
       "the points will be completely preserved when they are "
       "transferred to the new object.  If the button is selected, "
       "points that do not have their own individual sizes will be "
       "assigned the default size of the current object before being "
       "moved to the new object.  If the button is not depressed, "
       "such points will not be assigned individual sizes, and they "
       "will acquire the default size of the object they are moved "
       "to.  In either case, points that do have individual sizes "
       "will retain these sizes after the transfer.\n\n",
       "When moving contours to a different object, the surface "
       "number that they have in their new home depends on whether "
       "the first toggle button is selected.  If it is not (i.e., "
       "you are moving one contour at a time), then each "
       "contour moved will have the same surface number as before, "
       "and no surfaces will be created if there are none already.\n"
       "If the button is selected (i.e., you are moving all contours "
       "with the same surface number), then the set of contours that "
       "get moved in one operation will be assigned to a new surface "
       "with the next free number in the new object.  If you want to "
       "move all of the contours in an object with no surfaces into "
       "a different object and have them occupy a separate surface "
       "in that object, select the option to move all contours with "
       "the same surface number.\n\n",
       "\"Move contour one section\" can be selected to shift contours up or "
       "down in Z, provided that other kinds of movements are not selected.  "
       "If \"Move all contours with same surface #\" is selected, then all "
       "contours in the surface will be moved.  Otherwise, all selected "
       "contours will be moved if there is more than one.  "
       "The current point and selected contours remain unchanged "
       "so it is easy to undo this operation by changing the direction.\n\n"
       "\"Shift contour with first mouse button\" allows you to shift the "
       "current contour in the active Zap window by dragging with the first "
       "mouse button held down.  After "
       "pressing this key, position the mouse anywhere, press the first mouse "
       "button, and shift the contour to the desired position.  Shifting mode "
       "is terminated when you release the mouse button.  Shifting "
       "works for any contours in closed contour objects and for coplanar "
       "contours in open contour objects.\n",

       NULL);
    break;
  }
}

void ContourMove::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::fontChange(oldFont);
}

// The window is closing, clean up and remove from manager
void ContourMove::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)comv.dia);
  comv.dia = NULL;
  e->accept();
}


/*****************************************************************************/
/*   Contour-Surface-Point Edit                                              */
/*    This one has a companion form class managed by designer                */

static struct contour_edit_struct surf = {NULL, NULL};

/* Open the dialog box */
void imodContEditSurf(ImodView *vw)
{
  if (surf.dia){
    surf.dia->raise();
    return;
  }
  surf.vw = vw;
     
  surf.dia = new ContSurfPoint(imodDialogManager.parent(IMOD_DIALOG), 
                               "surface edit",
                               Qt::WType_TopLevel | Qt::WDestructiveClose);
  imodContEditSurfShow();
  imodDialogManager.add((QWidget *)surf.dia, IMOD_DIALOG);

  surf.dia->show();
}

/*
 * Update the dialog box completely
 */
void imodContEditSurfShow(void)
{
  Icont *cont;
  Iobj  *obj;
  QString surfLabel, contLabel, ptLabel;
  char *plabel = NULL;
  int max, val, defval, noCont, noPoint, noSurf, closedOpen, enabled;
  float size;
  int pt;

  if (!surf.dia)
    return;

  obj  = imodObjectGet(surf.vw->imod);
  cont = imodContourGet(surf.vw->imod);

  /* Set state of surface */
  max = 0;
  val = -1;
  noSurf = 1;
  if (obj) 
    max = obj->surfsize;  /* surfsize is the maximum surface # in object */
  if (cont) { 
    val = cont->surf;
    noSurf = 0;
    if (obj->label)
      plabel =  imodLabelItemGet(obj->label, val);
    if (plabel)
      surfLabel = plabel;
  }
  surf.dia->setSurface(val, max);

  /* Show state of ghost drawing */
  surf.dia->setGhostState(surf.vw->ghostdist, surf.vw->ghostmode);

  /* show closed/open state; enable for closed objects */
  closedOpen = 0;
  enabled = 0;
  if (cont && iobjClose(obj->flags)) {
    closedOpen = cont->flags & ICONT_OPEN;
    enabled = iobjClose(obj->flags);
  }
  surf.dia->setClosedOpen(closedOpen, enabled);


  /* Show time data. Set maximum to max of vw->nt and model tmax*/
  val = -2;
  max = surf.vw->imod->tmax;
  if (max < surf.vw->nt)
    max = surf.vw->nt;
  if (iobjTime(obj->flags)){
    val = -1;
    if (cont)
      val = cont->type;
  }
  surf.dia->setTimeIndex(val, max);


  /* Set the contour and point labels if any */
  noCont = 1;
  noPoint = 1;
  plabel = NULL;
  if (cont){
    noCont = 0;
    if ((cont->label) && (cont->label->name))
      contLabel = cont->label->name;
    
    if (surf.vw->imod->cindex.point >= 0){
      noPoint = 0;
      if (cont->label)
        plabel = imodLabelItemGet(cont->label, surf.vw->imod->cindex.point);
      
      if (plabel)
        ptLabel = plabel;
    }
  }
  surf.dia->setLabels(surfLabel, noSurf, contLabel, noCont, ptLabel, noPoint);

  /* Set the point size */
  /* Send default as -1 if no point, 0 if actual size, or 1 if default size */
  pt = surf.vw->imod->cindex.point;
  size = 0.0;
  defval = -1;
  if (cont && (pt != -1)) {
    defval = cont->sizes && cont->sizes[pt] >= 0. ? 0 : 1;
    size = imodPointGetSize(obj, cont, pt);
  }
  surf.dia->setPointSize(size, defval);

}

// Go to a different surface
void iceSurfGoto(int target)
{
  Icont *cont;
  Iobj  *obj;
  int distmin = 1000000;
  int co, closest, dist;
      
  obj = imodObjectGet(surf.vw->imod);

  if (!obj || !obj->contsize)
    return;

  /* if target is next or previous surface, use the AdjacentSurface call */
  // But update the window in case contour didn't change
  cont = imodContourGet(surf.vw->imod);
  if (cont)
    if (cont->surf == target + 1 || cont->surf == target - 1) {
      inputAdjacentSurface(surf.vw, target - cont->surf);
      imodContEditSurfShow();
      return;
    }
     
  /* find the first contour with the closest surface number */
  for (co = 0; co < obj->contsize; co++) {
    dist = obj->cont[co].surf - target;
    if (dist < 0)
      dist = -dist;
    if (dist < distmin) {
      distmin = dist;
      closest = co;
      if (!dist)
        break;
    }
  }
     
  surf.vw->imod->cindex.contour = closest;

  /* if point index is too high or low, change it. */
  if (surf.vw->imod->cindex.point >= obj->cont[closest].psize)
    surf.vw->imod->cindex.point = obj->cont[closest].psize - 1;
  if (surf.vw->imod->cindex.point < 0 && obj->cont[closest].psize > 0)
    surf.vw->imod->cindex.point = 0;

  imod_setxyzmouse();

  imodContEditSurfShow();
  return;
}

// Go to next or previous contour in surface
void iceContInSurf(int direction)
{
  inputAdjacentContInSurf(surf.vw, direction);
}

// Start a new surface
void iceSurfNew()
{
  inputNewSurface(surf.vw);
}

// Record change in open/closed state
void iceClosedOpen(int state)
{
  Icont *cont = imodContourGet(surf.vw->imod);
  if (!cont)
    return;

  surf.vw->undo->contourPropChg();
  if (state)
    cont->flags |= ICONT_OPEN;
  else
    cont->flags &= ~ICONT_OPEN;
  surf.vw->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

// Record change in time index of this contour
void iceTimeChanged(int value)
{
  Icont *cont;
  Iobj  *obj;
     
  obj = imodObjectGet(surf.vw->imod);
  if ((obj) &&  (iobjTime(obj->flags))){
    cont = imodContourGet(surf.vw->imod);
    if (cont){
      surf.vw->undo->contourPropChg();
      cont->type = value;
      cont->flags |= ICONT_TYPEISTIME;
      surf.vw->undo->finishUnit();
    }
  }    
  imodContEditSurfShow();
  imodDraw(surf.vw, IMOD_DRAW_MOD);
}

// Record change in contour or point label
void iceLabelChanged(char *st, int contPoint)
{
  Iobj  *obj = imodObjectGet(surf.vw->imod);
  Icont *cont = imodContourGet(surf.vw->imod);

  // With no signal blocking upon setting, it was sending nulls, so skip just
  // in case that happens
  if (!cont || !st) 
    return;

  if (contPoint == 2) {
    surf.vw->undo->objectPropChg();
    if (!obj->label)
      obj->label = imodLabelNew();
    imodLabelItemAdd(obj->label, st, cont->surf);
  } else {
     
    surf.vw->undo->contourDataChg();
    if (!cont->label)
      cont->label = imodLabelNew();
    if (contPoint)
      imodLabelItemAdd(cont->label, st, surf.vw->imod->cindex.point);
    else
      imodLabelName(cont->label, st);
  }
  surf.vw->undo->finishUnit();
}

// Record change in point size by whatever means
void icePointSize(float size)
{
  Icont *cont;
  int pt = surf.vw->imod->cindex.point;
  cont = imodContourGet(surf.vw->imod);
  if (!cont || (pt == -1))
    return;

  surf.vw->undo->contourDataChg();
  imodPointSetSize(cont, pt, size);
  surf.vw->undo->finishUnit();
  imodDraw(surf.vw, IMOD_DRAW_MOD);
}

// A change in the ghost distance
void iceGhostInterval(int value)
{
  surf.vw->ghostdist = value;
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

// One of the ghost check boxes is toggled
void iceGhostToggled(int state, int flag)
{
  if (!state)
    surf.vw->ghostmode &= ~flag;
  else
    surf.vw->ghostmode |= flag;

  /* Save the state of the mode for section ghost, unless it's all turned
     off */
  if ((flag & IMOD_GHOST_SECTION) && 
      (surf.vw->ghostmode & IMOD_GHOST_SECTION))
    surf.vw->ghostlast = surf.vw->ghostmode;
     
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void iceShowHelp()
{
  dia_vasmsg
    ("Surface/Contour/Point Edit Help\n"
     "---------------------------\n\n",
                
     "Surface controls:\n",
     "------------------------\n",
     "Each contour has its own surface number.  "
     "The surface number can be used for subgrouping contours "
     "without having to create new objects.  The surface number "
     "of the current contour is displayed in the spin box.  To the right "
     "of the spin box is the maximum surface number in the current object "
     "(\"/ #\").  You can go to the first contour in the next or previous "
     "surface by clicking the up or down arrows of the spin box; or you can "
     "go to the first contour of any surface by entering a number "
     "in the box and pressing Enter.\n\n",
                
     "The Surface Up and Down arrows can be used to move to the "
     "first contour in the next or previous surface, respectively. "
     "Non-existent surfaces are skipped when you move between surfaces by "
     "either means.  If you enter the number of a non-existent surface, the "
     "program selects the nearest available surface.\n\n"

     "The \"Contour in surf\" Up and Down arrows can be used to "
     "step to the next and previous contours within the current "
     "surface.\n\n"

     "The \"New Surf\" button will start a new surface with the "
     "next free number and "
     "create an empty contour with that surface number.\n\n"

     "The \"Ghost\" toggle button may be used during model "
     "editing to highlight the current contour, along with all "
     "other contours with the same surface number. \n\n",

     "It is possible to have a label for each surface.  The \"Label\" text "
     "box shows the label of the current surface; simply enter or change "
     "text in this box.\n\n"

     "Contour controls:\n",
     "-----------------\n",
     "The \"Closed\" and \"Open\" radio buttons select between closed and "
     "open contours in an object defined as having closed contours.  "
     "The default is a closed contour where the last point is "
     "connected to the first.  ",
     "An open contour will not be connected between the "
     "last and first points, thus allowing a partially cut surface "
     "to be represented by an object containing both closed and "
     "open contours.  This setting has no effect for an object "
     "defined as having open or scattered point contours, so the buttons are "
     "disabled in that case.\n\n",

     "The time index of contours in 4-dimensional datasets can "
     "be changed with the Time Index spin box.  "
     "The box shows the time index of the current contour, which can be "
     "incremented or decremented with the spin box arrows, or changed by "
     "typing in a new number followed by Enter.  Contours with a non-zero "
     "time index display at only the relevant time; contours with a zero "
     "index display at all times.  This box is active only "
     "when the Time Data toggle is selected for the particular "
     "object in the Edit-Object-Type dialog.\n\n"

     "It is possible to have a label for each contour.  The \"Label\" text "
     "box shows the label of the current contour; simply enter or change "
     "text in this box.\n\n"

     "Point controls:\n",
     "-----------------\n",
     "It is possible to assign a size for each point.  "
     "Any point with an individual size will be displayed as a 3D sphere of "
     "this radius, even if the object type is not scattered points.  "
     "The units are pixels in the image file, i.e., unbinned "
     "pixels if images are loaded in binned.  Points without a size will still"
     " be displayed at "
     "the size specified for the object as a whole.  You can "
     "change a size either by typing a number into the text box "
     "(followed by Enter) or by using the slider.  If a point has "
     "no size, the text box displays global point size for the object and "
     "\"-Default\".\n\n",

     "Each individual point can also have its own label..  The \"Label\" text "
     "box shows the label of the current point; simply enter or change "
     "text in this box.\n\n"

     "Section ghost controls:\n",
     "-----------------\n",
     "These items may be used to control "
     "the section-to-section ghost display mode.  Use the spin box to set "
     "the maximum number of sections that contours will be displayed as "
     "ghosts on.  For example, with a value of 3, contours will be displayed "
     "as ghosts from up to 3 sections away.  If the \"Up\" "
     "box is checked, then contours will be displayed as ghosts on "
     "following sections; the \"Down\" check box will display contours "
     "as ghosts on previous sections.  The \"g\" hot key will "
     "toggle "
     "the combination selected by these boxes on and off.\n\n",
     "If \"Lighter\" box is checked, the ghost contours will be displayed "
     "in a lighter color, otherwise they will be drawn with a darker color.  "
     "If the \"All objects\" box is checked, ghosts will be drawn for "
     "all objects instead of just for the current object.\n\n",
     NULL);

  return;
}

void iceClosing()
{
  imodDialogManager.remove((QWidget *)surf.dia);
  surf.dia = NULL;
}


/*
 * IMPLEMENTATION OF THE LITTLE ContourFrame BASE CLASS
 */
ContourFrame::ContourFrame(QWidget *parent, int numButtons, char *labels[], 
                           char *tips[], const char *name)
  : DialogFrame(parent, numButtons, 1, labels, tips, true, 
                ImodPrefs->getRoundedStyle(), " ", " ", name)
{
  imodDialogManager.add((QWidget *)this, IMOD_DIALOG);
}

// Close on escape, pass on keys
void ContourFrame::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

void ContourFrame::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

/*

$Log$
Revision 4.15  2004/11/09 00:37:27  mast
Fixed escaping of quote in help

Revision 4.14  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.13  2004/11/04 17:02:41  mast
Changes for switching to shifting contour as a mode that is turned on

Revision 4.12  2004/11/01 23:36:44  mast
Added conversion of point to circles and uses of multiple selections

Revision 4.11  2004/09/21 20:18:21  mast
Added surface labels

Revision 4.10  2004/07/11 18:23:45  mast
Changes for more ghost flags

Revision 4.9  2004/01/22 19:12:43  mast
changed from pressed() to clicked() or accomodated change to actionClicked

Revision 4.8  2004/01/05 18:25:07  mast
Improved explanation of point display; made conversion from contour to
point multiply by the binning in X and Y.

Revision 4.7  2004/01/05 18:21:19  mast
Add explanation of point size being in unbinned size to help.

Revision 4.6  2003/04/28 04:02:05  mast
Fix help texton hot key

Revision 4.5  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.4  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.3  2003/03/26 06:30:56  mast
adjusting to font changes

Revision 4.2  2003/03/03 22:14:34  mast
cleanup

Revision 4.1  2003/02/10 20:28:59  mast
autox.cpp

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/23 19:57:06  mast
Qt version

Revision 3.1.2.1  2002/12/19 04:37:12  mast
Cleanup of unused global variables and defines

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/
