/*  
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
#include <qvbuttongroup.h>
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
void joinError(Iindex *indArray, char *message);

#ifndef ICONT_ONLIST
#define ICONT_ONLIST ICONT_CONNECT_TOP
#endif

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
  int       openType;
  int       closedType;
};

struct contour_break_struct{
  ImodView  *vw;
  ContourBreak *dia;
  Iindex    i1, i2;
};

enum {OPEN_TYPE_CONCAT = 0, OPEN_TYPE_SPLICE};
enum {CLOSED_TYPE_CONCAT = 0, CLOSED_TYPE_NEAREST, CLOSED_TYPE_AUTO,
      CLOSED_TYPE_SETPOINT};

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
    str.sprintf("Obj None, Cont None, Pt None ");
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
    str = "Pt None ";

  mSet1Label->setText(str);

  if (indexGood(cobrk.i2)) {
    str.sprintf("Point %d", cobrk.i2.point+1);
    ob = cobrk.i2.object;
    co = cobrk.i2.contour;
  } else
    str = "Pt None ";
  mSet2Label->setText(str);

  if (co < 0 || ob < 0)
    str = "Obj None, Cont None";
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

static struct contour_join_struct cojoin = 
  {NULL, NULL, {-1, -1, -1}, {-1, -1, -1}, OPEN_TYPE_CONCAT, CLOSED_TYPE_AUTO};

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
  QRadioButton *radio;
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

  QHBoxLayout *hbox = new QHBoxLayout(mLayout);
  mClosedGroup = new QVButtonGroup("Closed Object", this, "closed group");
  hbox->addWidget(mClosedGroup);
  connect(mClosedGroup, SIGNAL(clicked(int)), this, 
          SLOT(closedTypeSelected(int)));
  mClosedGroup->setInsideSpacing(0);
  mClosedGroup->setInsideMargin(5);

  radio = diaRadioButton("Concatenate", mClosedGroup);
  QToolTip::add(radio, "Add points from one contour to end of other");
  radio = diaRadioButton("Join near pts", mClosedGroup);
  QToolTip::add(radio, "Add line connecting contours between their "
                "nearest points");
  radio = diaRadioButton("Auto-choose", mClosedGroup);
  QToolTip::add(radio, "Concatenate if contour openings are bigger than"
                " distance between contours");
  radio = diaRadioButton("Join set pts", mClosedGroup);
  QToolTip::add(radio, "Add line connecting contours between the set points");

  diaSetGroup(mClosedGroup, cojoin.closedType);

  mOpenGroup = new QVButtonGroup("Open Object", this, "open group");
  hbox->addWidget(mOpenGroup);
  connect(mOpenGroup, SIGNAL(clicked(int)), this, SLOT(openTypeSelected(int)));
  mOpenGroup->setInsideSpacing(0);
  mOpenGroup->setInsideMargin(5);

  radio = diaRadioButton("Concatenate", mOpenGroup);
  QToolTip::add(radio, "Add points from one contour to end of other");
  radio = diaRadioButton("Splice set pts", mOpenGroup);
  QToolTip::add(radio, "Retain points up to set point 1 and after set "
                "point 2");
  diaSetGroup(mOpenGroup, cojoin.openType);

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

void ContourJoin::openTypeSelected(int which)
{
  cojoin.openType = which;
}

void ContourJoin::closedTypeSelected(int which)
{
  cojoin.closedType = which;
}

void joinError(Iindex *indArray, char *message)
{
  if (indArray)
    free(indArray);
  wprint("\aContour Join Error:  %s\n", message);
  return;
}

/*
 * The main join routine - can be called externally
 */
void imodContEditJoin(ImodView *vw)
{
  Iindex *i1p = &cojoin.i1;
  Iindex *i2p = &cojoin.i2;
  Iindex *indp;
  Iindex *indArray;
  int co, pt1, pt2, ind1, ind2, tpt1, tpt2, end1, end2, concat, testConcat;
  int i, j, numJoin, ijoin, ic1, ic2;
  float dist, distMin, tdist;
  Icont *cont1, *cont2, *jcont;
  Imod *imod = vw->imod;
  Iobj *obj;
  Ipoint scale = {1., 1., 100.};
  bool useSetPoints;
  int iflabel = 0;

  // Get an array for the indices
  numJoin = B3DMAX(2, ilistSize(vw->selectionList));
  indArray = (Iindex *)malloc(numJoin * sizeof(Iindex));
  if (!indArray) {
    joinError(indArray, "Could not get memory for indexes");
    return;
  }

  // Copy multiple selection to indices if >= 2
  if (ilistSize(vw->selectionList) >= 2) {
    for (co = 0; co < numJoin; co++) {
      indp = (Iindex *)ilistItem(vw->selectionList, co);
      indArray[co] = *indp;
    }
    
    // Or, if at least one set point is good, set them into array, using
    // current point as fallback
  } else if (indexGood(*i1p) && indexGood(*i2p)) {
    indArray[0] = *i1p;
    indArray[1] = *i2p;
  } else if (indexGood(*i1p) && i1p->contour != imod->cindex.contour) {
    indArray[0] = *i1p;
    indArray[1] = imod->cindex;
  } else if (indexGood(*i2p) && i2p->contour != imod->cindex.contour) {
    indArray[0] = imod->cindex;
    indArray[1] = *i2p;
  } else {
    joinError(indArray, "Two contours not selected.");
    return;
  } 

  if (indArray[0].object >= imod->objsize) {
    joinError(indArray, "Object number no longer valid.");
    return;
  }

  // Determine whether set points are to be used
  obj = &imod->obj[indArray[0].object];
  useSetPoints = (iobjOpen(obj->flags) && cojoin.openType == OPEN_TYPE_SPLICE)
    || (iobjClose(obj->flags) && cojoin.closedType == CLOSED_TYPE_SETPOINT);

  if (useSetPoints && numJoin > 2) {
    joinError(indArray, "Cannot join more than two contours by set points.");
    return;
  }

  
  // Check validity of all objects, contours, points if using set points
  for (co = 0; co < numJoin; co++) {
    indp = &indArray[co];
    if (indp->object != indArray[0].object) {
      joinError(indArray, "Join contours must belong to the same object.");
      return;
    }
    if (co && indp->contour == indArray[0].contour) {
      joinError(indArray, "Set points must be in different contours.");
      return;
    }
    if (indp->contour < 0 || indp->contour >= obj->contsize) {
      joinError(indArray, "Contour number is no longer valid.");
      return;
    }
    cont1 = &obj->cont[indp->contour];
    if (useSetPoints && indp->point < 0 || indp->point >= cont1->psize) {
      joinError(indArray, "Point number is no longer valid.");
      return;
    }
    if (cont1->label)
      iflabel = 1;
  }

  // Confirm label deletion if there are any
  if (iflabel)
    if (dia_choice("Joining will destroy point and contour labels; do you "
                   "really want to join?", "Yes", "No", NULL) != 1) {
      free(indArray);
      return;
    }

  i1p->object = indArray[0].object;

  // Loop on join of pair of contours
  for (ijoin = 0; ijoin < numJoin - 1; ijoin++) {
    concat = 0;

    // For scattered contours or one pair, just set up pair with zero
    if (iobjScat(obj->flags) || useSetPoints) {
      ind1 = 0;
      ind2 = ijoin + 1;
      pt1 = indArray[0].point;
      pt2 = indArray[ind2].point;
    } else {

      distMin = 1.e30;

      // Loop on pairs of contours in list
      for (ic1 = 0; ic1 < numJoin - 1; ic1++) {
        if (indArray[ic1].object < 0)
          continue;
        for (ic2 = ic1 + 1; ic2 < numJoin; ic2++) {
          if (indArray[ic2].object < 0)
            continue;

          // Given a valid pair, first test for concatenation distance if open
          // or closed and not set for nearest
          cont1 = &obj->cont[indArray[ic1].contour];
          cont2 = &obj->cont[indArray[ic2].contour];
          testConcat = 0;
          if (iobjOpen(obj->flags) || 
              cojoin.closedType != CLOSED_TYPE_NEAREST) {
            testConcat = 1;
            tpt1 = 0;
            tpt2 = 0;
            end1 = cont1->psize - 1;
            end2 = cont2->psize - 1;
            dist = imodPoint3DScaleDistance(&cont1->pts[0], &cont2->pts[0],
                                             &scale);
            tdist = imodPoint3DScaleDistance(&cont1->pts[0], &cont2->pts[end2],
                                             &scale);
            if (tdist < dist) {
              dist = tdist;
              tpt2 = end2;
            }
            tdist = imodPoint3DScaleDistance(&cont1->pts[end1], &cont2->pts[0],
                                             &scale);
            if (tdist < dist) {
              dist = tdist;
              tpt1 = end1;
              tpt2 = 0;
            }
            tdist = imodPoint3DScaleDistance(&cont1->pts[end1],
                                             &cont2->pts[end2], &scale);
            if (tdist < dist) {
              dist = tdist;
              tpt1 = end1;
              tpt2 = end2;
            }

            // If auto choosing, now compare this to the openings of the two
            // contours; if either is bigger, set for connect
            if (iobjClose(obj->flags) && 
                cojoin.closedType == CLOSED_TYPE_AUTO && 
                (imodPointDistance(&cont1->pts[0], &cont1->pts[end1]) < dist ||
                 imodPointDistance(&cont2->pts[0], &cont2->pts[end1]) < dist))
              testConcat = 0;
          }

          // If not concantenating, just get nearest distance between contours
          if (!testConcat) {
            dist = 1.e30;
            for (i = 0; i < cont1->psize; i++) {
              for (j = 0; j < cont1->psize; j++) {
                tdist = imodPointDistance(&cont1->pts[i], &cont2->pts[j]);
                if (dist > tdist) {
                  dist = tdist;
                  tpt1 = i;
                  tpt2 = j;
                }
              }
            }
          }

          // Record character of contour pair with current minimum distance
          if (distMin > dist) {
            distMin = dist;
            pt1 = tpt1;
            pt2 = tpt2;
            ind1 = ic1;
            ind2 = ic2;
            concat = testConcat;
          }
        }
      }
    }
    
    // If concatenating and both need inversion, swap them
    if (concat && !pt1 && pt2) {
      ic1 = ind1;
      ind1 = ind2;
      ind2 = ic1;
      pt1 = obj->cont[indArray[ind1].contour].psize - 1;
      pt2 = 0;
    }

    cont1 = &obj->cont[indArray[ind1].contour];
    cont2 = &obj->cont[indArray[ind2].contour];
    end1 = cont1->psize - 1;

    // If concatenating, invert each one if needed
    if (concat) {
      if (pt1 != end1) {
        vw->undo->contourDataChg(i1p->object, indArray[ind1].contour);
        imodel_contour_invert(cont1);
        pt1 = end1;
      }
      if (pt2) {
        vw->undo->contourDataChg(i1p->object, indArray[ind2].contour);
        imodel_contour_invert(cont2);
        pt2 = 0;
      }
    }

    // Do the right join operation
    if (iobjScat(obj->flags)) {
      pt1 = end1;
      pt2 = 0;
    }

    if (iobjClose(obj->flags) && !concat) 
      jcont = imodContourJoin(cont1, cont2, pt1, pt2, FALSE, 0);
    else
      jcont = imodContourSplice(cont1, cont2, pt1, pt2);

    if (!jcont) {
      joinError(indArray, "Failed to get memory for joined contour");
      vw->undo->finishUnit();
      return;
    }

    // Clear out the data in contour 1 and copy the new pointers
    vw->undo->contourDataChg(i1p->object, indArray[ind1].contour);
    if (cont1->psize)
      free(cont1->pts);
    if (cont1->sizes)
      free(cont1->sizes);
    imodLabelDelete(cont1->label);
    cont1->label = NULL;
    cont1->flags |= jcont->flags;
    cont1->pts = jcont->pts;
    cont1->sizes = jcont->sizes;
    cont1->psize = jcont->psize;
    free(jcont);

    // Mark contour 2 as done and flag it for removal
    indArray[ind2].object = -1;
    cont2->flags |= ICONT_ONLIST;
  }

  // check wild flag of new resulting contour, set up new current point
  imodel_contour_check_wild(cont1);
  co = indArray[ind1].contour;
  if (pt1 + 2 < cont1->psize)
    pt1++;
  
  // Remove all contours, shift index of remaining one down if needed
  for (i = obj->contsize - 1; i >= 0; i--) {
    cont1 = &obj->cont[i];
    if (cont1->flags & ICONT_ONLIST) {
      cont1->flags &= ~ICONT_ONLIST;
      vw->undo->contourRemoval(i1p->object, i);
      imodObjectRemoveContour(obj, i);
      if (i < co)
        co--;
    }
  }

  imodSetIndex(imod, i1p->object, co, pt1);

  // Clear out the labels
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
    imodShowHelpPage("contourJoin.html");
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
 * Move a contour to a different object/surface/Z section
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
  int conew, ptnew, ob2;
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
    for (ob2 = 0; ob2 < imod->objsize; ob2++) {
      obj = &imod->obj[ob2];
      for (co = 0; co < obj->contsize; co++) {
        if ((ob2 == ob && 
             (co == conew || 
              (comv.wholeSurf && cont->surf == obj->cont[co].surf)))
            || (!comv.wholeSurf && imodSelectionListQuery(vi, ob2, co) > -2)) {
          vi->undo->contourDataChg(ob2, co);
          for (pt = 0; pt < obj->cont[co].psize; pt++)
            obj->cont[co].pts[pt].z += delz;
        }
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
  mLayout->setSpacing(2);

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

  QPushButton *button = diaPushButton("Toggle adjusting contour with mouse",
                                     this, mLayout);
  connect(button, SIGNAL(clicked()), this, SLOT(shiftContClicked()));
  QToolTip::add(button, "Toggle shifting, rotating, or scaling of current"
                " contour with mouse in Zap window (hot key P)");

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
    imodShowHelpPage("contourMove.html");
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
Revision 4.18  2005/02/09 01:10:02  mast
Adjusted text for contour shifting, migrated help for contour Move

Revision 4.17  2004/11/24 05:08:19  mast
Implemented multiple contour join and control over method of joining

Revision 4.16  2004/11/20 05:05:27  mast
Changes for undo/redo capability

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
