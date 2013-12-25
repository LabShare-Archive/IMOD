/*
 *  cont_copy.cpp -- Contour copy dialog.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <math.h>

#include <qspinbox.h>
#include <qtooltip.h>
#include <qlayout.h>
//Added by qt3to4:
#include <QCloseEvent>
#include <QKeyEvent>
#include "dia_qtutils.h"
#include <QHBoxLayout>
#include <qcombobox.h>
#include <qgroupbox.h>
#include <qbuttongroup.h>
#include <qradiobutton.h>
#include "cont_copy.h"
#include "imod.h"
#include "display.h"
#include "imod_edit.h"
#include "control.h"
#include "preferences.h"
#include "undoredo.h"
#include "vertexbuffer.h"

/* These have to be maintained as indexes for the combo box */
#define COPY_TO_OBJECT     0
#define COPY_TO_SECTION    1
#define COPY_TO_NEXT_SECTION 2
#define COPY_TO_PREV_SECTION 3
#define COPY_TO_CURRENT    4
#define COPY_TO_TIME       5

#define COPY_TO_NEXT_TIME    6

static int copyContour(Icont *cont, int coNum);
static int contCompare(Icont *c1, Icont *c2);
static int contRmDup(Icont *c1, Icont *c2);


static struct
{
  ContourCopy   *dia;
  ImodView      *vw;        /* image data to model                       */

  /* Flags */
  int doSurface;    
  int doAll;
  int doAllObj;

  /* Copy to information. */
  int copyOperation;
  int surfaceNumber;
  int objectNumber;
  int sectionNumber;
  int timeIndex;

  /* Copy from information. */
  int   currentTime;
  int   currentSection;    /* DNM 11/30/02: change from float to int */
  int   currentObject;
}

sData = 
  { 
    NULL, NULL , 
    0, 0, 0, 
    0, -1, 0, -1, 0, 0, 0, 0
  };


/* These two functions support 4D scope data. */
static int contCompare(Icont *c1, Icont *c2)
{
  int dif = 0;
  int pt;

  if ((!c1) || (!c2)) return -1;
  if (c1->time != c2->time) dif++;
  if (c1->surf != c2->surf) dif++;
  if (c1->flags != c2->flags) dif++;
  if (c1->psize != c2->psize) dif++;
  if (dif) return dif;
  dif = 7;
  for(pt = 0; pt < c1->psize; pt++){
	if (c1->pts[pt].x != c2->pts[pt].x) dif++;
	if (c1->pts[pt].y != c2->pts[pt].y) dif++;
	if (c1->pts[pt].z != c2->pts[pt].z) dif++;
  }
  return dif;
}
/* rm points in c2 that are in c1 */
static int contRmDup(Icont *c1, Icont *c2)
{
  int delpts = 0;
  int pt1,pt2;
  if ((!c1)||(!c2)||(!c1->psize)||(!c2->psize)) return delpts;
  for(pt1 = 0; pt1 < c1->psize; pt1++)
	for(pt2 = 0; pt2 < c2->psize; pt2++){
      if ((c1->pts[pt1].x == c2->pts[pt2].x) &&
          (c1->pts[pt1].y == c2->pts[pt2].y) &&
          (c1->pts[pt1].z == c2->pts[pt2].z)){
		imodPointDelete(c2, pt2); 
		delpts++;
		pt2--;
      }
	}
  return delpts;
}

/*
 * Copy a single contour, cont, to the place it needs to go.
 *
 */
static int copyContour(Icont *cont, int coNum)
{
  Iobj *toObj;
  int co,pt;
  int section;
  ImodView *vw = sData.vw;
  int obnum = vw->imod->cindex.object;

  if (!cont) return(-1);
  if (!cont->psize) return(-1);

  switch(sData.copyOperation){

  case COPY_TO_OBJECT:
    obnum = sData.objectNumber - 1;
    toObj = &vw->imod->obj[obnum];

    /* Don't copy if duplicate contour already exists. */
    for(co = 0; co < toObj->contsize; co++){
      if (contCompare(&toObj->cont[co], cont) == 0)
        return(0);
    }
    vbCleanupVBD(toObj);

    /* Remove duplicate points */
    if (iobjScat(toObj->flags)){
      for(co = 0; co < toObj->contsize; co++){
        contRmDup(&toObj->cont[co], cont);
      }
    }
    vw->undo->contourAddition(obnum, toObj->contsize);
    imodObjectAddContour(toObj, cont);
    break;

  case COPY_TO_CURRENT:
    toObj = imodObjectGet(vw->imod);
    vw->undo->contourAddition(obnum, toObj->contsize);
    imodObjectAddContour(toObj, cont);
    break;

  case COPY_TO_SECTION:
  case COPY_TO_NEXT_SECTION:
  case COPY_TO_PREV_SECTION:
    toObj   = &vw->imod->obj[sData.currentObject];
    section = sData.sectionNumber-1; 
    for(pt = 0; pt < cont->psize; pt++){
      cont->pts[pt].z = section;
    }
    vw->undo->contourAddition(sData.currentObject, toObj->contsize);
    imodObjectAddContour(toObj, cont);
    break;

  case COPY_TO_TIME:
  case COPY_TO_NEXT_TIME:
    toObj   = &vw->imod->obj[sData.currentObject];
    cont->time = sData.timeIndex;
    vw->undo->contourAddition(sData.currentObject, toObj->contsize);
    imodObjectAddContour(toObj, cont);
    break;
  }

  // Copy any contour properties from source to destination object
  if (istoreCountContSurfItems(vw->imod->obj[sData.currentObject].store, 
                               coNum, 0)) {
    vw->undo->objectPropChg(obnum);
    istoreCopyContSurfItems(vw->imod->obj[sData.currentObject].store, 
                             &toObj->store, coNum, toObj->contsize - 1, 0);
  }

  return(0);
}


int openContourCopyDialog(ImodView *vw)
{
     
  if (sData.dia){
    sData.dia->raise();
    return(0);
  }
     
  sData.vw = vw;
  sData.dia = new ContourCopy(imodDialogManager.parent(IMOD_DIALOG), 
                                   "contour join");

  if (!sData.dia) return(1);

  imodDialogManager.add((QWidget *)sData.dia, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)sData.dia, IMOD_DIALOG);
  return(0);
}

// Entry for hot key copying
void iccCopyContour(void)
{
  if (!sData.dia)
    wprint("\aContour Copy dialog must be open to copy with this hot key.\n");
  else
    sData.dia->apply();
}

// External entry for updating
void imodContCopyUpdate(void)
{
  if (sData.dia)
    sData.dia->update();
}

/****************************************************************************/


static const char *buttonLabels[] = {"Apply", "Done", "Help"};
static const char *buttonTips[] = {"Copy the selected contours (hot key K)",
                             "Close dialog box", "Open help box"};

ContourCopy::ContourCopy(QWidget *parent, const char *name)
  : DialogFrame(parent, 3, 1, buttonLabels, buttonTips, true, 
                ImodPrefs->getRoundedStyle(), " ", "", name)
{
  QRadioButton *radio;
  QHBoxLayout *hBox = diaHBoxLayout(mLayout);
  
  mToCombo = new QComboBox(this);
  hBox->addWidget(mToCombo);
  mToCombo->addItem("Copy to Object #");
  mToCombo->addItem("Copy to Section #");
  mToCombo->addItem("Copy to Next Section");
  mToCombo->addItem("Copy to Prev Section");
  mToCombo->addItem("Duplicate");
  if (sData.vw->numTimes)
    mToCombo->addItem("Copy to Time Index #");
  mToCombo->setFocusPolicy(Qt::NoFocus);
  mToCombo->setCurrentIndex(sData.copyOperation);
  mToCombo->setToolTip("Select type of place to copy contours to");
  connect(mToCombo, SIGNAL(currentIndexChanged(int)), this,
          SLOT(placeSelected(int)));

  mToSpinBox = new QSpinBox(this);
  hBox->addWidget(mToSpinBox);
  mToSpinBox->setFocusPolicy(Qt::ClickFocus);
  mToSpinBox->setKeyboardTracking(false);
  mToSpinBox->setToolTip("Set object, section, or time");
  connect(mToSpinBox, SIGNAL(valueChanged(int)), this, 
          SLOT(toValueChanged(int)));
  
  QGroupBox *gbox = new QGroupBox("Copy", this);
  QVBoxLayout *gbLayout = new QVBoxLayout(gbox);
  mRadioGroup = new QButtonGroup(this);
  mLayout->addWidget(gbox);
  connect(mRadioGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(rangeSelected(int)));

  radio = diaRadioButton("Just the current contour(s)", gbox, mRadioGroup, gbLayout, 0,
                         "Copy only the selected contour(s) in the current object");
  radio = diaRadioButton
    ("All contours in surface", gbox, mRadioGroup, gbLayout, 1, 
     "Copy contours with same surface number as current one");
  radio = diaRadioButton("All contours in object", gbox, mRadioGroup, 
                         gbLayout, 2, "Copy all eligible contours in object");

  if (sData.vw->numTimes) {
    mTimeRadio = diaRadioButton
      ("All contours in all objects", gbox, mRadioGroup, 
       gbLayout, 3, "Copy all contours at this time to selected time");
  }

  // Figure out initial radio button settings, enforce flag settings and
  // set the button
  int radioVal = 0;
  if (sData.doSurface)
    radioVal = 1;
  if (sData.doAll)
    radioVal = 2;
  if (sData.vw->numTimes && sData.doAllObj)
    radioVal = 3;
  rangeSelected(radioVal);
  diaSetGroup(mRadioGroup, radioVal);

  update();

  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
  setWindowTitle(imodCaption("3dmod Copy Contours"));
}

// Operation is passed directly when selected
void ContourCopy::placeSelected(int which)
{
  sData.copyOperation = which;
  update();
}

void ContourCopy::toValueChanged(int value)
{
  setFocus();
  switch (sData.copyOperation) {
  case COPY_TO_OBJECT:
    sData.objectNumber = value;
    break;
  case COPY_TO_SECTION:
    sData.sectionNumber = value;
    break;
  case COPY_TO_TIME:
    sData.timeIndex = value;
    break;
  default:
    break;
  }
}

// Set flags from radio button value
void ContourCopy::rangeSelected(int which)
{
  sData.doSurface = (which == 1) ? 1 : 0;
  sData.doAll = (which == 2) ? 1 : 0;
  sData.doAllObj = (which == 3) ? 1 : 0;
}


void ContourCopy::update()
{
  int minVal, maxVal, curVal;
  ImodView *vw = sData.vw;

  // For each operation that uses spin box, set up min and max value
  // and adjust the current value if necessary
  switch(sData.copyOperation){
  case COPY_TO_OBJECT:
    minVal = 1;
    maxVal = vw->imod->objsize;
    sData.objectNumber = B3DMAX(1, B3DMIN(maxVal, sData.objectNumber));
    curVal = sData.objectNumber;
    break;

  case COPY_TO_SECTION:
    minVal = 1;
    maxVal = vw->zsize;
    sData.sectionNumber = B3DMAX(1, B3DMIN(maxVal, sData.sectionNumber));
    curVal = sData.sectionNumber;
    break;

  case COPY_TO_TIME:
    minVal = 1;
    maxVal = vw->numTimes;
    sData.timeIndex = B3DMAX(1, B3DMIN(maxVal, sData.timeIndex));
    curVal = sData.timeIndex;
    break;

  case COPY_TO_NEXT_SECTION:
  case COPY_TO_PREV_SECTION:
  case COPY_TO_CURRENT:
  default:
    minVal = 0;
    maxVal = 0;
    curVal = 0;
    break;
  }

  // Set up spin box limits and value and enable it if meaningful
  diaSetSpinMMVal(mToSpinBox, minVal, maxVal, curVal);
  mToSpinBox->setSpecialValueText(minVal ? "" : "   ");
  mToSpinBox->setEnabled(minVal > 0);

  // Enable the time radio button if appropriate
  if (vw->numTimes)
    mTimeRadio->setEnabled(sData.copyOperation == COPY_TO_TIME);

}

void ContourCopy::apply()
{
  const char *badCopy = "Copy operation cancelled.  ";
  /*     char *badObjectErrorMsg = 
         "\nCopy operation cancelled.\n"
         "Object out of range or invalid\n.";
  */

  Imod *imod = sData.vw->imod;
  Iobj *obj   = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  Icont *ncont;
  bool doingSection = false;
  int ob, co, errcode, maxcont;

  if (!obj){
    wprint("\a%sBad input object.\n",badCopy);
    return;
  }

  /* DNM: check validity of current contour: here test on all the
     conditions where a current contour is not needed */
  if (!((sData.doAll || sData.doAllObj) && 
        (sData.copyOperation == COPY_TO_OBJECT ||
         sData.copyOperation == COPY_TO_CURRENT || 
         sData.copyOperation == COPY_TO_TIME))) {
    if ((!cont) || (cont->psize <= 0)){
      wprint("\a%sBad input contour.\n", badCopy);
      return;
    }
    
    /* Set surface number here since we know we have a contour */
    sData.surfaceNumber = cont->surf;
  }

  /* check copy to place is valid. */
  switch(sData.copyOperation){
  case COPY_TO_OBJECT:
    if ((sData.objectNumber < 1) ||
        (sData.objectNumber > (int)imod->objsize) ||
        (sData.objectNumber == imod->cindex.object + 1)){
      wprint("\a%sBad destination object.\n", badCopy);
      return;
    }
    break;

  case COPY_TO_CURRENT:
    break;

  case COPY_TO_SECTION:
    /* get section number to copy from.*/
    sData.currentSection = (int)floor(cont->pts->z + 0.5);

    /* check section number to copy to. */
    if ((sData.sectionNumber <= 0) || 
        ( sData.sectionNumber > sData.vw->zsize ) ||
        (sData.currentSection + 1 == sData.sectionNumber)){
      wprint("\a%sBad destination section.\n", badCopy);
      return;
    }
    doingSection = true;
    break;

  case COPY_TO_TIME:
    sData.currentTime = cont->time;
    if ((sData.timeIndex > sData.vw->numTimes) ||
        ( sData.timeIndex < 1) || 
        (sData.timeIndex ==  sData.vw->curTime)) {
      wprint("\a%sBad destination time index.\n", badCopy);
      return;
    }
    break;

    /* DNM 2/16/01: made these work relative to section of current
       contour */
  case COPY_TO_NEXT_SECTION:
    sData.currentSection = (int)floor(cont->pts->z + 0.5);
    if (sData.currentSection == (sData.vw->zsize - 1)){
      wprint("\a%sNext section invalid.\n", badCopy);
      return;
    }
    sData.sectionNumber = sData.currentSection + 2;
    doingSection = true;
    break;

  case COPY_TO_PREV_SECTION:
    sData.currentSection = (int)floor(cont->pts->z + 0.5);
    if (!sData.currentSection){
      wprint("\a%sPrevious section invalid.\n", badCopy);
      return;
    }
    sData.sectionNumber = sData.currentSection;
    doingSection = true;
    break;

  }


  /* Loop on all objects, skip if not doing all or it is not current one or it is not 
     selected and it is an allowed operation */
  for (ob = 0; ob < (int)imod->objsize; ob++) {
    if (!(sData.doAllObj || ob == imod->cindex.object || 
          (!sData.doAllObj && !sData.doSurface && !sData.doAll && 
           (doingSection || sData.copyOperation == COPY_TO_TIME) &&
           imodSelectionListQuery(sData.vw, ob, -1) > -2)))
      continue;

    sData.currentObject = ob;
    obj = &imod->obj[ob];
    maxcont = obj->contsize;

    /* look at all contours in current object */
    for (co = 0; co < maxcont; co++) {
      cont = &obj->cont[co];

      /* If copying to section, check for being at source section */
      if (doingSection) {
        if (!cont->psize)
          continue;
        if (floor(cont->pts->z + 0.5) != sData.currentSection)
          continue;
      }
        
      /* If copying to time, check for being at source time */
      if ((sData.copyOperation == COPY_TO_TIME) &&
          (cont->time != sData.currentTime))
        continue;

      /* If copying surface, make sure surface matches */
      if (sData.doSurface && cont->surf != sData.surfaceNumber)
        continue;

      /* copy the entire contour */
      if (cont->psize && (sData.doAll || sData.doAllObj || sData.doSurface || 
                          (co == imod->cindex.contour && ob == imod->cindex.object) || 
                          imodSelectionListQuery(sData.vw, ob, co) > -2)) {
        ncont  = imodContourDup(cont);
        errcode = copyContour(ncont, co);
        free(ncont);
        if (errcode)
          wprint("\a%sFailed to duplicate contour correctly.\n", badCopy);
      }
    }
  }
  sData.vw->undo->finishUnit();
  wprint("Copy operation completed\n");
  imodDraw(sData.vw, IMOD_DRAW_MOD);
  imod_setxyzmouse();
}

void ContourCopy::buttonPressed(int which)
{
  // Set focus gets a changed value to be signaled
  setFocus();

  switch (which) {
  case 0: // Apply
    apply();
    break;

  case 1: // Done
    close();
    break;

  case 2:
    imodShowHelpPage("contourCopy.html#TOP");
    break;
  }
}

void ContourCopy::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
}

// The window is closing, remove from manager
void ContourCopy::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)sData.dia);
  sData.dia = NULL;
  e->accept();
}

// Close on escape, pass on keys
void ContourCopy::keyPressEvent ( QKeyEvent * e )
{
  if (utilCloseKey(e))
    close();
  else
    ivwControlKey(0, e);
}

void ContourCopy::keyReleaseEvent ( QKeyEvent * e )
{
    ivwControlKey(1, e);
}
