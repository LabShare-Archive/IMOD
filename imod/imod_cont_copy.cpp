/*
 *  imod_cont_copy.c -- Contour copy dialog.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
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
#include "imod_cont_copy.h"
#include "imod.h"
#include "imod_display.h"
#include "imod_edit.h"
#include "control.h"
#include "preferences.h"
#include "undoredo.h"

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

ThisDialog = 
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
  ImodView *vw = ThisDialog.vw;
  int obnum = vw->imod->cindex.object;

  if (!cont) return(-1);
  if (!cont->psize) return(-1);

  switch(ThisDialog.copyOperation){

  case COPY_TO_OBJECT:
    obnum = ThisDialog.objectNumber - 1;
    toObj = &vw->imod->obj[obnum];

    /* Don't copy if duplicate contour already exists. */
    for(co = 0; co < toObj->contsize; co++){
      if (contCompare(&toObj->cont[co], cont) == 0)
        return(0);
    }
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
    toObj   = imodObjectGet(vw->imod);
    section = ThisDialog.sectionNumber-1; 
    for(pt = 0; pt < cont->psize; pt++){
      cont->pts[pt].z = section;
    }
    vw->undo->contourAddition(obnum, toObj->contsize);
    imodObjectAddContour(toObj, cont);
    break;

  case COPY_TO_TIME:
  case COPY_TO_NEXT_TIME:
    toObj   = &vw->imod->obj[ThisDialog.currentObject];
    cont->time = ThisDialog.timeIndex;
    vw->undo->contourAddition(obnum, toObj->contsize);
    imodObjectAddContour(toObj, cont);
    break;
  }

  // Copy any contour properties from source to destination object
  if (istoreCountContSurfItems(vw->imod->obj[ThisDialog.currentObject].store, 
                               coNum, 0)) {
    vw->undo->objectPropChg(obnum);
    istoreCopyContSurfItems(vw->imod->obj[ThisDialog.currentObject].store, 
                             &toObj->store, coNum, toObj->contsize - 1, 0);
  }

  return(0);
}


int openContourCopyDialog(ImodView *vw)
{
     
  if (ThisDialog.dia){
    ThisDialog.dia->raise();
    return(0);
  }
     
  ThisDialog.vw = vw;
  ThisDialog.dia = new ContourCopy(imodDialogManager.parent(IMOD_DIALOG), 
                                   "contour join");

  if (!ThisDialog.dia) return(1);

  imodDialogManager.add((QWidget *)ThisDialog.dia, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)ThisDialog.dia, IMOD_DIALOG);
  return(0);
}

// Entry for hot key copying
void iccCopyContour(void)
{
  if (!ThisDialog.dia)
    wprint("\aContour Copy dialog must be open to copy with this hot key.\n");
  else
    ThisDialog.dia->apply();
}

// External entry for updating
void imodContCopyUpdate(void)
{
  if (ThisDialog.dia)
    ThisDialog.dia->update();
}

/****************************************************************************/


static char *buttonLabels[] = {"Apply", "Done", "Help"};
static char *buttonTips[] = {"Copy the selected contours",
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
  if (ThisDialog.vw->nt)
    mToCombo->addItem("Copy to Time Index #");
  mToCombo->setFocusPolicy(Qt::NoFocus);
  mToCombo->setCurrentIndex(ThisDialog.copyOperation);
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

  radio = diaRadioButton("Just the current contour", gbox, mRadioGroup, 
                         gbLayout, 0, "Copy only the current contour");
  radio = diaRadioButton
    ("All contours in surface", gbox, mRadioGroup, gbLayout, 1, 
     "Copy contours with same surface number as current one");
  radio = diaRadioButton("All contours in object", gbox, mRadioGroup, 
                         gbLayout, 2, "Copy all eligible contours in object");

  if (ThisDialog.vw->nt) {
    mTimeRadio = diaRadioButton
      ("All contours in all objects", gbox, mRadioGroup, 
       gbLayout, 3, "Copy all contours at this time to selected time");
  }

  // Figure out initial radio button settings, enforce flag settings and
  // set the button
  int radioVal = 0;
  if (ThisDialog.doSurface)
    radioVal = 1;
  if (ThisDialog.doAll)
    radioVal = 2;
  if (ThisDialog.vw->nt && ThisDialog.doAllObj)
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
  ThisDialog.copyOperation = which;
  update();
}

void ContourCopy::toValueChanged(int value)
{
  setFocus();
  switch (ThisDialog.copyOperation) {
  case COPY_TO_OBJECT:
    ThisDialog.objectNumber = value;
    break;
  case COPY_TO_SECTION:
    ThisDialog.sectionNumber = value;
    break;
  case COPY_TO_TIME:
    ThisDialog.timeIndex = value;
    break;
  default:
    break;
  }
}

// Set flags from radio button value
void ContourCopy::rangeSelected(int which)
{
  ThisDialog.doSurface = (which == 1) ? 1 : 0;
  ThisDialog.doAll = (which == 2) ? 1 : 0;
  ThisDialog.doAllObj = (which == 3) ? 1 : 0;
}


void ContourCopy::update()
{
  int minVal, maxVal, curVal;
  ImodView *vw = ThisDialog.vw;

  // For each operation that uses spin box, set up min and max value
  // and adjust the current value if necessary
  switch(ThisDialog.copyOperation){
  case COPY_TO_OBJECT:
    minVal = 1;
    maxVal = vw->imod->objsize;
    if (ThisDialog.objectNumber < 1)
      ThisDialog.objectNumber = 1;
    if (ThisDialog.objectNumber < maxVal)
      ThisDialog.objectNumber = maxVal;
    curVal = ThisDialog.objectNumber;
    break;

  case COPY_TO_SECTION:
    minVal = 1;
    maxVal = vw->zsize;
    if (ThisDialog.sectionNumber < 1)
      ThisDialog.sectionNumber = 1;
    if (ThisDialog.sectionNumber < maxVal)
      ThisDialog.sectionNumber = maxVal;
    curVal = ThisDialog.sectionNumber;
    break;

  case COPY_TO_TIME:
    minVal = 1;
    maxVal = vw->nt;
    if (ThisDialog.timeIndex < 1)
      ThisDialog.timeIndex = 1;
    if (ThisDialog.timeIndex < maxVal)
      ThisDialog.timeIndex = maxVal;
    curVal = ThisDialog.timeIndex;
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
  if (vw->nt)
    mTimeRadio->setEnabled(ThisDialog.copyOperation == COPY_TO_TIME);

}

void ContourCopy::apply()
{
  char *badCopy = "Copy operation cancelled.  ";
  /*     char *badObjectErrorMsg = 
         "\nCopy operation cancelled.\n"
         "Object out of range or invalid\n.";
  */

  Imod *imod = ThisDialog.vw->imod;
  Iobj *obj   = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  Icont *ncont;
  int ob, co, errcode, maxcont;

  if (!obj){
    wprint("\a%sBad input object.\n",badCopy);
    return;
  }

  /* DNM: check validity of current contour: here test on all the
     conditions where a current contour is not needed */
  if (!((ThisDialog.doAll || ThisDialog.doAllObj) && 
        (ThisDialog.copyOperation == COPY_TO_OBJECT ||
         ThisDialog.copyOperation == COPY_TO_CURRENT || 
         ThisDialog.copyOperation == COPY_TO_TIME))) {
    if ((!cont) || (cont->psize <= 0)){
      wprint("\a%sBad input contour.\n", badCopy);
      return;
    }
    
    /* Set surface number here since we know we have a contour */
    ThisDialog.surfaceNumber = cont->surf;
  }

  /* check copy to place is valid. */
  switch(ThisDialog.copyOperation){
  case COPY_TO_OBJECT:
    if ((ThisDialog.objectNumber < 1) ||
        (ThisDialog.objectNumber > (int)imod->objsize) ||
        (ThisDialog.objectNumber == imod->cindex.object + 1)){
      wprint("\a%sBad destination object.\n", badCopy);
      return;
    }
    break;

  case COPY_TO_CURRENT:
    break;

  case COPY_TO_SECTION:
    /* get section number to copy from.*/
    ThisDialog.currentSection = (int)floor(cont->pts->z + 0.5);

    /* check section number to copy to. */
    if ((ThisDialog.sectionNumber <= 0) || 
        ( ThisDialog.sectionNumber > ThisDialog.vw->zsize ) ||
        (ThisDialog.currentSection + 1 == ThisDialog.sectionNumber)){
      wprint("\a%sBad destination section.\n", badCopy);
      return;
    }
    break;

  case COPY_TO_TIME:
    ThisDialog.currentTime = cont->time;
    if ((ThisDialog.timeIndex > ThisDialog.vw->nt) ||
        ( ThisDialog.timeIndex < 1) || 
        (ThisDialog.timeIndex ==  ThisDialog.vw->ct)) {
      wprint("\a%sBad destination time index.\n", badCopy);
      return;
    }
    break;

    /* DNM 2/16/01: made these work relative to section of current
       contour */
  case COPY_TO_NEXT_SECTION:
    ThisDialog.currentSection = (int)floor(cont->pts->z + 0.5);
    if (ThisDialog.currentSection == (ThisDialog.vw->zsize - 1)){
      wprint("\a%sNext section invalid.\n", badCopy);
      return;
    }
    ThisDialog.sectionNumber = ThisDialog.currentSection + 2;
    break;

  case COPY_TO_PREV_SECTION:
    ThisDialog.currentSection = (int)floor(cont->pts->z + 0.5);
    if (!ThisDialog.currentSection){
      wprint("\a%sPrevious section invalid.\n", badCopy);
      return;
    }
    ThisDialog.sectionNumber = ThisDialog.currentSection;
    break;

  }


  if (!(ThisDialog.doAll || ThisDialog.doAllObj || ThisDialog.doSurface)){
    ThisDialog.currentObject = imod->cindex.object;
    ncont = imodContourDup(cont);
    copyContour(ncont, imod->cindex.contour);
    free(ncont);
  }else{

    /* Loop on all objects, skip if not doing all or it is not current one */
    for (ob = 0; ob < (int)imod->objsize; ob++) {
      if (!(ThisDialog.doAllObj || ob == imod->cindex.object))
        continue;

      ThisDialog.currentObject = ob;
      obj = &imod->obj[ob];
      maxcont = obj->contsize;

      /* look at all contours in current object */
      for (co = 0; co < maxcont; co++) {
        cont = &obj->cont[co];

        /* If copying to section, check for being at source section */
        if (ThisDialog.copyOperation == COPY_TO_SECTION ||
            ThisDialog.copyOperation == COPY_TO_NEXT_SECTION ||
            ThisDialog.copyOperation == COPY_TO_PREV_SECTION){
          if (!cont->psize)
            continue;
          if (floor(cont->pts->z + 0.5) != ThisDialog.currentSection)
            continue;
        }
        
        /* If copying to time, check for being at source time */
        if ((ThisDialog.copyOperation == COPY_TO_TIME) &&
            (cont->time != ThisDialog.currentTime))
          continue;

        /* If copying surface, make sure surface matches */
        if (ThisDialog.doSurface && cont->surf != ThisDialog.surfaceNumber)
          continue;

        /* copy the entire contour */
        if (cont->psize){
          ncont  = imodContourDup(cont);
          errcode = copyContour(ncont, co);
          free(ncont);
          if (errcode)
            wprint("\a%sFailed to duplicate contour correctly.\n", badCopy);
        }
      }
    }
  }
  ThisDialog.vw->undo->finishUnit();
  wprint("Copy operation completed\n");
  imodDraw(ThisDialog.vw, IMOD_DRAW_MOD);
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
    imodShowHelpPage("contourCopy.html");
    break;
  }
}

void ContourCopy::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::fontChange(oldFont);
}

// The window is closing, remove from manager
void ContourCopy::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)ThisDialog.dia);
  ThisDialog.dia = NULL;
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

/*

$Log$
Revision 4.20  2009/03/22 19:54:25  mast
Show with new geometry adjust routine for Mac OS X 10.5/cocoa

Revision 4.19  2009/02/26 20:04:04  mast
Turn off keyboard tracking of spin boxes

Revision 4.18  2009/01/15 16:33:17  mast
Qt 4 port

Revision 4.17  2008/12/10 01:05:15  mast
Added hot key for contour copy

Revision 4.16  2008/04/04 21:22:03  mast
Free contour after adding to object

Revision 4.15  2007/09/14 21:56:38  sueh
bug# 1038 Switching from calling dia_vasmsg() to opening an .html file for help.

Revision 4.14  2006/09/12 15:34:54  mast
Handled contour member renames

Revision 4.13  2005/06/29 05:38:40  mast
Changes to manipulate fine grain properties and do undos correctly

Revision 4.12  2005/05/27 23:00:45  mast
Remove debugging statement

Revision 4.11  2005/05/27 22:59:10  mast
Set focus on button press so typed-in value without return is used

Revision 4.10  2004/11/30 18:59:09  mast
Switch to different combo box style to get better size behavior on Mac

Revision 4.9  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 4.8  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.7  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.6  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.5  2003/04/11 21:47:28  mast
adding tooltips

Revision 4.4  2003/03/03 22:14:34  mast
cleanup

Revision 4.3  2003/02/27 19:31:35  mast
remove include of unistd.h for windows

Revision 4.2  2003/02/14 01:14:06  mast
Add error report if duplication fails

Revision 4.1  2003/02/10 20:28:59  mast
autox.cpp

Revision 1.1.2.4  2003/02/07 01:03:23  mast
a little cleanup

Revision 1.1.2.3  2003/01/31 01:01:22  mast
Fixed spin box focus policy and combo box initial setting

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/23 19:57:06  mast
Qt version

Revision 3.3  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.2  2002/09/26 21:28:55  rickg
Removed empty string sprintf formats and unused variables.

Revision 3.1  2002/09/13 20:56:01  mast
Changed include of libgen.h to be on sun only

*/
