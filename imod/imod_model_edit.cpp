/*  IMOD VERSION 2.40
 *
 *  imod_model_edit.c -- model edit dialog functions.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <math.h>
#include <qlineedit.h>
#include <qcheckbox.h>
#include <qlabel.h>
#include <qlayout.h>
#include <qtooltip.h>
#include "dia_qtutils.h"
#include "tooledit.h"

#include "imod.h"
#include "imod_display.h"
#include "imod_info_cb.h"
#include "imod_model_edit.h"
#include "imodv_modeled.h"
#include "control.h"
#include "preferences.h"
#include "undoredo.h"

/* THE MODEL HEADER DIALOG  */

static struct
{
  ModelHeaderWindow  *dia;
  ImodView      *vw;        /* image data to model                       */

}HeaderDialog = { NULL, 0 };


int openModelEdit(ImodView *vw)
{
     
  if (HeaderDialog.dia){
    HeaderDialog.dia->raise();
    return(0);
  }
     
  HeaderDialog.vw = vw;
  HeaderDialog.dia = new ModelHeaderWindow
    (imodDialogManager.parent(IMOD_DIALOG), "model offset");

  imodDialogManager.add((QWidget *)HeaderDialog.dia, IMOD_DIALOG);
  return 0;

}

void imodModelEditUpdate()
{
  if (HeaderDialog.dia)
    HeaderDialog.dia->update();
}

/****************************************************************************/


/* DNM 12/21/02: interpret the pixel size from string and set into model */
void setPixsizeAndUnits(Imod *imod, char *string)
{
  float fscale = 0.;
  
  if (!string || string[0] == 0x00)
    return;
  fscale = atof(string);
  if (!fscale)
    return;
 
  /* Leave unchanged if zero, otherwise find the units */
  imod->pixsize = fscale;
  imod->units = IMOD_UNIT_PIXEL;
  if (strstr(string, "km"))
    imod->units = IMOD_UNIT_KILO;
  if (strstr(string, "m"))
    imod->units = IMOD_UNIT_METER;
  if (strstr(string, "cm"))
    imod->units = IMOD_UNIT_CM;
  if (strstr(string, "mm"))
    imod->units = IMOD_UNIT_MM;
  if (strstr(string, "um"))
    imod->units = IMOD_UNIT_UM;
  if (strstr(string, "nm"))
    imod->units = IMOD_UNIT_NM;
  if (strstr(string, "A"))
    imod->units = IMOD_UNIT_ANGSTROM;
  if (strstr(string, "pm"))
    imod->units = IMOD_UNIT_PM;
}

// 11/1/04: scale the model resolution when above or below limiting zooms
float scaleModelRes(int res, float zoom)
{
  float zoomMin = 0.75f;
  float zoomMax = 1.5f;
  float ret = res;
  if (zoom < zoomMin)
    ret *= zoomMin / zoom;
  if (zoom > zoomMax)
    ret *= zoomMax / zoom;
  return ret;
}


/* THE WINDOW CLASS FOR MODEL HEADER */

static char *headerLabels[] = {"Done", "Help"};
static char *headerTips[] = {"Open help window",
                             "Close dialog box using current values"};

ModelHeaderWindow::ModelHeaderWindow(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, 1, headerLabels, headerTips, true, 
                ImodPrefs->getRoundedStyle(), " ", "", name)
{
  char *boxLabels[] = {"Z-scale", "Resolution", "Pixel size"};
  char *boxTips[] = 
    {"Ratio of section thickness to X/Y pixel size",
     "Interval at which model points are added when drag drawing",
     "Pixel size and units (m, mm, um, nm, A)"}; 
  QString str;
  QLabel *label;

  mDrawBox = diaCheckBox("Draw model", this, mLayout);
  QGridLayout *grid = new QGridLayout(mLayout, 3, 2);
  connect(mDrawBox, SIGNAL(toggled(bool)), this, SLOT(drawToggled(bool)));
  QToolTip::add(mDrawBox, "Turn display of entire model on or off");

  for (int i = 0; i < 3; i++) {
    str = boxLabels[i];
    label = new QLabel(str, this);
    grid->addWidget(label, i, 0);
    mEditBox[i] = new ToolEdit(this, 12);
    grid->addWidget(mEditBox[i], i, 1);
    connect(mEditBox[i], SIGNAL(returnPressed()), this, SLOT(valueEntered()));
    connect(mEditBox[i], SIGNAL(focusLost()), this, SLOT(valueEntered()));
    QToolTip::add(mEditBox[i], boxTips[i]);
  }

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));
  setCaption(imodCaption("3dmod Model Header"));

  update();
  show();
}

void ModelHeaderWindow::buttonPressed(int which)
{
  if (!which)
    close();
  else
    dia_vasmsg
      ("Model Header Help\n",
       "---------------------------\n",
       "This dialog allows you to set the following items in the model header:"
       "\n\n"
       "The \"Z-scale\" text box allows you to enter a scaling factor for the "
       "size of the pixels in Z (section thickness) relative to their size in "
       "X and Y.  For serial sections, the Z-scale is the ratio of section "
       "thickness to pixel size.  For tomograms, the Z-scale is the ratio of "
       "the original section thickness (typically the nominal thickness at "
       "which the section was cut) to the thickness "
       "of material in the reconstruction.\n\n"
       "\tThe \"Resolution\" text box determines the spacing between "
       "successive "
       "points when adding points continuously while holding down the second "
       "mouse button.  Points will be added at the given interval for image "
       "zooms between 0.75 and 1.5, and at proportionally bigger or smaller "
       "intervals for zooms lower or higher than these values, respectively."
       "\n\n"
       "\tThe \"Pixel size\" text box allows you to set the pixel size of "
       "the model, which is needed for extracting quantitative information.  "
       "Enter a number then the units of measurement.  Available units are "
       "km, m, cm, um, nm, A, and pm.\n",
       NULL);
}

// Key press, lost focus, or window closing: unload the tool edits
// update them, and draw model
void ModelHeaderWindow::valueEntered()
{
  int i;
  for (i = 0; i < 3; i++)
    mEditBox[i]->blockSignals(true);
  setFocus();
  for (i = 0; i < 3; i++)
    mEditBox[i]->blockSignals(false);
  HeaderDialog.vw->undo->modelChange();
  HeaderDialog.vw->imod->zscale = mEditBox[0]->text().toFloat();

  HeaderDialog.vw->imod->res = atoi(mEditBox[1]->text().latin1());

  setPixsizeAndUnits(HeaderDialog.vw->imod, 
                     (char *)mEditBox[2]->text().latin1());

  update();
  imodvPixelChanged();
  HeaderDialog.vw->undo->finishUnit();
  imodDraw(HeaderDialog.vw, IMOD_DRAW_MOD);
}

// The draw box has been toggled
void ModelHeaderWindow::drawToggled(bool state)
{
  if (state && HeaderDialog.vw->imod->drawmode <= 0 ||
      !state &&HeaderDialog.vw->imod->drawmode > 0)
    HeaderDialog.vw->imod->drawmode = -HeaderDialog.vw->imod->drawmode;
  imodDraw(HeaderDialog.vw, IMOD_DRAW_MOD);
}

// Set the check box and edit boxes according to model state
void ModelHeaderWindow::update() 
{
  QString str;
  char *units;

  diaSetChecked(mDrawBox, HeaderDialog.vw->imod->drawmode > 0);
 
  str.sprintf("%g", HeaderDialog.vw->imod->zscale);
  mEditBox[0]->setText(str);

  str.sprintf("%g", (float)HeaderDialog.vw->imod->res);
  mEditBox[1]->setText(str);

  str.sprintf("%g ", HeaderDialog.vw->imod->pixsize);
  units = imodUnits(HeaderDialog.vw->imod);
  if (units)
    str += units;
  mEditBox[2]->setText(str);
}

void ModelHeaderWindow::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::fontChange(oldFont);
}

// Window is closing - unload values and remove dialog
void ModelHeaderWindow::closeEvent ( QCloseEvent * e )
{
  valueEntered();
  imodDialogManager.remove((QWidget *)HeaderDialog.dia);
  HeaderDialog.dia = NULL;
  e->accept();
}

// Exit on escape, pass on other keys
void ModelHeaderWindow::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

void ModelHeaderWindow::keyReleaseEvent ( QKeyEvent * e )
{
    ivwControlKey(1, e);
}



/****************************************************************************/
/*  MODEL OFFSET DIALOG                                                     */

static struct
{
  ModelOffsetWindow   *dia;
  ImodView      *vw;        /* image data to model                       */
  Ipoint        applied;
  Ipoint        base;

}OffsetDialog = { NULL, NULL, {0., 0., 0.}, {0., 0., 0.}};


int openModelOffset(ImodView *vw)
{
     
  if (OffsetDialog.dia){
    OffsetDialog.dia->raise();
    return(0);
  }
     
  OffsetDialog.vw = vw;
  OffsetDialog.base.x = OffsetDialog.base.y = OffsetDialog.base.z = 0.0;
  OffsetDialog.applied = OffsetDialog.base;

  OffsetDialog.dia = new ModelOffsetWindow
    (imodDialogManager.parent(IMOD_DIALOG), "model offset");

  imodDialogManager.add((QWidget *)OffsetDialog.dia, IMOD_DIALOG);
  return 0;
}

// Translate the model
void imodTransXYZ(Imod *imod, Ipoint trans)
{
  int ob, co, pt,  me, i;
  Iobj *obj;
  Icont *cont;
  Imesh *mesh;
     
  for(ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for(pt = 0; pt < cont->psize; pt++){
	cont->pts[pt].x += trans.x;
	cont->pts[pt].y += trans.y;
	cont->pts[pt].z += trans.z;
      }
    }

    /* Translate the meshes too */
    for(me = 0; me < obj->meshsize; me++) {
      mesh = &obj->mesh[me];
      if (!mesh || !mesh->vsize)
	continue;
      for(i = 0; i < mesh->vsize; i += 2){
	mesh->vert[i].x += trans.x;
	mesh->vert[i].y += trans.y;
	mesh->vert[i].z += trans.z;
      }
    }
  }

  if (OffsetDialog.dia) {
    OffsetDialog.applied.x += trans.x;
    OffsetDialog.applied.y += trans.y;
    OffsetDialog.applied.z += trans.z;
    OffsetDialog.dia->updateLabels();
  }
}

/* The window class */

static char *buttonLabels[] = {"Apply", "Revert", "Set Base", "Done"};
static char *buttonTips[] = 
  {"Make total model offsets be entered values plus base values", 
   "Restore model offsets to zero", 
   "Make current offset be a base for further incremental offsets",
   "Close dialog box and permanently accept currently applied offsets"};


ModelOffsetWindow::ModelOffsetWindow(QWidget *parent, const char *name)
  : DialogFrame(parent, 4, 1, buttonLabels, buttonTips, false, 
                ImodPrefs->getRoundedStyle(), " ", "", name)
{
  char *xyz[] = {"X", "Y", "Z"};
  QString str;
  QLabel *label;

  diaLabel("Total offsets to be applied:", this, mLayout);
  QGridLayout *grid = new QGridLayout(mLayout, 3, 3);

  for (int i = 0; i < 3; i++) {
    str = xyz[i];
    label = new QLabel(str + ":", this);
    grid->addWidget(label, i, 0);
    mEditBox[i] = new QLineEdit(this);
    grid->addWidget(mEditBox[i], i, 1);
    //   mEditBox[i]->setFocusPolicy(ClickFocus);
    connect(mEditBox[i], SIGNAL(returnPressed()), this, SLOT(valueEntered()));
    QToolTip::add(mEditBox[i], "Enter offset to apply in " + str);
    mBaseLabel[i] = new QLabel(" ", this);
    grid->addWidget(mBaseLabel[i], i, 2);
  }

  diaLabel("Current applied offset:", this, mLayout);
  mAppliedLabel = diaLabel("C", this, mLayout);
  updateLabels();

  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
  setCaption(imodCaption("3dmod Model Offset"));
  show();
}

// Respond to button press: apply, revert, set base, done
void ModelOffsetWindow::buttonPressed(int which)
{
  Ipoint offset;
  int i;
  setFocus();
  switch (which) {
  case 0:  // Apply
    for (i = 0; i < 3; i++)
      *(&offset.x + i) = mEditBox[i]->text().toFloat();

    offset.x += OffsetDialog.base.x - OffsetDialog.applied.x;
    offset.y += OffsetDialog.base.y - OffsetDialog.applied.y;
    offset.z += OffsetDialog.base.z - OffsetDialog.applied.z;

    OffsetDialog.vw->undo->modelShift(&offset);
    imodTransXYZ(OffsetDialog.vw->imod, offset);
    OffsetDialog.vw->undo->finishUnit();
    imodDraw(OffsetDialog.vw, IMOD_DRAW_MOD);
    break;

  case 1:  // revert
    offset.x = -OffsetDialog.applied.x;
    offset.y = -OffsetDialog.applied.y;
    offset.z = -OffsetDialog.applied.z;
    
    OffsetDialog.vw->undo->modelShift(&offset);
    imodTransXYZ(OffsetDialog.vw->imod, offset);
    OffsetDialog.vw->undo->finishUnit();
    OffsetDialog.base.x = OffsetDialog.base.y = OffsetDialog.base.z = 0.0;
    OffsetDialog.applied = OffsetDialog.base;
    imodDraw(OffsetDialog.vw, IMOD_DRAW_MOD);
    updateLabels();
    break;

  case 2: // New  base
    OffsetDialog.base = OffsetDialog.applied;
    for (i = 0; i < 3; i++)
      mEditBox[i]->clear();
    updateLabels();
    break;

  case 3: // Done
    close();
    break;
  }
}

// Return in an edit box is like apply
void ModelOffsetWindow::valueEntered()
{
  buttonPressed(0);
}

// Update the labels in the dialog
void ModelOffsetWindow::updateLabels()
{
  QString str;
  for (int i = 0; i < 3; i++){
    str.sprintf("+%9.2f base offset", *(&OffsetDialog.base.x + i));
    mBaseLabel[i]->setText(str);
  }
  str.sprintf("%9.2f,%9.2f,%9.2f", 
	      OffsetDialog.applied.x, OffsetDialog.applied.y,
	      OffsetDialog.applied.z);
  mAppliedLabel->setText(str);
}

void ModelOffsetWindow::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::fontChange(oldFont);
}

// The window is closing, remove from manager
void ModelOffsetWindow::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)OffsetDialog.dia);
  OffsetDialog.dia = NULL;
  e->accept();
}

// Close on escape, pass on keys
void ModelOffsetWindow::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

void ModelOffsetWindow::keyReleaseEvent ( QKeyEvent * e )
{
    ivwControlKey(1, e);
}

/*
$Log$
Revision 4.9  2004/11/05 19:08:12  mast
Include local files with quotes, not brackets

Revision 4.8  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.7  2004/11/01 23:29:01  mast
Added resolution scaling and help screen

Revision 4.6  2004/01/22 19:12:43  mast
changed from pressed() to clicked() or accomodated change to actionClicked

Revision 4.5  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.4  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.3  2003/04/11 21:47:28  mast
adding tooltips

Revision 4.2  2003/02/28 21:40:57  mast
Changing name of tooledit focus signal

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

Revision 1.1.2.3  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.2  2003/01/23 20:01:04  mast
Full Qt version

Revision 1.1.2.1  2003/01/18 01:16:20  mast
half Qt version

Revision 3.0.2.2  2003/01/13 01:15:43  mast
changes for Qt version of info window

Revision 3.0.2.1  2002/12/23 04:59:19  mast
Make routine for parsing pixel size string

*/
