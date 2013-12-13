/*
 *  model_edit.cpp -- model edit dialog functions.
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
#include <qlineedit.h>
#include <qcheckbox.h>
#include <qlabel.h>
#include <qlayout.h>
#include <qpushbutton.h>
#include <qtooltip.h>
//Added by qt3to4:
#include <QCloseEvent>
#include <QGridLayout>
#include <QKeyEvent>
#include "dia_qtutils.h"
#include "tooledit.h"

#include "imod.h"
#include "imod_io.h"
#include "display.h"
#include "info_cb.h"
#include "model_edit.h"
#include "mv_modeled.h"
#include "vertexbuffer.h"
#include "control.h"
#include "preferences.h"
#include "undoredo.h"

/* THE MODEL HEADER DIALOG  */

static struct
{
  ModelHeaderWindow  *dia;
  ImodView      *vw;        /* image data to model                       */

} sData = { NULL, 0 };


int openModelEdit(ImodView *vw)
{
     
  if (sData.dia){
    sData.dia->raise();
    return(0);
  }
     
  sData.vw = vw;
  sData.dia = new ModelHeaderWindow
    (imodDialogManager.parent(IMOD_DIALOG), "model offset");

  imodDialogManager.add((QWidget *)sData.dia, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)sData.dia, IMOD_DIALOG);
  return 0;

}

void imodModelEditUpdate()
{
  if (sData.dia)
    sData.dia->update();
}

/****************************************************************************/


/* DNM 12/21/02: interpret the pixel size from string and set into model */
void setPixsizeAndUnits(Imod *imod, const char *string)
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

static const char *headerLabels[] = {"Done", "Help"};
static const char *headerTips[] = {"Open help window",
                             "Close dialog box using current values"};

ModelHeaderWindow::ModelHeaderWindow(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, 1, headerLabels, headerTips, true, 
                ImodPrefs->getRoundedStyle(), " ", "", name)
{
  const char *boxLabels[] = {"Added Z-scale", "Total Z-scale", "Drawing resolution", 
                       "Pixel size"};
  const char *boxTips[] = 
    {"Ratio of section thickness to Z pixel size",
     "Total ratio of section thickness to X/Y pixel size",
     "Interval at which model points are added when drag drawing",
     "Pixel size and units (m, mm, um, nm, A)"}; 
  QString str;
  QLabel *label;
  QPushButton *button;

  mSettingInc = false;

  // Get a pixel size ratio and round to 3 digits, keep this consistent with
  // initNewModel's initial setting of Z scale
  mPixRatio = 1.;
  if (!App->cvi->fakeImage && App->cvi->image->zscale && 
      App->cvi->image->xscale)
    mPixRatio = App->cvi->image->zscale / App->cvi->image->xscale;
  mPixRatio = (float)(0.001 * floor(1000. * mPixRatio + 0.5));

  mDrawBox = diaCheckBox("Draw model", this, mLayout);
  connect(mDrawBox, SIGNAL(toggled(bool)), this, SLOT(drawToggled(bool)));
  mDrawBox->setToolTip("Turn display of entire model on or off");

  mSetIncBox = diaCheckBox("Set incremental Z-scale", this, mLayout);
  connect(mSetIncBox, SIGNAL(toggled(bool)), this, SLOT(setIncToggled(bool)));
  mSetIncBox->setToolTip("Enter ratio of section thickness to Z pixel "
                         "size instead of total Z-scale");

  str.sprintf("Z/X pixel size ratio = %.3f", mPixRatio);
  label = new QLabel(str, this);
  mLayout->addWidget(label);
  QGridLayout *grid = new QGridLayout();
  mLayout->addLayout(grid);

  for (int i = 0; i < 4; i++) {
    str = boxLabels[i];
    label = new QLabel(str, this);
    grid->addWidget(label, i, 0);
    mEditBox[i] = new ToolEdit(this, 12);
    grid->addWidget(mEditBox[i], i, 1);
    connect(mEditBox[i], SIGNAL(returnPressed()), this, SLOT(valueEntered()));
    connect(mEditBox[i], SIGNAL(focusLost()), this, SLOT(valueEntered()));
    mEditBox[i]->setToolTip(boxTips[i]);
  }

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));

  button = diaPushButton("Set Pixel Size from Image", this, mLayout);
  diaSetButtonWidth(button, ImodPrefs->getRoundedStyle(), 1.25,
                    "Set Pixel Size from Image");
  button->setToolTip("Set the model pixel size from pixel spacing in image file header");
  connect(button, SIGNAL(clicked()), this, SLOT(setPixelClicked()));

  setWindowTitle(imodCaption("3dmod Model Header"));
  mEditBox[0]->setEnabled(false);

  update();
}

void ModelHeaderWindow::buttonPressed(int which)
{
  if (!which)
    close();
  else
    imodShowHelpPage("modelHeader.html#TOP");
}

// Key press, lost focus, or window closing: unload the tool edits
// update them, and draw model
void ModelHeaderWindow::valueEntered()
{
  int i;
  for (i = 0; i < 4; i++)
    mEditBox[i]->blockSignals(true);
  setFocus();
  for (i = 0; i < 4; i++)
    mEditBox[i]->blockSignals(false);
  sData.vw->undo->modelChange();

  if (mSettingInc)
    sData.vw->imod->zscale = mPixRatio * mEditBox[0]->text().toFloat();
  else
    sData.vw->imod->zscale = mEditBox[1]->text().toFloat();

  sData.vw->imod->res = atoi(LATIN1(mEditBox[2]->text()));

  setPixsizeAndUnits(sData.vw->imod, LATIN1(mEditBox[3]->text()));

  update();
  imodvPixelChanged();
  sData.vw->undo->finishUnit();
  imodDraw(sData.vw, IMOD_DRAW_MOD);
}

// Set pixel size from image header
void ModelHeaderWindow::setPixelClicked()
{
  sData.vw->undo->modelChange();
  setModelScalesFromImage(sData.vw->imod, false);
  imodvPixelChanged();
  update();
  sData.vw->undo->finishUnit();
}

// The draw box has been toggled
void ModelHeaderWindow::drawToggled(bool state)
{
  if ((state && sData.vw->imod->drawmode <= 0) ||
      (!state &&sData.vw->imod->drawmode > 0))
    sData.vw->imod->drawmode = -sData.vw->imod->drawmode;
  imodDraw(sData.vw, IMOD_DRAW_MOD);
}

// The box for setting incremental Z is toggled
void ModelHeaderWindow::setIncToggled(bool state)
{
  mSettingInc = state;
  mEditBox[0]->setEnabled(state);
  mEditBox[1]->setEnabled(!state);
}

// Set the check box and edit boxes according to model state
void ModelHeaderWindow::update() 
{
  QString str;
  char *units;

  diaSetChecked(mDrawBox, sData.vw->imod->drawmode > 0);
 
  str.sprintf("%g", sData.vw->imod->zscale / mPixRatio);
  mEditBox[0]->setText(str);

  str.sprintf("%g", sData.vw->imod->zscale);
  mEditBox[1]->setText(str);

  str.sprintf("%g", (float)sData.vw->imod->res);
  mEditBox[2]->setText(str);

  str.sprintf("%g ", sData.vw->imod->pixsize);
  units = imodUnits(sData.vw->imod);
  if (units)
    str += units;
  mEditBox[3]->setText(str);
}

void ModelHeaderWindow::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
}

// Window is closing - unload values and remove dialog
void ModelHeaderWindow::closeEvent ( QCloseEvent * e )
{
  valueEntered();
  imodDialogManager.remove((QWidget *)sData.dia);
  sData.dia = NULL;
  e->accept();
}

// Exit on escape, pass on other keys
void ModelHeaderWindow::keyPressEvent ( QKeyEvent * e )
{
  if (utilCloseKey(e))
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
  adjustGeometryAndShow((QWidget *)OffsetDialog.dia, IMOD_DIALOG);
  return 0;
}

// Translate the model
void imodTransXYZ(Imod *imod, Ipoint trans)
{
  int ob, co, pt,  me, i;
  Iobj *obj;
  Icont *cont;
  Imesh *mesh;
     
  vbCleanupVBD(imod);
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

static const char *buttonLabels[] = {"Apply", "Revert", "Set Base", "Done"};
static const char *buttonTips[] = 
  {"Make total model offsets be entered values plus base values", 
   "Restore model offsets to zero", 
   "Make current offset be a base for further incremental offsets",
   "Close dialog box and permanently accept currently applied offsets"};


ModelOffsetWindow::ModelOffsetWindow(QWidget *parent, const char *name)
  : DialogFrame(parent, 4, 1, buttonLabels, buttonTips, false, 
                ImodPrefs->getRoundedStyle(), " ", "", name)
{
  const char *xyz[] = {"X", "Y", "Z"};
  QString str;
  QLabel *label;

  diaLabel("Total offsets to be applied:", this, mLayout);
  QGridLayout *grid = new QGridLayout();
  mLayout->addLayout(grid);

  for (int i = 0; i < 3; i++) {
    str = xyz[i];
    label = new QLabel(str + ":", this);
    grid->addWidget(label, i, 0);
    mEditBox[i] = new QLineEdit(this);
    grid->addWidget(mEditBox[i], i, 1);
    //   mEditBox[i]->setFocusPolicy(ClickFocus);
    connect(mEditBox[i], SIGNAL(returnPressed()), this, SLOT(valueEntered()));
    mEditBox[i]->setToolTip("Enter offset to apply in " + str);
    mBaseLabel[i] = new QLabel(" ", this);
    grid->addWidget(mBaseLabel[i], i, 2);
  }

  diaLabel("Current applied offset:", this, mLayout);
  mAppliedLabel = diaLabel("C", this, mLayout);
  updateLabels();

  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
  setWindowTitle(imodCaption("3dmod Model Offset"));
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

void ModelOffsetWindow::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
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
  if (utilCloseKey(e))
    close();
  else
    ivwControlKey(0, e);
}

void ModelOffsetWindow::keyReleaseEvent ( QKeyEvent * e )
{
    ivwControlKey(1, e);
}

