/*
 *  mv_control.cpp -- The imodv control edit dialog.
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

#include <qstring.h>
#include "formv_control.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "mv_control.h"
#include "mv_gfx.h"
#include "mv_input.h"
#include "model_edit.h"
#include "control.h"
#include "mv_window.h"
#include "rotationtool.h"

static imodvControlForm *dialog = NULL;
static float lastX = -999;
static float lastY = -999.;
static float lastZ = -999.;
static float lastScale = -999.;

#define ROTATION_MAX 100

////////////////////////////////////////
// Functions called from the form
//

void imodvControlHelp(void)
{
  imodShowHelpPage("modvControl.html#TOP");
}


/* increase or decrease the zoom, depending on whether zoom > 0 */
void imodvControlZoom(int zoom)
{
  if (zoom > 0)
    imodv_zoomd(Imodv, 1.05);
  else
    imodv_zoomd(Imodv, 0.95238095);
  imodvDraw(Imodv);
  return;
}

/* Set the flag for kicking clipping planes out */
void imodvControlKickClips(bool state)
{
  ImodvApp *a = Imodv;
  int m, mst, mnd;
  mst = a->crosset ? 0 : a->curMod;
  mnd = a->crosset ? a->numMods - 1 : a->curMod;

  imodvRegisterModelChg();
  for (m = mst; m <= mnd; m++) {
    if (state)
      a->mod[m]->view->world |= WORLD_KICKOUT_CLIPS;
    else
      a->mod[m]->view->world &= ~WORLD_KICKOUT_CLIPS;
  }
  imodvFinishChgUnit();
  imodvDraw(a);
}

/* Set the clipping or perspective values: near = plane 1, far = plane 0,
   perspective (fovy) = plane 2 */
void imodvControlClip(int plane, int value, bool dragging)
{
  ImodvApp *a = Imodv;
  Iview *view, *vwv;
  int m;
  static bool sliding = false;

  if (!Imodv->imod) return;
  /*     view = &Imodv->imod->view[Imodv->imod->cview];*/
  view = Imodv->imod->view;

  if (!sliding) {
    imodvRegisterModelChg();
    imodvFinishChgUnit();
  }
  sliding = dragging;

  if (plane == IMODV_CONTROL_FAR){
    Imodv->cfar = value;
    if (Imodv->cnear >= Imodv->cfar){
      Imodv->cnear = Imodv->cfar - 1;
      dialog->setViewSlider(IMODV_CONTROL_NEAR, a->cnear);
    }
    view->cnear = Imodv->cnear * 0.001;
    view->cfar  = Imodv->cfar * 0.001;
    if (Imodv->crosset)
      for (m = 0; m < Imodv->numMods; m++) {
        vwv = Imodv->mod[m]->view;
        vwv->cfar = view->cfar;
        if (vwv->cnear >= vwv->cfar)
          vwv->cnear = vwv->cfar - 0.001;
      }
  }

  if (plane == IMODV_CONTROL_NEAR){
    Imodv->cnear = value;
    if (Imodv->cfar <= Imodv->cnear){
      Imodv->cfar = Imodv->cnear + 1;
      dialog->setViewSlider(IMODV_CONTROL_FAR, a->cfar);
    }
    view->cnear = Imodv->cnear * 0.001;
    view->cfar  = Imodv->cfar * 0.001;
    if (Imodv->crosset)
      for (m = 0; m < Imodv->numMods; m++) {
        vwv = Imodv->mod[m]->view;
        vwv->cnear = view->cnear;
        if (vwv->cfar <= vwv->cnear)
          vwv->cfar = vwv->cnear + 0.001;
      }
  }

  if (plane == IMODV_CONTROL_FOVY) {
    Imodv->fovy = value;
    view->fovy  = Imodv->fovy;
    if (Imodv->crosset)
      for (m = 0; m < Imodv->numMods; m++) 
        Imodv->mod[m]->view->fovy = view->fovy;
  }

  imodvDraw(Imodv);
}

void imodvControlZscale(int value, bool dragging)
{
  static bool sliding = false;
  if (!sliding) {
    imodvRegisterModelChg();
    imodvFinishChgUnit();
  }
  sliding = dragging;

  /*  int m, nm;
      nm = Imodv->numMods; */
  Imodv->mod[Imodv->curMod]->zscale = value / 100.0;
  /* DNM: this should not be adjusted in tandem in general */
  /*     if (Imodv->crosset)
         for(m = 0; m < nm; m++){
         Imodv->mod[m]->zscale = cbs->value / 10.0;
         }
  */
  imodvDraw(Imodv);
  imodvDrawImodImages();
  imodModelEditUpdate();
}

void imodvControlScale(float scale)
{
  ImodvApp *a = Imodv;
  int m;
  a->imod->view->rad = 
    0.5 * (a->winx > a->winy ? a->winy : a->winx) / scale;
  if (Imodv->crosset)
    for(m = 0; m < a->numMods; m++)
      a->mod[m]->view->rad = a->imod->view->rad;
  imodvDraw(Imodv);
}


/* Start/stop movie */
void imodvControlStart(void)
{
  if (Imodv->movie)
    Imodv->movie = 0;
  else
    Imodv->movie = 1;
  
  Imodv->xrotMovie = 0;
  Imodv->yrotMovie = 0;
  Imodv->zrotMovie = 0;
  imodvDraw(Imodv);
  if (Imodv->mainWin->mRotationTool)
    Imodv->mainWin->mRotationTool->setCenterState(Imodv->movie == 1);
}

/* Increase/decrease rotation angle using button */
void imodvControlAxisButton(int axisDir)
{
  ImodvApp *a = Imodv;
  switch(axisDir){
  case IMODV_CONTROL_XAXIS:
    imodv_rotate_model(a, a->deltaRot, 0, 0);
    break;
  case -IMODV_CONTROL_XAXIS:
    imodv_rotate_model(a, -a->deltaRot, 0, 0);
    break;
  case IMODV_CONTROL_YAXIS:
    imodv_rotate_model(a, 0, a->deltaRot, 0);
    break;
  case -IMODV_CONTROL_YAXIS:
    imodv_rotate_model(a, 0, -a->deltaRot, 0);
    break;
  case IMODV_CONTROL_ZAXIS:
    imodv_rotate_model(a, 0, 0, a->deltaRot);
    break;
  case -IMODV_CONTROL_ZAXIS:
    imodv_rotate_model(a, 0, 0, -a->deltaRot);
    break;
  }
  imodvDraw(Imodv);
}

/* Respond to a new rotation angle typed in to text box */
void imodvControlAxisText(int axis, float rot)
{
  ImodvApp *a = Imodv;
  int m, mstrt, mend;
  if (!a->moveall) {
    mstrt = a->curMod;
    mend = mstrt + 1;
  } else {
    mstrt = 0;
    mend = a->numMods;
  }
  
  switch(axis){
  case IMODV_CONTROL_XAXIS:
    for(m = mstrt; m < mend; m++)
      a->mod[m]->view->rot.x = rot;
    lastX = -999;
    break;
  case IMODV_CONTROL_YAXIS:
    for(m = mstrt; m < mend; m++)
      a->mod[m]->view->rot.y = rot;
    lastY = -999;
    break;
  case IMODV_CONTROL_ZAXIS:
    for(m = mstrt; m < mend; m++)
      a->mod[m]->view->rot.z = rot;
    lastZ = -999;
    break;
  }
  for(m = mstrt; m < mend; m++) {
    imodMatUniqueRotationPt(&a->mod[m]->view->rot);
  }
  imodvNewModelAngles(&a->imod->view->rot);
  imodvDraw(a);
}

/* A change in the rotation rate slider*/
void imodvControlRate(int value)
{
  Imodv->deltaRot = value;
  /* DNM 11/3/03: do not change movie rates with new constant-speed scheme */

  if (Imodv->mainWin->mRotationTool)
    Imodv->mainWin->mRotationTool->setStepLabel(Imodv->deltaRot / 10.);
}

/* Change in the movie spped */
void imodvControlSpeed(float value)
{
  Imodv->movieSpeed = value;
}

void imodvControlIncSpeed(int step)
{
  if (step > 0) {
    Imodv->movieSpeed *= IMODV_ROTATION_FACTOR;
    if (Imodv->movieSpeed > 360.)
      Imodv->movieSpeed = 360.;
  } else {
    Imodv->movieSpeed /= IMODV_ROTATION_FACTOR;
    if (Imodv->movieSpeed < 3.6)
      Imodv->movieSpeed = 3.6;
  }
  if (dialog)
    dialog->setSpeedText(Imodv->movieSpeed);
}


/* receive the signal that the dialog is really closing, and set to NULL */
void imodvControlClosing(void)
{
  imodvDialogManager.remove((QWidget *)dialog);
  dialog = NULL;
}

/* Receive the signal to quit from the dialog box */
void imodvControlQuit(void)
{
  dialog->close();
}

////////////////////////////////////////
// Functions called from other parts of imodv
//

// Change both step size and rotation speed
void imodvControlChangeSteps(ImodvApp *a, int delta)
{
  float newval;
  if (delta > 0)
    newval = B3DNINT(a->deltaRot * IMODV_ROTATION_FACTOR);
  else
    newval = B3DNINT(a->deltaRot / IMODV_ROTATION_FACTOR);
  if (newval == a->deltaRot)
    newval += delta;
  newval = B3DMAX(1, newval);
  imodvControlSetArot(a, newval);
  imodvControlIncSpeed(delta);
}

/* Set a new rotation rate */
void imodvControlSetArot(ImodvApp *a, int newval)
{
  if (newval > ROTATION_MAX)
    newval = ROTATION_MAX;
  imodvControlRate(newval);
  /* DNM 11/3/03: do not change movie rates with new constant-speed scheme */

  if (dialog)
    dialog->setRotationRate(a->deltaRot);
}

/* Set the clipping, perspective, and z-scale sliders */
/* It turns out this is called on every display, so consider making a call
   only if something changes */
void imodvControlSetView(ImodvApp *a)
{
  if (!a->imod) return;
          
  /*DNM 11/30/02: should this really be an int?   are these radians? */
  a->fovy  = (int)a->imod->view->fovy;
  a->cnear = (int)(a->imod->view->cnear * 1000.0 + 0.5);
  a->cfar  = (int)(a->imod->view->cfar * 1000.0 + 0.5);
  if (!dialog) 
    return;

  /* Do not worry about sending out-of-range values.  Qt silently enforces
   its limits.  The only one that would get out of range is zscale, which the
   user might need.  If the user doesn't touch the slider, it stays at the set
   value; if they do, it snaps to the limit. */
  dialog->setViewSlider(IMODV_CONTROL_NEAR, a->cnear);
  dialog->setViewSlider(IMODV_CONTROL_FAR, a->cfar);
  dialog->setViewSlider(IMODV_CONTROL_FOVY, a->fovy);
  dialog->setViewSlider(IMODV_CONTROL_ZSCALE, 
                        (int) (a->imod->zscale * 100.0 + 0.5));
  dialog->setKickBox((a->imod->view->world & WORLD_KICKOUT_CLIPS) != 0);
}

void imodvControlUpdate(ImodvApp *a)
{
  float scale;
  if (!dialog) 
    return;

  /* Only update the text boxes if they change */
  if (lastX != a->imod->view->rot.x) {
    lastX = a->imod->view->rot.x;
    dialog->setAxisText(IMODV_CONTROL_XAXIS, lastX);
  }
  if (lastY != a->imod->view->rot.y) {
    lastY = a->imod->view->rot.y;
    dialog->setAxisText(IMODV_CONTROL_YAXIS, lastY);
  }
  if (lastZ != a->imod->view->rot.z) {
    lastZ = a->imod->view->rot.z;
    dialog->setAxisText(IMODV_CONTROL_ZAXIS, lastZ);
  }

  scale = 0.5 * B3DMIN(a->winx, a->winy) / a->imod->view->rad;
  if (lastScale != scale) {
    lastScale = scale;
    dialog->setScaleText(lastScale);
  }
}

/* function for opening, closing, or raising the window */
int imodv_control(ImodvApp *a, int state)
{
  if (!state){
    if (dialog)
      dialog->close();
    return -1;
  }

  if (dialog){
    dialog->raise();
    return -1;
  }
  
  dialog = new imodvControlForm(imodvDialogManager.parent(IMODV_DIALOG), 
                                Qt::Window);
  if (!dialog){
    dia_err("Failed to create 3dmodv controls window!");
    return(-1);
  }
  setModvDialogTitle(dialog, "3dmodv Controls: ");

  imodvDialogManager.add((QWidget *)dialog, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)dialog, IMODV_DIALOG);

  lastX = lastY = lastZ = lastScale = -999.;
  imodvControlUpdate(a);
  imodvControlSetArot(a, a->deltaRot);
  imodvControlSetView(a);
  dialog->setSpeedText(a->movieSpeed);
    
  return(0);
}
