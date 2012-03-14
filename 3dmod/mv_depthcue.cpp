/*
 *  mv_depthcue.cpp -- Depth cue dialog for imodv.
 *                        Companion form class is imodvDepthcueFrom in
 *                           formv_depthcue.cpp
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

#include <qgl.h>
#include "formv_depthcue.h"
#include "dia_qtutils.h"
#include "imod.h"
#include "imodv.h"
#include "mv_gfx.h"
#include "mv_input.h"
#include "mv_depthcue.h"
#include "control.h"

static struct{
  imodvDepthcueForm *dia;
  ImodvApp  *a;
  int       fstart, fend;
  int       fmode;
     
}idcData = {0, 0, DEPTHCUE_MIN, 50, GL_LINEAR };

void imodvDepthcueHelp()
{
  imodShowHelpPage("depthcue.html#TOP");
}

// Send the values to the form; make sure they are within limits
void imodvDepthCueSetWidgets(void)
{
  ImodvApp *a = Imodv;
  int fstart, fend;

  a->depthcue = a->imod->view->world & VIEW_WORLD_DEPTH_CUE;
  if (idcData.dia){
    fstart = (int)(a->imod->view->dcstart * 100.0f);
    fend = (int)(a->imod->view->dcend * 100.0f);
    if (fstart < DEPTHCUE_MIN)
      fstart = DEPTHCUE_MIN;
    if (fstart > DEPTHCUE_MAX)
      fstart = DEPTHCUE_MAX;
    if (fend < DEPTHCUE_MIN)
      fend = DEPTHCUE_MIN;
    if (fend > DEPTHCUE_MAX)
    fend = DEPTHCUE_MAX;
    idcData.dia->setStates(a->depthcue, fstart, fend);
    idcData.fstart = fstart;
    idcData.fend   = fend;
  }
}

void imodvDepthcueDone()
{
  idcData.dia->close();
}

void imodvDepthcueClosing()
{
  imodvDialogManager.remove((QWidget *)idcData.dia);
  idcData.dia = NULL;
}

void imodvDepthCueEditDialog(ImodvApp *a, int state)
{
  idcData.a = Imodv;
  if (!state){
    if (idcData.dia)
      idcData.dia->close();
    return;
  }
  if (idcData.dia){
    idcData.dia->raise();
    return;
  }

  idcData.dia = new imodvDepthcueForm(imodvDialogManager.parent(IMODV_DIALOG), 
                                      Qt::Window);

  imodvDepthCueSetWidgets();
  imodvDialogManager.add((QWidget *)idcData.dia, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)idcData.dia, IMODV_DIALOG);
}

/****************************************************************************/
/* Dialog controls.                                                 */

void imodvDepthcueStartEnd(int value, bool end, bool dragging)
{
  static bool sliding = false;

  if (!sliding) {
    imodvRegisterModelChg();
    imodvFinishChgUnit();
  }
  sliding = dragging;
  if (end) {
    idcData.fend = value;
    Imodv->imod->view->dcend = (float)value * 0.01f;
  } else {
    idcData.fstart = value;
    Imodv->imod->view->dcstart = (float)value * 0.01f;
  }
  imodvDraw(Imodv);
}

void imodvDepthcueToggle(int state)
{
  ImodvApp *a = Imodv;
 
  imodvRegisterModelChg();
  imodvFinishChgUnit();
  a->depthcue = state;
  if (!state)
    a->imod->view->world &= ~VIEW_WORLD_DEPTH_CUE;
  else
    a->imod->view->world |= VIEW_WORLD_DEPTH_CUE;

  imodvDraw(a);
}
