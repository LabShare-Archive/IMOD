/*
 *  imodv_depthcue.cpp -- Depth cue dialog for imodv.
 *                        Companion form class is imodvDepthcueFrom in
 *                           formv_depthcue.cpp
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

#include <qgl.h>
#include "formv_depthcue.h"
#include "dia_qtutils.h"
#include "imod.h"
#include "imodv.h"
#include "imodv_gfx.h"
#include "imodv_input.h"
#include "imodv_depthcue.h"
#include "control.h"

static struct{
  imodvDepthcueForm *dia;
  ImodvApp  *a;
  int       fstart, fend;
  int       fmode;
     
}idcData = {0, 0, DEPTHCUE_MIN, 50, GL_LINEAR };

void imodvDepthcueHelp()
{
  imodShowHelpPage("depthcue.html");
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

/*
$Log$
Revision 4.6  2009/01/15 16:33:17  mast
Qt 4 port

Revision 4.5  2004/11/22 00:22:17  mast
Changed to use help page in Qt Assistant

Revision 4.4  2004/11/21 06:07:49  mast
Changes for undo/redo

Revision 4.3  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.2  2003/02/27 17:39:06  mast
Had to include qgl.h instead of GL/gl.h under windows

Revision 4.1  2003/02/10 20:29:01  mast
autox.cpp

Revision 1.1.2.7  2003/01/18 01:10:17  mast
add include of dia_qtutils

Revision 1.1.2.6  2003/01/13 07:21:38  mast
Changes to use new dialog manager class

Revision 1.1.2.5  2002/12/30 06:49:50  mast
rationalizing dialogs as widgets and using dialog list

Revision 1.1.2.4  2002/12/23 04:51:22  mast
Qt version

Revision 1.1.2.3  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 1.1.2.2  2002/12/17 18:33:19  mast
using new includes for imodv compoennts

Revision 1.1.2.1  2002/12/15 21:14:02  mast
conversion to cpp

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/
