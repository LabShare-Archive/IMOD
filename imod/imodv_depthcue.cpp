/*  IMOD VERSION 2.40
 *
 *  imodv_depthcue.cpp -- Depth cue dialog for imodv.
 *                        Companion form class is imodvDepthcueFrom in
 *                           formv_depthcue.cpp
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

#include <qgl.h>
#include "formv_depthcue.h"
#include "dia_qtutils.h"
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
  dia_vasmsg
    ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
     "Depth Cue Edit Dialog Help.\n"
     "~~~~~~~~~~~~~~~~~~~~~~~~"
     "\n\n",
     "\tDepth cueing is implemented by having objects blend into the "
     "background color with increasing distance from the viewer.  "
     "The range of distances over which this blending occurs is "
     "controlled by the sliders.\n",
     "\tThe [Start] slider controls the distance at which the blending "
     "starts - everything in front of this distance will not be "
     "fogged.  Typically you would move this slider until the frontmost "
     "features in your model appear at full intensity.\n",
     "\tThe [End] slider controls the distance beyond which everything "
     "will disappear.  Move this slider to change how fast the blending "
     "into the background occurs.\n",
     "\tUse the [Depth Cue] checkbox to turn the cueing on and off.\n",
     NULL);
  return;
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

  idcData.dia = new imodvDepthcueForm(NULL, NULL,// false,
				      Qt::WDestructiveClose |
				      Qt::WType_TopLevel);

  imodvDepthCueSetWidgets();
  imodvDialogManager.add((QWidget *)idcData.dia, IMODV_DIALOG);
  idcData.dia->show();
}

/****************************************************************************/
/* Dialog controls.                                                 */

void imodvDepthcueStart(int value)
{
  idcData.fstart = value;

  Imodv->imod->view->dcstart = (float)value * 0.01f;
  imodvDraw(Imodv);
}

void imodvDepthcueEnd(int value)
{
  idcData.fend = value;

  Imodv->imod->view->dcend = (float)value * 0.01f;
  imodvDraw(Imodv);
}

void imodvDepthcueToggle(int state)
{
  ImodvApp *a = Imodv;
 
  a->depthcue = state;
  if (!state)
    a->imod->view->world &= ~VIEW_WORLD_DEPTH_CUE;
  else
    a->imod->view->world |= VIEW_WORLD_DEPTH_CUE;

  imodvDraw(a);
}

/*
$Log$
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
