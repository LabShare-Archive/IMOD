/*
 *  win support functions -- originally from xzap.cpp.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */
 
#include <qwidget.h>
#include <qgl.h>

#include "win_support.h"
#include "control.h"
#include "b3dgfx.h"

 // Set the control priority and set flag to record subarea and do float
void setControlAndLimits(ImodView *iv, int inCtrlId, int &recordSubarea)
{
  ivwControlPriority(iv, inCtrlId);
  recordSubarea = 1;
}

void stepZoom(ImodView *iv, int inCtrlId, int &recordSubarea, float &zoom,
              int step, QGLWidget *gl)
{
  setControlAndLimits(iv, inCtrlId, recordSubarea);
  zoom = b3dStepPixelZoom(zoom, step);
  draw(gl);
}

// This is the central drawing routine
void draw(QGLWidget *gl)
{
  gl->updateGL();
}
 
/*
$Log$
*/