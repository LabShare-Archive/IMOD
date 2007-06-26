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
#ifndef WIN_SUPPORT_H_
#define WIN_SUPPORT_H_

#include "imodP.h"

class QGLWidget;

#define BM_WIDTH 16
#define BM_HEIGHT 16

void setControlAndLimits(ImodView *iv, int inCtrlId,int &recordSubarea);
void stepZoom(ImodView *iv, int inCtrlId, int &recordSubarea, float &zoom, int step,QGLWidget *gl);
void draw(QGLWidget *gl);

#endif /*WIN_SUPPORT_H_*/
 /*
$Log$
*/