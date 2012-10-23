/*   mv_input.h  -  declarations for mv_input.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMODV_INPUT_H
#define IMODV_INPUT_H

#include <qevent.h>
//Added by qt3to4:
#include <QKeyEvent>
#include <QMouseEvent>
#include "imodel.h"

typedef struct __imodv_struct ImodvApp;
typedef struct imodel_matrix Imat;

void imodvMovieTimeout();
void imodvKeyPress(QKeyEvent *event);
void imodvKeyRelease(QKeyEvent *event);
void imodvMousePress(QMouseEvent *event);
void imodvMouseRelease(QMouseEvent *event);
void imodvMouseMove(QMouseEvent *event);
void imodvScrollWheel(QWheelEvent *e);
void imodvRotScaleMatrix(ImodvApp *a, Imat *mat, Imod *mod);
void imodv_rotate_model(ImodvApp *a, int x, int y, int z);
void imodv_zoomd(ImodvApp *a, double zoom);
int imodv_sys_time(void);
void imodvInputRaise();
void imodvResolveRotation(Imat *mat, float x, float y, float z);
void clipCenterAndAngles(ImodvApp *a, Ipoint *clipPoint, Ipoint *clipNormal, 
                         Ipoint *cen, double &alpha, double &beta);
  
#endif

