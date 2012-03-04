/*   mv_ogl.h  -  declarations for mv_ogl.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           
#ifndef IMODV_OGL_H
#define IMODV_OGL_H

typedef struct __imodv_struct ImodvApp;

void imodvDraw_models(ImodvApp *a);
void imodvDraw_model(ImodvApp *a, Imod *imod);
void imodvSelectVisibleConts(ImodvApp *a, int &pickedOb, int &pickedCo);
int sphereResForSize(float drawsize);
int imodvCheckContourDraw(Icont *cont, int co, int checkTime);
bool imodvCheckThickerContour(int co);

#endif
