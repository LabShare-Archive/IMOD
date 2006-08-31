/*   imodv_ogl.h  -  declarations for imodv_ogl.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2002/12/17 17:40:23  mast
initial creation

*/

#ifndef IMODV_OGL_H
#define IMODV_OGL_H

typedef struct __imodv_struct ImodvApp;

void imodvDraw_models(ImodvApp *a);
void imodvDraw_model(ImodvApp *a, Imod *imod);
void imodvSelectVisibleConts(ImodvApp *a, int &pickedOb, int &pickedCo);

#endif
