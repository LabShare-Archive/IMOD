/*   mv_gfx.h  -  declarations for mv_gfx.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */

#ifndef IMODV_GFX_H
#define IMODV_GFX_H
#include "qstring.h"

extern Ipoint ImodvCurModLight;

typedef struct __imodv_struct ImodvApp;
class ImodvGL;

int imodv_auto_snapshot(QString fname, int format_type);
void imodvResetSnap();
int imodv_winset(ImodvApp *a);
void imodvDraw(ImodvApp *a);
void imodvPaintGL();
void imodvResizeGL(ImodvGL *GLw, int winx, int winy);
void imodvInitializeGL();
void imodv_setbuffer(ImodvApp *a, int db, int stereo, int alpha);
void imodv_swapbuffers(ImodvApp *a);

#endif
