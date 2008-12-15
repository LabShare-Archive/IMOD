/*   imodv_gfx.h  -  declarations for imodv_gfx.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */

/*  

$Log$
Revision 4.3  2007/11/10 04:07:10  mast
Changes for setting snapshot directory

Revision 4.2  2003/12/30 06:31:46  mast
Make swapbuffers globally available

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2002/12/17 17:41:21  mast
initial creation

*/

#ifndef IMODV_GFX_H
#define IMODV_GFX_H
#include "qstring.h"

typedef struct __imodv_struct ImodvApp;
class ImodvGL;

int imodv_auto_snapshot(QString fname, int format_type);
void imodvResetSnap();
int imodv_winset(ImodvApp *a);
void imodvDraw(ImodvApp *a);
void imodvPaintGL();
void imodvResizeGL(ImodvGL *GLw, int winx, int winy);
void imodvInitializeGL();
void imodv_setbuffer(ImodvApp *a, int db, int stereo);
void imodv_swapbuffers(ImodvApp *a);

#endif
