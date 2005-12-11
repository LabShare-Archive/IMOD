/*
 *  $Id$
 *
 *  Original Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.3  2004/11/21 06:07:49  mast
Changes for undo/redo

Revision 4.2  2003/11/04 04:41:54  mast
Add new calls for rotation speed

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.3  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.2  2002/12/17 18:34:31  mast
Adding declarations for all global functions

Revision 1.1.2.1  2002/12/05 16:30:38  mast
First addition to archive


*/

#ifndef IMODV_CONTROL_H
#define IMODV_CONTROL_H

typedef struct __imodv_struct ImodvApp;

#define IMODV_ROTATION_FACTOR 1.26

#define IMODV_CONTROL_NEAR 1
#define IMODV_CONTROL_FAR 2
#define IMODV_CONTROL_FOVY 3
#define IMODV_CONTROL_ZSCALE 4
#define IMODV_CONTROL_XAXIS 1
#define IMODV_CONTROL_YAXIS 2
#define IMODV_CONTROL_ZAXIS 3
void imodvControlAxisText(int axis, float rot);
void imodvControlScale(float value);
void imodvControlKickClips(bool state);
void imodvControlZscale(int value, bool dragging);
void imodvControlRate(int value);
void imodvControlSpeed(float value);
void imodvControlIncSpeed(int step);
void imodvControlClip(int plane, int value, bool dragging);
void imodvControlZoom(int zoom);
void imodvControlHelp(void);
void imodvControlStart(void);
void imodvControlAxisButton(int axisDir);
void imodvControlClosing(void);
void imodvControlQuit(void);

int imodv_control(ImodvApp *a, int state);
void imodvControlSetArot(ImodvApp *a, int newval);
void imodvControlSetView(ImodvApp *a);
void imodvControlUpdate(ImodvApp *a);


#endif /* IMODV_CONTROL_H */
