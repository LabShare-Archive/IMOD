/*  IMOD VERSION 2.7.8
 *
 *  $Id$
 *
 *  Original Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
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

$Log$
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
void imodvControlZscale(int value);
void imodvControlRate(int value);
void imodvControlSpeed(float value);
void imodvControlIncSpeed(int step);
void imodvControlClip(int plane, int value);
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
