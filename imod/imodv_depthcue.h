/*   imodv_depthcue.h  -  declarations for imodv_depthcue.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1.2.3  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.2  2002/12/23 04:51:01  mast
Qt version

Revision 1.1.2.1  2002/12/18 04:10:30  mast
initial creation

*/

#ifndef IMODV_DEPTHCUE_H
#define IMODV_DEPTHCUE_H

typedef struct __imodv_struct ImodvApp;

#define DEPTHCUE_MIN 0
#define DEPTHCUE_MAX 100

/* depth cue functions */
void imodvDepthCueSet(void);
void imodvDepthCueSetWidgets(void);
void imodvDepthCueEditDialog(ImodvApp *a, int state);
void imodvDepthcueHelp();
void imodvDepthcueDone();
void imodvDepthcueClosing();
void imodvDepthcueStart(int value);
void imodvDepthcueEnd(int value);
void imodvDepthcueToggle(int state);

#endif
