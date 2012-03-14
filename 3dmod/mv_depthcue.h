/*   mv_depthcue.h  -  declarations for mv_depthcue.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
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
void imodvDepthcueStartEnd(int value, bool end, bool dragging);
void imodvDepthcueToggle(int state);

#endif
