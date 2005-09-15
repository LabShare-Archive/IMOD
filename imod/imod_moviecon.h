/*   imod_moviecon.h  -  declarations for imod_moviecon.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.3  2004/11/29 19:25:21  mast
Changes to do QImage instead of RGB snapshots

Revision 4.2  2003/12/18 22:44:23  mast
New functions for starting at current point and slicer movies

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/14 21:44:51  mast
initial creation

*/
#ifndef IMOD_MOVIECON_H
#define IMOD_MOVIECON_H

typedef struct ViewInfo ImodView;

int imcGetIncrement(ImodView *vw, int xyzt);
void imcGetStartEnd(ImodView *vw, int xyzt, int *stout, int *endout);
void imodMovieConDialog(ImodView *vw);
float imcGetInterval(void);
int imcGetMontageFactor();
void imcSetMovierate(ImodView *vw, int newrate);
void imcResetAll(ImodView *vw);
void imcUpdateDialog();
int imcGetLoopMode(ImodView *vw);
int imcGetSnapshot(ImodView *vw);
int imcStartSnapHere(ImodView *vw);
int imcGetStarterID();
void imcSetStarterID(int value);
void imcSetSpecialLimits(int axis, int inStart, int inEnd);
void imcStartTimer(void);
void imcReadTimer(void);

void imcHelp();
void imcClosing();
void imcResetPressed();
void imcSliderChanged(int which, int value);
void imcAxisSelected(int which);
void imcExtentSelected(int which);
void imcStartHereSelected(int which);
void imcSnapSelected(int which);;
void imcRateEntered(float value);
void imcIncrementRate(int dir);
void imcSetMontageFactor(int val);

#endif
