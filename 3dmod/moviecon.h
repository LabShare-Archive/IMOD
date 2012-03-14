/*   moviecon.h  -  declarations for moviecon.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 * 
 * $Id$
 */

#ifndef IMOD_MOVIECON_H
#define IMOD_MOVIECON_H

typedef struct ViewInfo ImodView;

int imcGetIncrement(ImodView *vw, int xyzt);
void imcGetStartEnd(ImodView *vw, int xyzt, int *stout, int *endout);
void imodMovieConDialog(ImodView *vw);
float imcGetInterval(void);
int imcGetMontageFactor(void);
void imcSetMontageFactor(int val);
bool imcGetScaleSizes(void);
void imcSetScaleSizes(bool state);
int imcGetSizeScaling(void);
void imcSetSizeScaling(int value);
bool imcGetSnapMontage(bool forDoing);
void imcSetSnapMontage(bool state);
bool imcGetSnapWholeMont(void);
void imcSetSnapWholeMont(bool state);
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
bool imcGetSlicerMontage(bool forDoing);
void imcSetSlicerMontage(bool state);
int imcGetSlicerMontFactor();
void imcSetSlicerMontFactor(int val);
bool imcGetScaleThicks(void);
void imcSetScaleThicks(bool state);
int imcGetThickScaling(void);
void imcSetThickScaling(int value);

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

#endif
