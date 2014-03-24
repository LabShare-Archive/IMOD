/*   info_cb.h  -  declarations for info-window related functions in
 *                      info_cb.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  No more Log
 */                                                                           

#ifndef IMOD_INFO_CB_H
#define IMOD_INFO_CB_H

typedef struct ViewInfo ImodView;

void imodInfoNewOCP(int which, int value, int edited);
void imodInfoNewXYZ(int *values);
void imodInfoNewBW(int which, int value, int dragging);
void imodInfoNewLH(int which, int value, int dragging);
void imodInfoFloat(int state);
void imodInfoSubset(int state);
void imodInfoMMSelected(int mode);
void imodInfoCtrlPress(int pressed);
int imodInfoSliderToLevel(ImodView *vi, int slider);
void imod_info_setobjcolor(void);
void imod_info_setocp(void);
void imod_info_setxyz(void);
void imod_info_setbw(int black, int white);
int imod_info_bwfloat(ImodView *vw, int section, int time);
void imodInfoSetFloatFlags(int inFloat, int inSubset);
void imodInfoGetFloatFlags(int &outFloat, int &outSubset);
void imodInfoAutoContrast(int targetMean, int targetSD);
int imodInfoCurrentMeanSD(float &mean, float &sd, float &scaleLo, float &scaleHi);
void imodInfoSaveNextClear();
void imod_info_float_clear(int section, int time);
void show_status(const char *info);
void imod_show_info(const char *info, int line);
void imod_info_msg(const char *top, const char *bot);
void imod_info_forbid(void);
void imod_info_enable(void);
int imod_info_input(void);
void imod_set_mmode(int mode);
void imod_draw_window(void);
int imodQuitCheck(int cz);
void imod_imgcnt(const char *string);
void imodStartAutoDumpCache();
void imodInfoUpdateOnly(int value);
void imodInfoLimitSubarea(int leftXpad, int rightX, int leftYpad, int rightY,
                          int &ixStart, int &iyStart, int &nxUse, int &nyUse);
 
#endif
