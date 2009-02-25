/*   imod_info_cb.h  -  declarations for info-window related functions in
 *                      imod_info_cb.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMOD_INFO_CB_H
#define IMOD_INFO_CB_H

typedef struct ViewInfo ImodView;

void imodInfoNewOCP(int which, int value, int edited);
void imodInfoNewXYZ(int *values);
void imodInfoNewBW(int which, int value, int dragging);
void imodInfoFloat(int state);
void imodInfoSubset(int state);
void imodInfoMMSelected(int mode);
void imodInfoCtrlPress(int pressed);
void imod_info_setobjcolor(void);
void imod_info_setocp(void);
void imod_info_setxyz(void);
void imod_info_setbw(int black, int white);
int imod_info_bwfloat(ImodView *vw, int section, int time);
void imodInfoSetFloatFlags(int inFloat, int inSubset);
void imodInfoGetFloatFlags(int &outFloat, int &outSubset);
void imodInfoAutoContrast(int targetMean, int targetSD);
int imodInfoCurrentMeanSD(float &mean, float &sd);
void imodInfoSaveNextClear();
void imod_info_float_clear(int section, int time);
void show_status(char *info);
void imod_show_info(char *info, int line);
void imod_info_msg(char *top, char *bot);
void imod_info_forbid(void);
void imod_info_enable(void);
int imod_info_input(void);
void imod_set_mmode(int mode);
void imod_draw_window(void);
void imod_imgcnt(const char *string);
void imodStartAutoDumpCache();
void imodInfoUpdateOnly(int value);
 
#endif
/* 

$Log$
Revision 4.7  2009/01/02 16:07:57  mast
Change to const char for Qt4 port

Revision 4.6  2008/03/06 00:12:46  mast
Changes to allow settings of float and subarea checkboxes to be saved

Revision 4.5  2004/11/07 23:02:29  mast
Added routine to indicate saving next clear

Revision 4.4  2004/10/22 22:15:33  mast
Added function to start dumping of cache after each section load

Revision 4.3  2003/09/18 05:59:08  mast
Added functions for autocontrast

Revision 4.2  2003/02/27 19:35:02  mast
Remove unneeded imod_open function

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.3  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.2  2003/01/23 20:04:29  mast
took care of orphan

Revision 1.1.2.1  2003/01/13 01:04:51  mast
Initial creation

*/
