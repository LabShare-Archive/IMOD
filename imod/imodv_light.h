/*   imodv_light.h  -  declarations for imodv_light.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1.2.1  2002/12/18 04:10:30  mast
initial creation

*/

#ifndef IMODV_LIGHT_H
#define IMODV_LIGHT_H

// This has to be included because Iview has no struct name
#include <imodel.h>

/* light functions */
void light_init(void);
void light_getparam(int param, float *outValue);
void light_setparam(int param, double value);
void light_move(int *x, int *y);
int clip_obj(Iobj *obj, int flag, double zscale, double zoom);
void light_on(Iobj *obj);
void light_off(void);
void imodvSetLight(Iview *vw);
void light_moveby(int x, int y);
void light_adjust(Iobj *obj, float r, float g, float b);

#endif
