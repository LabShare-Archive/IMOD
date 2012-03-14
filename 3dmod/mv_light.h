/*   mv_light.h  -  declarations for mv_light.cpp
 *
 *
 *  $Id$
 */

#ifndef IMODV_LIGHT_H
#define IMODV_LIGHT_H

extern float Imodv_light_position[4];

// This has to be included because Iview has no struct name
#include "imodel.h"

/* light functions */
void light_init(void);
void light_getparam(int param, float *outValue);
void light_setparam(int param, double value);
void light_move(int *x, int *y);
void light_on(Iobj *obj, int modind);
void light_off(void);
void imodvSetLight(Iview *vw);
void light_moveby(Iview *vw, int x, int y);
void light_adjust(Iobj *obj, float r, float g, float b, int trans);

#endif

