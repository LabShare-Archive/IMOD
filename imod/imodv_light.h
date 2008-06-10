/*   imodv_light.h  -  declarations for imodv_light.cpp
 *
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef IMODV_LIGHT_H
#define IMODV_LIGHT_H

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

/*

$Log$
Revision 4.5  2008/06/10 02:02:27  mast
Pass view to moveby

Revision 4.4  2005/06/20 22:20:29  mast
Pass transparency to light_adjust

Revision 4.3  2004/11/05 19:08:12  mast
Include local files with quotes, not brackets

Revision 4.2  2004/09/21 20:18:51  mast
Moved clipping function to imodv_ogl

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.1  2002/12/18 04:10:30  mast
initial creation

*/
