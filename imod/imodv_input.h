/*   imodv_input.h  -  declarations for imodv_input.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMODV_INPUT_H
#define IMODV_INPUT_H

#include <qevent.h>
//Added by qt3to4:
#include <QKeyEvent>
#include <QMouseEvent>
#include "imodel.h"

typedef struct __imodv_struct ImodvApp;
typedef struct imodel_matrix Imat;

void imodvMovieTimeout();
void imodvKeyPress(QKeyEvent *event);
void imodvKeyRelease(QKeyEvent *event);
void imodvMousePress(QMouseEvent *event);
void imodvMouseRelease(QMouseEvent *event);
void imodvMouseMove(QMouseEvent *event);
void imodv_rotate_model(ImodvApp *a, int x, int y, int z);
void imodv_zoomd(ImodvApp *a, double zoom);
int imodv_sys_time(void);
void imodvInputRaise();
void imodvResolveRotation(Imat *mat, float x, float y, float z);
void clipCenterAndAngles(ImodvApp *a, Ipoint *clipPoint, Ipoint *clipNormal, 
                         Ipoint *cen, double &alpha, double &beta);
  
#endif

/*  

$Log$
Revision 4.6  2007/11/16 03:12:47  mast
Foxed log format

Revision 4.5  2007/09/20 22:06:55  mast
Changes for visualizing clipping plane

    
Revision 4.4  2007/06/13 15:19:21  mast
Made function for computing matrix from angle stpes

Revision 4.3  2003/11/12 18:54:56  mast
moved quit call out, added raise call

Revision 4.2  2003/02/27 23:12:09  mast
Change type of imodv_sys_time

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.6  2003/01/29 01:29:05  mast
remove imodv_exit

Revision 1.1.2.5  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.4  2003/01/13 07:21:38  mast
Changes to use new dialog manager class

Revision 1.1.2.3  2002/12/30 06:48:32  mast
Add widget list capability

Revision 1.1.2.2  2002/12/17 21:38:18  mast
include time.h so clock_t is defined

Revision 1.1.2.1  2002/12/17 17:41:01  mast
initial creation

*/
