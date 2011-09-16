/*   imodv_views.h  -  declarations for imodv_views.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.2  2003/06/27 19:59:05  mast
Remove call to go to default view

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.4  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.3  2002/12/30 06:45:28  mast
changes to control multiple draws

Revision 1.1.2.2  2002/12/23 04:51:01  mast
Qt version

Revision 1.1.2.1  2002/12/18 04:10:30  mast
initial creation

*/

#ifndef IMODV_VIEWS_H
#define IMODV_VIEWS_H

#define VIEW_LABEL_LENGTH  32

typedef struct __imodv_struct ImodvApp;

/* view editing functions */
void imodvUpdateModel(ImodvApp *a, bool setView);
void imodvViewEditDialog(ImodvApp *a, int state);
void imodvAutoStoreView(ImodvApp *a);
void imodvViewsHelp();
void imodvViewsDone();
void imodvViewsClosing();
void imodvViewsSave();;
void imodvViewsGoto(int item, bool draw, bool regChg = true);
void imodvViewsStore(int item);
void imodvViewsNew(const char *label);;
void imodvViewsDelete(int item, int newCurrent);
void imodvViewsLabel(const char *label, int item);;
void imodvViewsAutostore(int state);
void imodvViewsInitialize(struct Mod_Model *imod);

#endif

