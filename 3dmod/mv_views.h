/*   mv_views.h  -  declarations for mv_views.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMODV_VIEWS_H
#define IMODV_VIEWS_H

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
void imodvViewsSetView(ImodvApp *a, int view, bool draw, bool external, 
                       bool regChg = true);
void imodvViewsStore(int item);
void imodvViewsNew(const char *label);;
void imodvViewsDelete(int item, int newCurrent);
void imodvViewsLabel(const char *label, int item);;
void imodvViewsAutostore(int state);
void imodvViewsInitialize(struct Mod_Model *imod);

#endif

