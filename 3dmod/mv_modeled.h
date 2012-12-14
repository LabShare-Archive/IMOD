/*   mv_modeled.h  -  declarations for mv_modeled.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMODV_MODELED_H
#define IMODV_MODELED_H

typedef struct __imodv_struct ImodvApp;
class QString;

void imodvPixelChanged();
void imeSetViewData(int wi);
void imodvModelEditDialog(ImodvApp *a, int state);
int imodvSelectModel(ImodvApp *a, int ncm);
void imodvModeledHelp();
void imodvModeledDone();
void imodvModeledClosing();
void imodvModeledNumber(int which);
void imodvModeledMove(int item);
void imodvModeledView(int item);
void imodvModeledEdit(int item);
void imodvModeledName(QString nameStr);
void imodvModeledScale(int update);
void imodvModelDrawRange(ImodvApp *a, int &mstart, int &mend);
void imodvModeledSameScale();

#endif
