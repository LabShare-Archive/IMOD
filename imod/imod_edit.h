/*   imod_edit.h  -  declarations for imod_edit.cpp
 *
 *   Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMOD_EDIT_H
#define IMOD_EDIT_H

struct Mod_Object;
struct Mod_Index;
struct Mod_Point;
typedef struct ViewInfo ImodView;

int imod_setxyzmouse(void);
void imod_contour_move(int ob);
void imodMoveAllContours(ImodView *vi, int obNew);
float imod_obj_nearest(ImodView *vi, Iobj *obj, Iindex *index, Ipoint *pnt,
                       float selsize, Imat *mat = NULL);
int imod_redraw(ImodView *vw);
void imodSelectionListAdd(ImodView *vi, Iindex newIndex);
int imodSelectionListClear(ImodView *vi);
int imodSelectionListQuery(ImodView *vi, int ob, int co);
void imodSelectionListRemove(ImodView *vi, int ob, int co);
void imodSelectionNewCurPoint(ImodView *vi, Imod *imod, Iindex indSave, 
                              int controlDown);
int imodNumSelectedObjects(ImodView *vi, int &minOb, int &maxOb);
#endif
/*
$Log$
Revision 4.6  2007/06/04 15:04:47  mast
Added optional matrix argument to nearest point function

Revision 4.5  2006/08/31 23:27:44  mast
Changes for stored value display

Revision 4.4  2004/11/21 05:50:34  mast
Switch from int to float for nearest point distance measurement

Revision 4.3  2004/11/01 23:21:15  mast
Added selection list functions

Revision 4.2  2003/10/01 05:05:54  mast
change to rationalize location of ivw functions

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 23:34:22  mast
Initial creation

*/
