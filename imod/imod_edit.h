/*   imod_edit.h  -  declarations for imod_edit.cpp
 *
 *   Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.3  2004/11/01 23:21:15  mast
Added selection list functions

Revision 4.2  2003/10/01 05:05:54  mast
change to rationalize location of ivw functions

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 23:34:22  mast
Initial creation

*/

#ifndef IMOD_EDIT_H
#define IMOD_EDIT_H

struct Mod_Object;
struct Mod_Index;
struct Mod_Point;
typedef struct ViewInfo ImodView;

int imod_setxyzmouse(void);
void imod_contour_move(int ob);
float imod_obj_nearest(ImodView *vi, struct Mod_Object *obj, 
                       struct Mod_Index *index,
                       struct Mod_Point *pnt,
                       float selsize);
int imod_redraw(ImodView *vw);
void imodSelectionListAdd(ImodView *vi, Iindex newIndex);
int imodSelectionListClear(ImodView *vi);
int imodSelectionListQuery(ImodView *vi, int ob, int co);
void imodSelectionListRemove(ImodView *vi, int ob, int co);
#endif
