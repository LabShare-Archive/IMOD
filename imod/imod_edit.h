/*   imod_edit.h  -  declarations for imod_edit.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
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
int imod_obj_nearest(struct Mod_Object *obj, 
		     struct Mod_Index *index,
		     struct Mod_Point *pnt,
		     float selsize);
int ivwRedraw(ImodView *vw);
#endif
