/*   imod_edit.h  -  declarations for imod_edit.cpp
 *
 *   Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMOD_EDIT_H
#define IMOD_EDIT_H

struct Mod_Object;
struct Mod_Index;
struct Mod_Point;
typedef struct ViewInfo ImodView;

void imod_contour_move(int ob);
int imodContInsideCont(Iobj *obj, Icont *cont, Icont *outer, float zmin, float zmax);
int imodContInSelectArea(Iobj *obj, Icont *cont, Ipoint selmin, Ipoint selmax);
void imodMoveAllContours(ImodView *vi, int obNew);
float imod_obj_nearest(ImodView *vi, Iobj *obj, Iindex *index, Ipoint *pnt,
                       float selsize, Imat *mat = NULL);
float imodAllObjNearest(ImodView *vi, Iindex *index, Ipoint *pnt,
                          float selsize, Imat *mat = NULL);
void imodTrimContourLoops(Icont *cont, int openObj);
#endif
