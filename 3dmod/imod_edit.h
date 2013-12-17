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

void imod_contour_move(int ob);
int imodContInsideCont(Iobj *obj, Icont *cont, Icont *outer, float zmin, float zmax);
int imodContInSelectArea(Iobj *obj, Icont *cont, Ipoint selmin, Ipoint selmax);
void imodMoveAllContours(ImodView *vi, int obNew);
float imod_obj_nearest(ImodView *vi, Iobj *obj, Iindex *index, Ipoint *pnt,
                       float selsize, int ctime, Imat *mat = NULL);
float imodAllObjNearest(ImodView *vi, Iindex *index, Ipoint *pnt,
                        float selsize, int ctime, Imat *mat = NULL);
void imodTrimContourLoops(Icont *cont, int openObj);
bool imodContourIsPlanar(Icont *cont, int plane);
int imodSurfaceIsPlanar(Iobj *obj, int surface, int time, int plane);
int imodCheckSurfForNewCont(Iobj *obj, Icont *cont, int time, int plane);
#endif
