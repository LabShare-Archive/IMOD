/*
 *  iview.h -- Image model view header.
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef IVIEW_H
#define IVIEW_H

#include "imodel.h"

#ifdef __cplusplus
extern "C" {
#endif

  Iview *imodViewNew(int size);
  void   imodViewDelete(Iview *vw);
  void   imodViewDefault(Iview *vw);
  void imodViewDefaultScale(Imod *imod, Iview *vw, Ipoint *imageMax, 
                            float binScale);
  void imodViewModelDefault(Imod *imod, Iview *vw, Ipoint *imageMax);
  int imodViewWrite(Iview *vw, FILE *fout, Ipoint *scale);
  int imodViewModelWrite(Imod *imod, Ipoint *scale);
  int imodViewModelRead(Imod *imod);
  int imodViewClipRead(Imod *imod);
  int imodViewModelNew(Imod *imod);
  void imodViewUse(Imod *imod);
  int imodViewStore(Imod *imod, int cview);
  void imodObjviewToObject(Iobjview *objview, Iobj *obj);
  void imodObjviewFromObject(Iobj *obj, Iobjview *objview);
  int imodObjviewComplete(Imod *imod);
  void imodObjviewDelete(Imod *imod, int index);
  void imodObjviewsFree(Imod *imod);


/* Image File view functions. */
  int imodIMNXRead(Imod *imod);
  int imodIMNXWrite(Imod *imod);
  IrefImage *imodIMNXNew();

#ifdef __cplusplus
}
#endif
#endif


/*

$Log$
Revision 3.6  2005/10/13 20:02:01  mast
Added an arg to a declaration

Revision 3.5  2004/11/05 18:52:53  mast
Include local files with quotes, not brackets

Revision 3.4  2004/09/28 15:12:03  mast
Move clipping plane functions to iplane

Revision 3.3  2004/09/21 20:08:59  mast
Added clipping plane declarations

Revision 3.2  2003/07/31 21:32:24  mast
New functions to operate on objviews

Revision 3.1  2003/06/27 20:11:38  mast
Add function to set a view to default scaling, redefine imodViewModelDefault

*/
