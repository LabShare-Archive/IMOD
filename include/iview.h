/*  IMOD VERSION 2.10
 *
 *  iview.h -- Image model view header.
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.4  2004/09/28 15:12:03  mast
    Move clipping plane functions to iplane

    Revision 3.3  2004/09/21 20:08:59  mast
    Added clipping plane declarations

    Revision 3.2  2003/07/31 21:32:24  mast
    New functions to operate on objviews

    Revision 3.1  2003/06/27 20:11:38  mast
    Add function to set a view to default scaling, redefine imodViewModelDefault

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
  void imodViewDefaultScale(Imod *imod, Iview *vw, Ipoint *imageMax);
  void imodViewModelDefault(Imod *imod, Iview *vw, Ipoint *imageMax);
  int imodViewWrite(Iview *vw, FILE *fout);
  int imodViewWrite(Iview *vw, FILE *fout);
  int imodViewModelWrite(Imod *imod);
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

#ifdef __cplusplus
}
#endif
#endif
