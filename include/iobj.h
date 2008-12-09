/*
 *  iobj.h -- Image model object header.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef IOBJ_H
#define IOBJ_H

#include "imodel.h"

/*****************************************************************************/
/* object bit flags.  Default values are all off.                            */
/* Type of data, all contours in object default to this data type.           */
#define IMOD_OBJFLAG_OPEN (1l << 3)  /* Object contains Open/Closed contours */
#define IMOD_OBJFLAG_WILD (1l << 4)  /* No constraints on contour data       */
#define IMOD_OBJFLAG_OUT  (1l << 5)  /* Look at data outside contour         */
#define IMOD_OBJFLAG_SCAT (1l << 9)  /* Scatterd points, don't connect.      */

/* What/how to draw                                                          */
#define IMOD_OBJFLAG_FILL    (1l << 8)  /* Turns on drawing filled polygons. */
#define IMOD_OBJFLAG_OFF     (1l << 1)  /* Turns object line drawing off     */
#define IMOD_OBJFLAG_MESH    (1l << 10) /* Draw mesh in 3D, imod view        */
#define IMOD_OBJFLAG_NOLINE  (1l << 11) /* Do not draw lines in 3D mod view  */
#define IMOD_OBJFLAG_DCUE    (1l << 2)  /* Draw using depth cue.             */
#define IMOD_OBJFLAG_FCOLOR  (1l << 14) /* Use Fill color                    */
#define IMOD_OBJFLAG_FCOLOR_PNT (1l << 6)  /* Use fill color for spheres     */
#define IMOD_OBJFLAG_PNT_ON_SEC (1l << 7)  /* Show spheres only on-section   */
#define IMOD_OBJFLAG_PLANAR     (1l << 13) /* Keep open contours on planes   */
#define IMOD_OBJFLAG_ANTI_ALIAS (1l << 15) /* Render using anti alias. */
#define IMOD_OBJFLAG_USE_VALUE  (1l << 12) /* Modify draw with stored values */
#define IMOD_OBJFLAG_SCALAR  (1l << 16) /* Normals have magnitude. */
#define IMOD_OBJFLAG_MCOLOR  (1l << 17) /* Use color map for scalar values.  */
#define IMOD_OBJFLAG_TIME    (1l << 18) /* Contours contain time data.       */
#define IMOD_OBJFLAG_TWO_SIDE   (1l << 19) /* Light both sides of surface    */
#define IMOD_OBJFLAG_THICK_CONT (1l << 20) /* Draw current contour thicker   */
#define IMOD_OBJFLAG_EXTRA_MODV (1l << 21) /* Draw extra object in model view*/
#define IMOD_OBJFLAG_EXTRA_EDIT (1l << 22) /* Allow editing of extra object  */
#define IMOD_OBJFLAG_PNT_NOMODV (1l << 23) /* Draw no spheres in model view  */
#define IMOD_OBJFLAG_MODV_ONLY  (1l << 24) /* Draw extra only in model view */
#define IMOD_OBJFLAG_TEMPUSE    (1l << 31) /* For temporary use              */

#define IMOD_OBJFLAG_LINE IMOD_OBJFLAG_NOLINE

/* macros for testing above flags. */
#define iobjConnect(flag) ( (~(flag)) & IMOD_OBJFLAG_SCAT)

#define iobjClose(flag) (((~(flag)) & IMOD_OBJFLAG_OPEN) && iobjConnect(flag))
#define iobjOpen(flag)  ((iobjConnect(flag)) && ((flag) & IMOD_OBJFLAG_OPEN))
#define iobjFill(flag)    ((flag)  & IMOD_OBJFLAG_FILL)
#define iobjOff(flag)     ((flag)  & IMOD_OBJFLAG_OFF )
#define iobjMesh(flag)    ((flag)  & IMOD_OBJFLAG_MESH)
#define iobjScat(flag)    ((flag)  & IMOD_OBJFLAG_SCAT)
#define iobjLine(flag)    (((~(flag)) & IMOD_OBJFLAG_NOLINE))
#define iobjTime(flag)    ((flag)  & IMOD_OBJFLAG_TIME)
#define iobjDraw(flag)    ((~(flag))  & IMOD_OBJFLAG_OFF)
#define iobjFlagTime(o)   (iobjTime((o)->flags))
#define iobjPlanar(flag) (iobjClose(flag) || (iobjOpen(flag) && ((flag) & IMOD_OBJFLAG_PLANAR)))

/* Flags in other places in object structure */
#define MATFLAGS2_SKIP_LOW    1         /* Skip drawing below valblack */
#define MATFLAGS2_SKIP_HIGH   (1 << 1)  /* Skip drawing above valwhite */
#define MATFLAGS2_CONSTANT    (1 << 2)  /* Draw with constant color */

/* new flags for objects V1.2       */
/* symbol flags  for point display. */
#define IOBJ_SYM_CIRCLE   0
#define IOBJ_SYM_NONE     1
#define IOBJ_SYM_SQUARE   2
#define IOBJ_SYM_TRIANGLE 3
#define IOBJ_SYM_STAR     4
#define IOBJ_SYM_LAST     5

#define IOBJ_SYMF_FILL    1          /* Fill the symbol       */
#define IOBJ_SYMF_ENDS    (1 << 1)   /* Draw bgn/end symbols. */

/* old imod V1.1, use symbols instead. */
#define IMOD_OBJM_OFF      0
#define IMOD_OBJM_CIRCLE   1
#define IMOD_OBJM_SQUARE   2
#define IMOD_OBJM_TRIANGLE 3


/* plugin defines. */
#define IobjMaxContour    33
#define IobjLineWidth     34
#define IobjPointSize     35
#define IobjMaxMesh       36
#define IobjMaxSurface    37
#define IobjLineWidth2    38

#define IobjFlagClosed    3
#define IobjFlagConnected 9
#define IobjFlagFilled    8
#define IobjFlagDraw      1
#define IobjFlagMesh      10
#define IobjFlagLine      11
#define IobjFlagTime      12
#define IobjFlagExtraInModv 21

/* Indices to extra array */
#define IOBJ_EX_PNT_LIMIT  0

/*****************************************************************************/
/* iobj.c functions                                                          */
/*****************************************************************************/
#ifdef __cplusplus
extern "C" {
#endif

  Iobj *imodObjectsNew(int size);
  Iobj *imodObjectNew (void);
  int   imodObjectDelete(Iobj *obj);
  int   imodObjectsDelete(Iobj *obj, int size);
  int   imodObjectCopy(Iobj *from, Iobj *to);
  Iobj *imodObjectDup(Iobj *obj);


  int   imodObjectSort(Iobj *obj);
  int   imodObjectAddContour(Iobj *obj, Icont *ncont);
  int   imodObjectInsertContour(Iobj *obj, Icont *ncont, int index);
  Icont *imodObjectGetContour(Iobj *inObject, int inIndex);
  int   imodObjectSkin(Iobj *obj, Ipoint *scale);
  void  imodObjectDefault(Iobj *obj);

  float imodObjectVolume(Iobj *obj);
  Iobj *imodObjectClip(Iobj *obj, Iplane *plane, int planes);
  int   imodObjectGetBBox(Iobj *obj, Ipoint *ll, Ipoint *ur);
  int   imodObjectRemoveContour(Iobj *obj, int index);
  void  imodObjectCleanSurf(Iobj *obj);

  int   imodObjectGetMaxContour(Iobj *inObject);
  char *imodObjectGetName(Iobj *inObject);
  int   imodObjectSetName(Iobj *inObject, char *inName);
  int   imodObjectGetValue(Iobj *inObject, int inValueType);
  void  imodObjectSetValue(Iobj *inObject, int inValueType, int inValue);
  void  imodObjectGetColor(Iobj *inObject,
                           float *outRed, float *outGreen, float *outBlue);
  void  imodObjectSetColor(Iobj *inObject,
                           float inRed, float inGreen, float inBlue);

  Imesh *imodObjectGetMesh(Iobj *inObject, int inIndex);
  int    imodObjectAddMesh(Iobj *inObject, Imesh *inMesh);

  /* internal calls */

  int   imodel_object_centroid(Iobj *obj, struct Mod_Point *rcp);

#ifdef __cplusplus
}
#endif
#endif /* iobj.h */

/*

$Log$
Revision 3.19  2008/11/28 06:05:33  mast
New flag for drawing only in model view

Revision 3.18  2008/07/16 04:31:41  mast
Added new define for extra array index

Revision 3.17  2008/06/17 20:09:51  mast
Yet another flag defined

Revision 3.16  2008/05/22 15:38:44  mast
Added flag for editable extra object

Revision 3.15  2008/04/24 18:51:12  mast
Added flag for setting @D line width

Revision 3.14  2008/03/05 20:07:38  mast
Added flag for drawing extra object in model view

Revision 3.13  2007/09/22 00:05:55  mast
Added matflags2 defines

Revision 3.12  2007/06/08 04:44:29  mast
Added planar flag and macro to test for planar contours

Revision 3.11  2006/08/31 21:02:45  mast
Flag definitions

Revision 3.10  2006/06/09 20:30:17  mast
Added flag for osphere display on-section only

Revision 3.9  2005/09/11 19:19:15  mast
Added temporary flag

Revision 3.8  2005/06/06 17:27:23  mast
Dropped 2DPOINT and 3DPOINT flags and added FCOLOR_PNT flag

Revision 3.7  2005/04/23 23:36:23  mast
Moved some functions into imodel.c

Revision 3.6  2005/03/20 19:55:48  mast
Eliminating duplicate functions

Revision 3.5  2004/11/20 04:04:16  mast
cleaned up, removed virtual, added dup and insert contour functions

Revision 3.4  2004/11/05 19:15:51  mast
Include local files with quotes, not brackets

Revision 3.3  2004/09/21 20:09:30  mast
Added clean surface call

Revision 3.2  2004/04/28 05:30:24  mast
Added flag for drawing current contour thicker

Revision 3.1  2003/06/27 20:10:28  mast
Add functions to set object color and get specific contour from object

*/
