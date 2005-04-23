/*
 *  iobj.h -- Image model object header.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

    $Date$

    $Revision$

    $Log$
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
#define IMOD_OBJFLAG_2DPOINT (1l << 6)  /* Turns points off in 2d.           */
#define IMOD_OBJFLAG_3DPOINT (1l << 7)  /* Turns points on in 3d.            */
#define IMOD_OBJFLAG_FILL    (1l << 8)  /* Turns on drawing filled polygons. */
#define IMOD_OBJFLAG_OFF     (1l << 1)  /* Turns object line drawing off     */
#define IMOD_OBJFLAG_MESH    (1l << 10) /* Draw mesh in 3D, imod view        */
#define IMOD_OBJFLAG_LINE    (1l << 11) /* Draw lines in 3D, imod view       */
#define IMOD_OBJFLAG_DCUE    (1l << 2)  /* Draw using depth cue.             */
#define IMOD_OBJFLAG_LIGHT   (1l << 13) /* Use lighting for rendering in 3D  */
#define IMOD_OBJFLAG_FCOLOR  (1l << 14) /* Use Fill color                    */
#define IMOD_OBJFLAG_ANTI_ALIAS (1l << 15) /* Render using anti alias. */
#define IMOD_OBJFLAG_SCALAR  (1l << 16) /* Normals have magnitude. */
#define IMOD_OBJFLAG_MCOLOR  (1l << 17) /* Use color map for scalar values.  */
#define IMOD_OBJFLAG_TIME    (1l << 18) /* Contours contain time data.       */
#define IMOD_OBJFLAG_TWO_SIDE   (1l << 19) /* Light both sides of surface    */
#define IMOD_OBJFLAG_THICK_CONT (1l << 20) /* Draw current contour thicker   */

/* macros for testing above flags. */
#define iobjConnect(flag) ( (~(flag)) & IMOD_OBJFLAG_SCAT)

#define iobjClose(flag) (((~(flag)) & IMOD_OBJFLAG_OPEN) && iobjConnect(flag))
#define iobjOpen(flag)  ((iobjConnect(flag)) && ((flag) & IMOD_OBJFLAG_OPEN))
#define iobjFill(flag)    ((flag)  & IMOD_OBJFLAG_FILL)
#define iobjOff(flag)     ((flag)  & IMOD_OBJFLAG_OFF )
#define iobjMesh(flag)    ((flag)  & IMOD_OBJFLAG_MESH)
#define iobjScat(flag)    ((flag)  & IMOD_OBJFLAG_SCAT)
#define iobjLine(flag)    (((~(flag)) & IMOD_OBJFLAG_LINE))
#define iobjTime(flag)    ((flag)  & IMOD_OBJFLAG_TIME)
#define iobjLight(flag)   ((flag)  & IMOD_OBJFLAG_LIGHT)
#define iobjDraw(flag)    ((~(flag))  & IMOD_OBJFLAG_OFF)
#define iobjFlagTime(o)   (iobjTime((o)->flags))


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

#define IobjFlagClosed    3
#define IobjFlagConnected 9
#define IobjFlagFilled    8
#define IobjFlagDraw      1
#define IobjFlagMesh      10
#define IobjFlagLine      11
#define IobjFlagTime      12

/*****************************************************************************/
/* imodel_object.c functions                                                 */
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
