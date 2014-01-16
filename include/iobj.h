/*
 *  iobj.h -- Image model object header.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
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
#define IMOD_OBJFLAG_POLY_CONT  (1l << 26) /* Draw contour with overlay polygon */
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
#define IOBJ_SYMF_ARROW   (1 << 2)   /* Draw arrowhead at end */

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
#define IobjSymType       39
#define IobjSymSize       40
#define IobjSymFlags      41

#define IobjFlagClosed    3
#define IobjFlagConnected 9
#define IobjFlagFilled    8
#define IobjFlagDraw      1
#define IobjFlagPntOnSec  7
#define IobjFlagMesh      10
#define IobjFlagLine      11
#define IobjFlagTime      12
#define IobjFlagExtraInModv 21

/* Indices to extra array.  Elements used temporarily by extra objects can be assigned 
   at top of array until the space is needed. */
#define IOBJ_EX_PNT_LIMIT  0
#define IOBJ_EX_2D_TRANS   1
#define IOBJ_EX_LASSO_ID   (IOBJ_EXSIZE - 1)

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
  int   imodObjectCopyClear(Iobj *from, Iobj *to);
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
  int imodObjectSortSurf(Iobj *obj);

  int   imodObjectGetMaxContour(Iobj *inObject);
  char *imodObjectGetName(Iobj *inObject);
  int   imodObjectSetName(Iobj *inObject, char *inName);
  Ilabel *imodObjectGetLabel(Iobj *obj);
  Ilabel *imodObjectNewLabel(Iobj *obj);
  int   imodObjectGetValue(Iobj *inObject, int inValueType);
  void  imodObjectSetValue(Iobj *inObject, int inValueType, int inValue);
  void  imodObjectGetColor(Iobj *inObject,
                           float *outRed, float *outGreen, float *outBlue);
  void  imodObjectSetColor(Iobj *inObject,
                           float inRed, float inGreen, float inBlue);

  Imesh *imodObjectGetMesh(Iobj *inObject, int inIndex);
  int    imodObjectAddMesh(Iobj *inObject, Imesh *inMesh);
  double imodObjectChecksum(Iobj *obj, int obNum);

  /* internal calls */

  int   imodel_object_centroid(Iobj *obj, struct Mod_Point *rcp);

#ifdef __cplusplus
}
#endif
#endif /* iobj.h */
