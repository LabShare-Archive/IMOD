/*  IMOD VERSION 2.02
 *
 *  iobj.h -- Image model object header.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine    *
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
    Revision 3.1  2003/06/27 20:10:28  mast
    Add functions to set object color and get specific contour from object

*/

#ifndef IOBJ_H
#define IOBJ_H

#include <imodel.h>

/*****************************************************************************/
/* object bit flags.  Default values are all off.                            */
/* Type of data, all contours in object default to this data type.           */
#define IMOD_OBJFLAG_OPEN (1l << 3)  /* Object contains Open/Closed contours */
#define IMOD_OBJFLAG_WILD (1l << 4)  /* No constraints on contour data       */
#define IMOD_OBJFLAG_OUT  (1l << 5)  /* Look at data outside contour         */
#define IMOD_OBJFLAG_SCAT (1l << 9)  /* Scatterd points, don't connect.      */
#define IMOD_OBJFLAG_VIRT (1l << 12) /* Each point in contour represents the */
                                     /* center of a virtual contour or mesh. */
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
#define iobjVirtual(flag) ((flag)  & IMOD_OBJFLAG_VIRT) 
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
Iobj *imodObjectGet(Imod *imod);
Iobj *imodObjectGetFirst(Imod *imod);
Iobj *imodObjectGetNext(Imod *imod);


int   imodObjectSort(Iobj *obj);
int   imodObjectAddContour(Iobj *obj, Icont *ncont);
Icont *imodObjectGetContour(Iobj *inObject, int inIndex);
int   imodObjectSkin(Iobj *obj, Ipoint *scale);
void  imodObjectDefault(Iobj *obj);

float imodObjectVolume(Iobj *obj);
Iobj *imodObjectClip(Iobj *obj, Iplane *plane, int planes);
int   imodObjectGetBBox(Iobj *obj, Ipoint *ll, Ipoint *ur);
int   imodObjectRemoveContour(Iobj *obj, int index);

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

/* defunct */
int   imodObjectVirtIn(Iobj *obj);
int   imodObjectVirtOut(Iobj *obj);

int   imod_object_write_ascii(Iobj *obj, char *filename);
Iobj *imodel_object_get(Imod *mod);     
int   imodel_object_centroid(Iobj *obj, struct Mod_Point *rcp);
Iobj *imodObjectCreateThresholdData
(unsigned char **idata, int nx, int ny, int nz,  
double highthresh, double lowthresh, 
int dim, int minsize, int maxsize);

#ifdef __cplusplus
}
#endif
#endif /* iobj.h */
