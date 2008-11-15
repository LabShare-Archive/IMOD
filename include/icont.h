/*
 *  icont.h -- Image model contour header.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */
#ifndef ICONT_H
#define ICONT_H

#include "imodel.h"

/*****************************************************************************/
/* contour flags                                                             */

#define ICONT_OPEN         (1l << 3 ) /* Don't close end points.             */
#define ICONT_WILD         (1l << 4 ) /* Points are not in one Z plane       */
#define ICONT_STIPPLED     (1l << 5 ) /* Draw stippled lines                 */
#define ICONT_CURSOR_LIKE  (1l << 6 ) /* Draw if mouse in window, ignore Z   */
#define ICONT_DRAW_ALLZ    (1l << 7 ) /* Draw an all planes; ignore Z        */
#define ICONT_MMODEL_ONLY  (1l << 8 ) /* Draw only in model mode             */
#define ICONT_NOCONNECT    (1l << 9 ) /* Don't connect contour points.       */
#define ICONT_SCANLINE     (1l << 17) /* Points are in pairs for scanline    */
#define ICONT_CONNECT_TOP  (1l << 18) /* Contour connected above in mkmesh.*/
#define ICONT_CONNECT_BOTTOM (1l << 19) /* Contour connected below in mkmesh.*/
#define ICONT_CONNECT_INVERT (1l << 20) /* Contour inverted to connect in mkmesh.*/
#define ICONT_TEMPUSE      (1l << 31)   /* Temporary marker flag */


#define IMOD_CONTOUR_CLOCKWISE -1
#define IMOD_CONTOUR_COUNTER_CLOCKWISE 1

#define ICONT_FIND_NOSORT 0
#define ICONT_FIND_SORTX  1
#define ICONT_FIND_SORTY  2
#define ICONT_FIND_SORTXY 3

#define imodContourBad(c,p) ((c)?(((c)->psize<(p))?1:0):1)
#define imodContourIsOpen(c) (c->flags & ICONT_OPEN)

typedef struct Nest_struct
{
  int co;        /* contour number */
  int level;     /* Level in from outside-most */
  int ninside;   /* Number inside */
  int *inside;   /* Numbers of contours inside */
  int noutside;  /* Number outside */
  int *outside;  /* Numbers of contours outside */
  Icont *inscan; /* Scan contour of object interior, for odd levels */
}Nesting;

/*****************************************************************************/
/* imodel_contour.c functions                                                */
/*****************************************************************************/
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Contour creation/deletion functions.
 * Delete funcions free all data in contour.
 */
Icont *imodContourNew(void);
Icont *imodContoursNew(int size);  /* Creates array of contours. */
void   imodContourDefault(Icont *cont);
int    imodContourDelete(Icont *cont);
int    imodContoursDelete(Icont *cont, int size);
int imodContoursDeleteToEnd(Iobj *obj, int keep);
int    imodContourClear(Icont *cont);
int imodContourClearPoints(Icont *cont);

/*
 * Contour data moving functions.
 */
void   imodContourSwap(Icont *c1, Icont *c2);
int    imodContourCopy(Icont *from, Icont *to);
Icont *imodContourDup(Icont *cont);


Ilabel *imodContourGetLabel(Icont *inContour);
void    imodContourSetLabel(Icont *inContour, Ilabel *inLabel);
int     imodContourGetMaxPoint(Icont *inContour);
Ipoint *imodContourGetPoints(Icont *inContour);
Ipoint *imodContourGetPoint(Icont *inContour, int inIndex);
void    imodContourSetPointData(Icont *inContour, Ipoint *inPoint, int inMax);

int     imodContourGetTimeIndex(Icont *inContour);
void    imodContourSetTimeIndex(Icont *inContour, int inTime);
int     imodContourGetSurface(Icont *inContour);
void    imodContourSetSurface(Icont *inContour, int inSurface);

void imodContourSetFlag(Icont *inContour, unsigned int inFlag, int inState);
int imodContourGetFlag(Icont *inContour, unsigned int inFlag);

/*
 *
 */
void   imodContourMakeDirection(Icont *cont, int direction);
Icont *imodContourJoin(Icont *c1, Icont *c2, int st1, int st2, int fill,
		       int counterdir);
Icont *imodContourSplice(Icont *c1, Icont *c2, int p1, int p2);
Icont *imodContourBreak(Icont *cont, int p1, int p2);
Icont *imodContourFill(Icont *cont);
void   imodContourScale(Icont *cont, Ipoint *spoint);
void   imodContourRotateZ(Icont *cont, double rot);
int    imodContourShave(Icont *cont, double dist);
int    imodContourFindPoint(Icont *cont, Ipoint *point, int flag);
void   imodContourReduce(Icont *cont, float tol);

Icont *imodContoursFromImagePoints(unsigned char *data, unsigned char **imdata,
                                   int xsize, int ysize, int z, 
                                   unsigned char testmask, int diagonal,
                                   float threshold, int polarity, int *ncont);
int    imodContourAutoSort(Icont *cont);
int    imodContourStrip(Icont *cont);

float  imodContourArea(Icont *cont);
float  imodContourLength(Icont *cont, int closed);
int    imodContourGetBBox(Icont *cont, Ipoint *ll, Ipoint *ur);
double imodContourLongAxis(Icont *cont, float precision, float *aspect,
			   float *longaxis);
double imodContourPrincipalAxis(Icont *cont);
double imodContourCircularity(Icont *cont);
int imodContourZValue(Icont *cont);
int imodContourFitPlane(Icont *cont, Ipoint *scale, Ipoint *norm, float *dval,
                        double *alpha, double *beta);

/* calculates the center of mass values for cont and puts them in rpt. */
int    imodContourCenterOfMass(Icont *cont, Ipoint *rpt);

double imodContourCenterMoment(Icont *cont, Ipoint *org, int a, int b);

/* 
 * returns index of point in cont that is nearest to pnt. 
 */
int    imodContourNearest(Icont *cont, Ipoint *pnt);

/****************************************************************************
 * All functions with ContZ in their name assume all z values are the same.
 */


/*
 * Returns: IMOD_CONTOUR_COUNTER_CLOCKWISE, IMOD_CONTOUR_CLOCKWISE
 *          or 0 if undefined.
 */
int    imodContZDirection(Icont *cont);


char  *imodContourGetName(Icont *inContour);
double imodContourMoment(Icont *cont, int a, int b);     
int    imodContourUnique(Icont *cont);
int imodContourSort3D(Icont *cont, Ipoint *scale);

/****************************************************************************/
/* in testing, old, or internal functions */
Icont *imodContourTracer(Icont *ic); /* broken */
void   imodel_contour_swapxy(Icont *cont);
int    imodel_contour_on(struct Mod_Contour *cont, int x, int y);
int    imodel_contour_unique(Icont *cont);
int    imodel_contour_sortx (Icont *cont, int bgnpt, int endpt);
int    imodel_contour_sorty (Icont *cont, int bgnpt, int endpt);
int    imodel_contour_sortz (Icont *cont, int bgnpt, int endpt);
int    imodel_contour_sort  (Icont *cont);
int    imodel_contour_invert  (Icont *cont);
void   imodel_contour_check_wild(Icont *cont);
void   imodel_contour_whole (Icont *cont);
void   imodContourFlatten(Icont *cont);
Icont *imodel_contour_scan  (Icont *ocont);
Icont *imodel_contour_double(Icont *cont);
double imodel_contour_length(struct Mod_Contour *cont);
int imodel_contour_nearest(struct Mod_Contour *cont, int x, int y);
int imodel_contour_overlap(struct Mod_Contour *c1, struct Mod_Contour *c2);
int imodel_scans_overlap(Icont *cs1, Ipoint pmin1, Ipoint pmax1,
			 Icont *cs2, Ipoint pmin2, Ipoint pmax2);
int imodel_overlap_fractions(Icont **cs1p, Ipoint pmin1, Ipoint pmax1,
			     Icont **cs2p, Ipoint pmin2, Ipoint pmax2,
			     float *frac1, float *frac2);
int imodel_contour_centroid(struct Mod_Contour *icont, struct Mod_Point *rcp,
			    double *rtw);
int imodel_contour_newsurf(struct Mod_Object *obj, struct Mod_Contour *cont);
int imodel_unused_surface(struct Mod_Object *obj);

int imodel_contour_area(struct Mod_Contour *icont);

int imodContourMakeZTables(Iobj *obj, int incz, unsigned int clearFlag, 
                           int **contzp, int **zlistp, int **numatzp, 
                           int ***contatzp, int *zminp, int *zmaxp, 
                           int *zlsizep, int *nummaxp);
void imodContourFreeZTables(int *numatz, int **contatz, int *contz, int *zlist,
                            int zmin, int zmax);
int imodContourCheckNesting(int co, int eco, Icont **scancont, Ipoint *pmin,
                            Ipoint *pmax, Nesting **nests, int *nestind,
                            int *numnests, int *numwarn);
void imodContourFreeNests(Nesting *nests, int numnests);
void imodContourNestLevels(Nesting *nests, int *nestind, int numnests);

#ifdef __cplusplus
}
#endif
#endif /* icont.h */

/*
$Log$
Revision 3.17  2008/05/27 05:30:34  mast
Changed call for contour from points

Revision 3.16  2008/01/14 19:44:15  mast
Added a function for Andrew

Revision 3.15  2008/01/09 05:58:41  mast
Added 3D version of contour sort

Revision 3.14  2007/12/04 18:28:05  mast
Added new draw flags for contours

Revision 3.13  2007/11/27 21:28:47  mast
Added functions for getting and setting bit flags in contour

Revision 3.12  2007/11/27 17:52:30  mast
Add stippled falg

Revision 3.11  2006/10/11 04:06:36  mast
Changed to plane fitting from mean normal routine

Revision 3.10  2006/09/12 15:20:15  mast
Added mean normal routine, cleaned up flags

Revision 3.9  2006/09/05 14:23:53  mast
Renamed imodel_contour_clear

Revision 3.8  2006/09/01 20:50:18  mast
Added flatten function

Revision 3.7  2005/06/26 19:41:31  mast
Changed break routine call

Revision 3.6  2005/04/23 23:36:23  mast
Moved some functions into imodel.c

Revision 3.5  2005/03/30 02:23:30  mast
Eliminates dummy imodel_contour_move

Revision 3.4  2005/03/20 19:55:48  mast
Eliminating duplicate functions

Revision 3.3  2005/01/30 17:45:02  mast
changes arguments to imodel_overlap_fractions

Revision 3.2  2005/01/29 20:27:31  mast
Added nested contour routines

*/
