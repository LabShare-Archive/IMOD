/*
 *  icont.h -- Image model contour header.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

$Log$
*/

#ifndef ICONT_H
#define ICONT_H

#include "imodel.h"

/*****************************************************************************/
/* contour flags                                                             */

#define ICONT_ISHOLE     (1l << 5 ) /* Data is a hole.                       */
#define ICONT_NOCONNECT  (1l << 9 ) /* Don't connect contour points.         */
#define ICONT_OPEN       (1l << 3 ) /* Don't close end points.               */
#define ICONT_WILD       (1l << 4 ) /* No sane constrains on data.           */
#define ICONT_VIRT       (1l << 12) /* Use virtual contours.                 */
#define ICONT_TYPEISTIME (1l << 15) /* Contour is point list with type=time  */
#define ICONT_PNTLABEL   (1l << 16)
#define ICONT_SCANLINE   (1l << 17) /* Points are in pairs for scanline.     */
#define ICONT_CONNECT_TOP    (1l << 18) /* Contour connected above in mkmesh.*/
#define ICONT_CONNECT_BOTTOM (1l << 19) /* Contour connected below in mkmesh.*/

/* DNM 6/19/01: obsolete; type is now used only for time; SCANLINE flag 
   is enough */
/* #define ICONT_TYPE_POINTLIST  0
#define ICONT_TYPE_EDGETABLE  1
#define ICONT_TYPE_LINESEG    2 */

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

/*
 *
 */
Icont *imodContourGet(struct Mod_Model *mod);
Icont *imodContourGetFirst(Imod *imod);
Icont *imodContourGetNext(Imod *imod);

void   imodContourMakeDirection(Icont *cont, int direction);
Icont *imodContourJoin(Icont *c1, Icont *c2, int st1, int st2, int fill,
		       int counterdir);
Icont *imodContourSplice(Icont *c1, Icont *c2, int p1, int p2);
Icont *imodContourBreak(Icont *cont, int p);
Icont *imodContourFill(Icont *cont);
void   imodContourScale(Icont *cont, Ipoint *spoint);
void   imodContourRotateZ(Icont *cont, double rot);
int    imodContourShave(Icont *cont, double dist);
int    imodContourFindPoint(Icont *cont, Ipoint *point, int flag);
void   imodContourReduce(Icont *cont, float tol);

Icont *imodContoursFromImagePoints(unsigned char *data, int xsize, int ysize,
				   int z, unsigned char testmask, 
				   int diagonal, int *ncont);
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

/****************************************************************************/
/* in testing, old, or internal functions */
Icont *imodContourTracer(Icont *ic); /* broken */
void   imodel_contour_swapxy(Icont *cont);
int    imodel_contour_clear(Icont *cont);
int    imodel_contour_mm(Icont *cont, Ipoint *max, Ipoint *min);
int    imodel_contour_on(struct Mod_Contour *cont, int x, int y);
int    imodel_contour_unique(Icont *cont);
int    imodel_contour_sortx (Icont *cont, int bgnpt, int endpt);
int    imodel_contour_sorty (Icont *cont, int bgnpt, int endpt);
int    imodel_contour_sortz (Icont *cont, int bgnpt, int endpt);
int    imodel_contour_sort  (Icont *cont);
int    imodel_contour_invert  (Icont *cont);
void   imodel_contour_check_wild(Icont *cont);
int    imodel_contour_shave (Icont *cont, double dist);
void   imodel_contour_whole (Icont *cont);
Icont *imodel_contour_scan  (Icont *ocont);
Icont *imodel_contour_double(Icont *cont);
double imodel_contour_length(struct Mod_Contour *cont);
int imodel_contour_nearest(struct Mod_Contour *cont, int x, int y);
int imodel_contour_overlap(struct Mod_Contour *c1, struct Mod_Contour *c2);
int imodel_scans_overlap(Icont *cs1, Ipoint pmin1, Ipoint pmax1,
			 Icont *cs2, Ipoint pmin2, Ipoint pmax2);
int imodel_overlap_fractions(Icont *cs1, Ipoint pmin1, Ipoint pmax1,
			     Icont *cs2, Ipoint pmin2, Ipoint pmax2,
			     float *frac1, float *frac2);
int imodel_contour_centroid(struct Mod_Contour *icont, struct Mod_Point *rcp,
			    double *rtw);
int imodel_contour_newsurf(struct Mod_Object *obj, struct Mod_Contour *cont);
int imodel_unused_surface(struct Mod_Object *obj);
int imodel_contour_move(void);
#define imodel_contour_create imodContourNew
#define imodel_contour_delete imodContourDelete
#define imodel_contour_get    imodContourGet

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
