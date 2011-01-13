/* 
 *  skinobj.c -- Skins an object with branching and nesting analysis
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 * Log at end
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <time.h>
#include "mkmesh.h"
#include "istore.h"

#define CONNECT_TOP     ICONT_CONNECT_TOP
#define CONNECT_BOTTOM  ICONT_CONNECT_BOTTOM
#define CONNECT_BOTH    (ICONT_CONNECT_TOP & ICONT_CONNECT_BOTTOM)

#define connectTop(f)    ((f) & CONNECT_TOP)
#define connectBottom(f) ((f) & CONNECT_BOTTOM)
#define connectBoth(f)   ((connectTop(f)) && (connectBottom(f)))


static int getnextz(int *zlist, int zlsize, int cz);
static int    imeshDefaultCallback(int inStatus);

static Imesh *imeshContourCap(Iobj *obj, Icont *cont, int co, int side,
                              int inside, Ipoint *scale);

static float segment_separation(float l1, float u1, float l2, float u2);
static int mesh_open_tube_obj(Iobj *obj, Ipoint *scale, unsigned int flags,
                              double meshDiameter);
static Icont *makeDomeConts(Icont *lastCont, Ipoint *lastPt, Ipoint *nrot, Ipoint *scale, 
                            float diameter, float direction, int *numCont, Ipoint *capPt);
static int mesh_open_obj(Iobj *obj, Ipoint *scale, int incz, 
                         unsigned int flags, int skipPasses,
                         int zmin, int zmax, int *contz, int *zlist, 
                         int zlsize, int *numatz, int **contatz, Ipoint *pmin, 
                         Ipoint *pmax);

static int eliminate_overlap(Icont *c1, Icont *c2);
static Icont *connect_orphans(Iobj *obj, Icont *cout, int *list, int *used,
                              int *justinl, int njustin, int *inlist, 
                              int joindir,
                              int oddeven, int surf, int time, Ipoint *scale,
                              Icont **scancont, Ipoint *pmin, Ipoint *pmax);
static float evaluate_break(Icont *cout, int *list, int *used,
                            int *justinl, int njustin, Icont **scancont,
                            Ipoint *pmin, Ipoint *pmax, int st1, int st2,
                            float curmin, float pathlen, float fullarea,
                            int joindir, int areaonly, float *innerarea);
static void add_whole_nest(int nind, Iobj *obj, Nesting *nests, int *nestind,
                           int flag, int *tlist, int *ntop);
static Icont *join_all_contours(Iobj *obj, int *list, int ncont, 
                                int nsamelevel, int fill, int *olist, 
                                int nother, int nothersame, Icont *other);
static void subtract_scan_contours(Icont *cs1, Icont *cs2);
static int mesh_contours(Iobj *obj, Icont *bcont, Icont *tcont, int surf,
                         int time, Ipoint *scale, int inside, int bco, 
                         int tco);
static int break_contour_inout(Icont *cin, int st1, int st2,  int fill,
                               Icont **cout1, Icont **cout2);
static void segment_mm(Icont *cont, int ptst, int ptnd, float *xmin, 
                       float *xmax, float *ymin, float *ymax);
static void scan_points_to_segments(Icont *c1, Icont *c2, double *legalmin,
                                    double *dsqrmin, int *pt1, int *pt2, 
                                    float *tbest, Ipoint *close,
                                    int docheck, int samelevel, Iobj *obj,
                                    int *olist, int nother, int nothersame,
                                    Icont *other);
static int find_closest_contour(Icont *tcont, Icont *onecont, Iobj *obj, 
                                int *list, int ncont,
                                int *used, int *pt1min, int *pt2min, 
                                int *firstbest, float *tbest, Ipoint *close,
                                int *olist, int nother, int nothersame, 
                                Icont *other, int nsamelevel);
static int check_legal_joiner(Ipoint pt1, Ipoint pt2, Icont *c1, Icont *c2,
                              int samelevel, Iobj *obj, int *olist,
                              int nother, int nothersame, Icont *other);
static int cross_cont(Icont *cout, float x1s, float y1s, float x1e, float y1e,
                      int st1, int st2);
static void backoff_overlap(Icont *c1, Icont *c2);
static void interpolate_point(Ipoint pt1, Ipoint pt2, float frac, Ipoint *pt3);
static int robustCenterOfMass(Icont *cont, Ipoint *pt);
static void dump_lists(char *message, int *blist, int nb, int *tlist, int nt,
                       int *blook, int *tlook);
static void addMeshToObject(Iobj *obj, Icont *cont, Imesh *nmesh, unsigned int flags);

static int fastmesh = 0;
static unsigned int skinFlags;

static int    imeshDefaultCallback(int inStatus)
{
  int dummy = inStatus;
  return(0);
}

static clock_t timeval1, timeval2;

void skin_report_time(char *string)
{
  char *dummy = string;
  float elapsed;
  timeval2 = clock();
  elapsed = (float)(timeval2 - timeval1) / CLOCKS_PER_SEC;
  /*printf("%s: %.3f\n",string, elapsed); */
  timeval1 = timeval2;
}
          

static int getnextz(int *zlist, int zlsize, int cz)
{
  int z;

  for (z = 0; z < zlsize - 1; z++) {
    if (cz == zlist[z])
      return(zlist[z+1]);
  }
  /* DNM: this will work the same as when not connecting skips */
  if (cz == zlist[zlsize - 1])
    return (zlist[zlsize - 1] + 1);

  /* 11/15/08: nobody is testing for negative return, just return past the max */
  return(zlist[zlsize - 1] + 1);
}


static void dump_lists(char *message, int *blist, int nb, int *tlist, int nt,
                       int *blook, int *tlook)
{
  int i;
  printf("%s:\nbottom: ", message);
  for (i = 0; i < nb; i++)
    if (blook)
      printf(" %d", blook[blist[i]]+1);
    else
      printf(" %d", blist[i]+1);
  printf("\ntop: ");
  for (i = 0; i < nt; i++)
    if (tlook)
      printf(" %d", tlook[tlist[i]]+1);
    else
      printf(" %d", tlist[i]+1);
  printf("\n");
}

/*
 * connect open contours like a surface.
 */
static int mesh_open_obj(Iobj *obj, Ipoint *scale, int incz, 
                         unsigned int flags, int skipPasses,
                         int zmin, int zmax, int *contz, int *zlist, 
                         int zlsize, int *numatz, int **contatz, Ipoint *pmin, 
                         Ipoint *pmax)
{
  Imesh *nmesh = NULL;
  Icont *cont, *econt;
  int co,eco, nextz, iz, nextiz, i, j, k, indmin, needmat, matsize, zpass;
  float *sepmat;
  float minsep, xlap, ylap, singleSep;
  int indSingle, inside = 0;

  if (obj->flags & IMOD_OBJFLAG_OUT)
    inside = 1;

  fastmesh = 1;
  imeshSetSkinFlags(skinFlags, fastmesh);

  co = (obj->contsize + zmax - zmin) / (zmax + 1 - zmin);
  matsize = co * co;
  sepmat = (float *)malloc(matsize * sizeof(float));
  if (!sepmat)
    return 1;
     
  /* Do multiple passes for connections farther apart in Z */
  for (zpass = 1; zpass <= skipPasses; zpass++) {

    for (iz = 0; iz < zmax - zmin; iz++) {
      nextz = iz + zmin;
      for (i = 0; i < zpass; i++) {
        if (flags & IMESH_MK_SKIP) {
          nextz = getnextz(zlist, zlsize, nextz);
        } else {
          nextz += incz;
        }
        if (nextz > zmax)
          break;
      }
      if (nextz > zmax)
        continue;
      nextiz = nextz - zmin;
      needmat = numatz[iz] * numatz[nextiz];
      if (needmat > matsize) {
        sepmat = (float *)realloc(sepmat, needmat * sizeof(float));
        matsize = needmat;
        if (!sepmat)
          return 1;
      }

      for (i = 0; i < numatz[iz]; i++) {
        co = contatz[iz][i];
        cont = &(obj->cont[co]);
        if (connectTop(cont->flags))
          continue;
        
        for (j = 0; j < numatz[nextiz]; j++) {
          eco = contatz[nextiz][j];
          econt = &(obj->cont[eco]);
          if (connectBottom(econt->flags))
            continue;
          sepmat[i + j * numatz[iz]] = -1.;
          if (!cont->psize)
            continue;
          if (!econt->psize)
            continue;
          if ((flags & IMESH_MK_SURF) && (cont->surf != econt->surf))
            continue;
          if ((flags & IMESH_MK_TIME) && (cont->time != econt->time))
            continue;
          
          /* for a valid pair, compute a separation factor */
          xlap = segment_separation(pmin[co].x, pmax[co].x,
                                    pmin[eco].x, pmax[eco].x);
          ylap = segment_separation(pmin[co].y, pmax[co].y,
                                    pmin[eco].y, pmax[eco].y);
          if (ylap > xlap)
            xlap = ylap;
          sepmat[i + j * numatz[iz]] = xlap;
        }
      }

      for (;;) {
        
        /* Loop: find pair with minimum separation and mesh them; keep track
         of a minimum for pairs including single points and pairs not */
        minsep = 1.e30;
        singleSep = 1.e30;
        for (i = 0; i < needmat; i++) {
          if (sepmat[i] >= 0) {
            co = contatz[iz][i % numatz[iz]];
            eco = contatz[nextiz][i / numatz[iz]];
            if (obj->cont[co].psize == 1 || obj->cont[eco].psize == 1) {
              if (sepmat[i] < singleSep) {
                singleSep = sepmat[i];
                indSingle = i;
              }
            } else {
              if (sepmat[i] >= 0. && sepmat[i] < minsep) {
                minsep = sepmat[i];
                indmin = i;
              }
            }
          }
        }
        
        /* If only a single was found, use it.  This allows stray single 
           point to be rejected unless it logically pairs up with something */
        if (minsep > 1.e20 && singleSep < 1.e20) {
          indmin = indSingle;
          minsep = singleSep;
        }

        /* Done if no more pairs available */
        if (minsep > 1.e20)
          break;
        i = indmin % numatz[iz];
        j = indmin / numatz[iz];
        co = contatz[iz][i];
        cont = &(obj->cont[co]);
        eco = contatz[nextiz][j];
        econt = &(obj->cont[eco]);
        cont->flags |= (ICONT_OPEN | CONNECT_TOP);
        econt->flags |= (ICONT_OPEN |  CONNECT_BOTTOM);
        nmesh = imeshContoursCost(obj, cont, econt, scale, inside, co, eco);
        addMeshToObject(obj, cont, nmesh, flags);
        
        /* mark all pairs involving these two conts as unavailable */
        for (k = 0; k < numatz[nextiz]; k++)
          sepmat[i + k * numatz[iz]] = -1.;
        for (k = 0; k < numatz[iz]; k++)
          sepmat[k + j * numatz[iz]] = -1.;
      }
    }
  }

  if (flags & IMESH_MK_NORM) {
    obj->mesh = imeshReMeshNormal(obj->mesh, &(obj->meshsize), scale, 0);
    if (!obj->mesh)
      obj->meshsize = 0;
  } 

  imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);
  free(sepmat);
  free(pmin);
  free(pmax);
  return(0);
}


static float segment_separation(float l1, float u1, float l2, float u2)
{
  int minlen;
  minlen = u1 - l1;
  if (u2 - l2 < minlen)
    minlen = u2 - l2;
  if ((l1 >= l2 && u1 <= u2) || (l2 >= l1 && u2 <= u1))
    return (0.);
  if (l1 > u2)
    return (minlen + l1 - u2);
  if (l2 > u1)
    return (minlen + l2 - u1);
          
  if (u1 > u2) {
    minlen = u1 - u2;
    if (l1 - l2 < minlen)
      minlen = l1 - l2;
  } else {
    minlen = u2 - u1;
    if (l2 - l1 < minlen)
      minlen = l2 - l1;
  }
  return (minlen);
}

/*
 * Mesh an open object as tubes 
 */
static int mesh_open_tube_obj(Iobj *obj, Ipoint *scale, unsigned int flags,
                              double meshDiameter)
{
  Imesh *nmesh;
  Icont *cont, *nextcont, *domeCont1, *domeCont2;
  Icont *clst;
  Ipoint nrot, domeRot1, domeRot2, domeCap1, domeCap2;
  DrawProps defProps, contProps, ptProps, lastProps;
  int nextChange, stateFlags, changeFlags, lastState;
  int stateTest = CHANGED_COLOR | CHANGED_FCOLOR | CHANGED_TRANS;
  int co, pt, npt, ppt, slices, numDome1, numDome2;
  float tubeDiameter;

  istoreDefaultDrawProps(obj, &defProps);

  obj->meshsize = 0;
  obj->mesh = NULL;
     
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    if (cont->psize < 2)
      continue;
    clst = imodContoursNew(cont->psize);
          
    lastState = istoreContSurfDrawProps(obj->store, &defProps, &contProps, co,
                                        cont->surf, &stateFlags, &changeFlags);
    if (!(skinFlags & IMESH_MK_SURF))
      stateFlags = lastState;
    ptProps = contProps;
    nextChange = istoreFirstChangeIndex(cont->store);

    /* DNM 8/25/03: Set tube diameter here instead of in makeTubeCont and make 
       number of segments variable within limits */
    tubeDiameter = meshDiameter;
      
    for (pt = 0; pt < cont->psize; pt++) {

      if (pt == nextChange)
        nextChange = istoreNextChange(cont->store, &contProps, &ptProps, 
                                      &stateFlags, &changeFlags);
      if (meshDiameter <= 0.) {
        if (!meshDiameter)
          tubeDiameter = ptProps.linewidth;
        else if (meshDiameter < -1.0001)
          tubeDiameter = ptProps.symsize;
        else
          tubeDiameter = 2. * imodPointGetSize(obj, cont, pt);
        tubeDiameter = B3DMAX(1., tubeDiameter); 
      }
      slices = tubeDiameter / 2;
      if (slices < 12) 
        slices = 12;
      if (slices > 50)
        slices = 50;

      /* DNM: simplify and include scale in normal calculation */
               
      ppt = pt - 1;
      if (ppt < 0)
        ppt = 0;
      npt = pt + 1;
      if (npt == cont->psize)
        npt = pt;

      nrot.x = scale->x * (cont->pts[npt].x - cont->pts[ppt].x);
      nrot.y = scale->y * (cont->pts[npt].y - cont->pts[ppt].y);
      nrot.z = scale->z * (cont->pts[npt].z - cont->pts[ppt].z);

      imodPointNormalize(&nrot);
      makeTubeCont(&clst[pt], &cont->pts[pt], &nrot, scale,
             tubeDiameter, slices);

      /* Get the contours for dome caps if at start or end */
      if (!pt && (flags & IMESH_MK_CAP_DOME)) {
        domeCont1 = makeDomeConts(&clst[pt], &cont->pts[pt], &nrot, scale, 
                                  tubeDiameter, -1., &numDome1, &domeCap1);
        domeRot1 = nrot;
      }
      if (pt == cont->psize - 1 && (flags & IMESH_MK_CAP_DOME)) {
        domeCont2 = makeDomeConts(&clst[pt], &cont->pts[pt], &nrot, scale, 
                                  tubeDiameter, 1., &numDome2, &domeCap2);
        domeRot2 = nrot;
      }
    }

    /* Restart the state for drawing */
    lastState = istoreContSurfDrawProps(obj->store, &defProps, &contProps, co,
                                        cont->surf, &stateFlags, &changeFlags);
    if (!(skinFlags & IMESH_MK_SURF))
      stateFlags = lastState;
    ptProps = contProps;
    nextChange = istoreFirstChangeIndex(cont->store);
    ptProps.gap = 0;
    if (!nextChange)
      nextChange = istoreNextChange(cont->store, &contProps, &ptProps, 
                                      &stateFlags, &changeFlags);

    /* DNM 8/25/02: cap if flag is set*/
    if ((flags & IMESH_MK_CAP_DOME) && numDome1) {

      /* At start, cap from point up to first regular contour for dome */
      nmesh = makeCapMesh(&domeCont1[numDome1 - 1], &domeCap1, 1, &ptProps, 
                          stateFlags, stateTest);
      addMeshToObject(obj, cont, nmesh, flags);
      for (pt = numDome1 - 1; pt >= 0; pt--) {
        nextcont = pt ? &domeCont1[pt - 1] : &clst[0];
        nmesh = joinTubeCont(&domeCont1[pt], nextcont, &domeRot1, &ptProps,
                             stateFlags, &ptProps, stateFlags);
        addMeshToObject(obj, cont, nmesh, flags);
      }
      imodContoursDelete(domeCont1, numDome1);

    } else if (flags & IMESH_MK_CAP_TUBE) {
      nmesh = makeCapMesh(&clst[0], &cont->pts[0], 1, &ptProps, stateFlags, 
                          stateTest);
      addMeshToObject(obj, cont, nmesh, flags);
    }

    for (pt = 0; pt < cont->psize - 1; pt++) {
      lastState = stateFlags;
      lastProps = ptProps;
      ptProps.gap = 0;
      if (pt + 1 == nextChange)
        nextChange = istoreNextChange(cont->store, &contProps, &ptProps, 
                                      &stateFlags, &changeFlags);
      if (lastProps.gap)
        continue;
      
      nrot.x = scale->x * (cont->pts[pt+1].x - cont->pts[pt].x);
      nrot.y = scale->y * (cont->pts[pt+1].y - cont->pts[pt].y);
      nrot.z = scale->z * (cont->pts[pt+1].z - cont->pts[pt].z);

      imodPointNormalize(&nrot);
      nmesh = joinTubeCont(&clst[pt], &clst[pt+1], &nrot, &lastProps, 
                           lastState, &ptProps, stateFlags);
      addMeshToObject(obj, cont, nmesh, flags);
    }

    if ((flags & IMESH_MK_CAP_DOME) && numDome2) {

      /* At end, cap from last regular contour out to point for dome */
      for (pt = 0; pt < numDome2; pt++) {
        nextcont = pt ? &domeCont2[pt - 1] : &clst[cont->psize - 1];
        nmesh = joinTubeCont(nextcont, &domeCont2[pt], &domeRot2, &ptProps,
                             stateFlags, &ptProps, stateFlags);
        addMeshToObject(obj, cont, nmesh, flags);
      }
      nmesh = makeCapMesh(&domeCont2[numDome2 - 1], &domeCap2, 0, &ptProps, 
                          stateFlags, stateTest);
      addMeshToObject(obj, cont, nmesh, flags);
      imodContoursDelete(domeCont2, numDome2);

    } else if (flags & IMESH_MK_CAP_TUBE) {
      nmesh = makeCapMesh(&clst[cont->psize - 1], &cont->pts[cont->psize - 1], 
                          0, &ptProps, stateFlags, stateTest);
      addMeshToObject(obj, cont, nmesh, flags);
    }
          
    imodContoursDelete(clst, cont->psize);
  }
     
    
  obj->mesh = imeshReMeshNormal(obj->mesh, &(obj->meshsize), scale, 0);
  if (!obj->mesh)
    obj->meshsize = 0;
  return(0);
}

/* 
 * Make contours for a hemispherical cap and return the terminal point also
 */
static Icont *makeDomeConts(Icont *lastCont, Ipoint *lastPt, Ipoint *nrot, 
                            Ipoint *scale, float diameter, float direction,
                            int *numCont, Ipoint *capPt)
{
  int i, slices;
  Ipoint sclPt;
  int numPos = B3DNINT(lastCont->psize / 4.);
  Icont *conts = imodContoursNew(numPos - 1);
  double dist, angle, dtor = 0.017453293;
  float radius;
  *numCont = 0;
  if (!conts)
    return NULL;
  sclPt.x = lastPt->x * scale->x;
  sclPt.y = lastPt->y * scale->y;
  sclPt.z = lastPt->z * scale->z;

  for (i = 1; i <= numPos; i++) {
    angle = (dtor * i * 90.) / numPos;
    dist = 0.5 * diameter * direction * sin(angle);
    capPt->x = (sclPt.x + nrot->x * dist) / scale->x;
    capPt->y = (sclPt.y + nrot->y * dist) / scale->y;
    capPt->z = (sclPt.z + nrot->z * dist) / scale->z;
    if (i < numPos) {
      radius = (float)(0.5 * diameter * cos(angle));
      slices = B3DMAX(12, B3DMIN(50, (int)radius));
      makeTubeCont(&conts[i - 1], capPt, nrot, scale, 2.f * radius, slices);
    }
  }
  *numCont = numPos - 1;
  return conts;
}

/* Common operations when adding a new mesh to an object's mesh */
static void addMeshToObject(Iobj *obj, Icont *cont, Imesh *nmesh, unsigned int flags)
{
  if (nmesh) {
    nmesh->surf = (flags & IMESH_MK_SURF) ? cont->surf : 0;
    nmesh->time = (flags & IMESH_MK_TIME) ? cont->time : 0;
    obj->mesh = imodel_mesh_add(nmesh, obj->mesh, &(obj->meshsize));
    free(nmesh);
  }
}

/*!
 * The main meshing routine for closed or open contours; calls appropriate
 * routines for open contours or tube contours.  Arguments are:
 * ^  [*obj]          The object to be skinned.           
 * ^  [*scale]        Scaling used for normal calculation.
 * ^  [overlap]       Overlap percentage.
 * ^  [cap]           Cap ends of surface.                 
 * ^  [*cap_skip_zlist]  List of Z values to not cap 
 * ^  [cap_skip_nz]      Number of Z values to not cap 
 * ^  [incz]          Increment in z values 
 * ^  [int flags]     Flags
 * ^  [skipPasses]    Number of passes for skipped sections 
 * ^  [tubeDiameter]  Diameter for tube meshing, or 0 to use 3D line width, -1
 * to use point sizes, or -2 to use symbol sizes 
 * ^  [inCB]          A callback function, or NULL for none
 */
int imeshSkinObject(Iobj *obj, Ipoint *scale, double overlap, int cap,       
                    int *cap_skip_zlist, int cap_skip_nz, int incz,            
                    unsigned int flags, int skipPasses, double tubeDiameter, 
                    int (*inCB)(int))
{
  int co, tco, bco, m;
  Imesh *nmesh = NULL;
  Icont *cont, *tcont, *bcont, *jcont;
  Icont **scancont;
  int zmin,zmax;
  int i, j, nummax;
  int found_data, force, tCnctNum, bCnctNum;
  int *zlist;
  int zlsize = 0;
  int nextz, prevz;
  int direction;
  int foundInside;

  int (*cb)(int);
  int status;

  int *blist, *tlist, *contz;
  Ipoint *pmin, *pmax;
  int nbot, ntop, lind, inl;
  int *numatz;
  int **contatz;
  int indz, lis, kis;
  int numnests = 0;
  Icont *econt;  /* The contour that will be tested    */
  int eco;       /* for being eaten all up.            */
  float frac1, frac2, dist, minDist;
  Nesting *nest, *nests;
  int *nestind;
  int nind, zpass;
  Ipoint bcm, tcm;
  static int numwarn = 0;

  /* start the timer if doing reports */
  timeval1 = clock();

  /* Check that the input object is skinable.
   */
  if (obj == NULL)
    return(-1);
  if (obj->flags & IMOD_OBJFLAG_SCAT)
    return(-1);     
  obj->mesh = NULL;
  obj->meshsize = 0;
  if (!obj->contsize)
    return 0;
     
  if (!iobjClose(obj->flags) && (flags & IMESH_MK_TUBE) )
    return(mesh_open_tube_obj(obj, scale, flags, tubeDiameter));

  if (flags & IMESH_MK_FAST)
    fastmesh = 0;
  else
    fastmesh = 1;

  skinFlags = flags;
  imeshSetSkinFlags(skinFlags, fastmesh);
  if (flags & IMESH_MK_NO_WARN)
    numwarn = -1;

  if (imodContourMakeZTables(obj, incz, CONNECT_BOTH | ICONT_CONNECT_INVERT, 
                             &contz, &zlist, &numatz, &contatz, &zmin, &zmax,
                             &zlsize, &nummax))
    return -1;


  /* Allocate space for lists of min's max's, and found contours */

  pmin = (Ipoint *)malloc(obj->contsize * sizeof(Ipoint));
  pmax = (Ipoint *)malloc(obj->contsize * sizeof(Ipoint));
  if (!pmin || !pmax)
    return(-1);

  for (co = 0; co < obj->contsize; co++)
    if (obj->cont[co].psize)
      imodContourGetBBox(&(obj->cont[co]), &(pmin[co]), &(pmax[co]));

  /* OPEN OBJECT: GO OFF AND DO IT */
  if (!iobjClose(obj->flags))
    return(mesh_open_obj(obj, scale, incz, flags, skipPasses, zmin, zmax,
                         contz, zlist, zlsize, numatz, contatz, pmin, pmax));

  blist = (int *)malloc(obj->contsize * sizeof(int));
  tlist = (int *)malloc(obj->contsize * sizeof(int));
  if (!blist || !tlist)
    return(-1);

  /* Tell callback function we are just getting started. */
  if (inCB) cb = inCB; else cb = imeshDefaultCallback;
  status = (*cb)(1);
  if (status) return(status);

  /*
   * Prescan contours for faster rendering.
   */
  scancont = (Icont **)malloc(obj->contsize * sizeof (Icont *)); 
  if (!scancont) return(-1);
  for (co = 0; co < obj->contsize; co++)
    scancont[co] = imodel_contour_scan(&(obj->cont[co]));

  skin_report_time("Got scan contours");
     
  /* Get array for pointers to inside/outside information */
  nestind = (int *)malloc(obj->contsize * sizeof(int));
  if (!nestind)
    return(-1);
  for (co = 0; co < obj->contsize; co++)
    nestind[co] = -1;

  /*
   *  Mark all contours with no data as connected.
   *  Build lists of inside and outside contours.
   */
     
  for (indz = 0; indz < zmax + 1 - zmin; indz++) {
    for (kis = 0; kis < numatz[indz] - 1; kis++) {
      co = contatz[indz][kis];
      cont = &(obj->cont[co]);
      if (!cont->psize)
        cont->flags |= CONNECT_BOTH;
      if (cont->flags & CONNECT_BOTH)
        continue;
      for (lis = kis + 1; lis < numatz[indz]; lis++) {
        eco = contatz[indz][lis];
        econt = &(obj->cont[eco]);
        if (!econt->psize)
          econt->flags |= CONNECT_BOTH;
        if (econt->flags & CONNECT_BOTH)
          continue;
        if ((flags & IMESH_MK_TIME) && (cont->time != econt->time))
          continue;

        if (imodContourCheckNesting(co, eco, scancont, pmin, pmax, &nests,
                                    nestind, &numnests, &numwarn))
          return -1;
      }
    }
  }


  /* Analyze inside and outside contours to determine level */
  imodContourNestLevels(nests, nestind, numnests);

  /* Don't strip the inside and outside lists down to adjacent levels only */
  /* Deleted code 1/28/05 */

  /* Compose a scan contour excluding inner areas for each outer contour
     at an odd level */
  /* DNM 12/18/01: Need to include even levels also now */
  for (nind = 0; nind < numnests; nind++) {
    nest = &nests[nind];        
    if (/*(nest->level % 2) == 1 && */ nest->ninside > 0) {
      nest->inscan = imodContourDup(scancont[nest->co]);
      for (i = 0; i < nest->ninside; i++)
        subtract_scan_contours(nest->inscan, scancont[nest->inside[i]]);
    }
  }

     
  skin_report_time("Marked contours");

  status = (*cb)(25); /* quarter of the way done. */     
  /*status = (*cb)(1); ? */
  if (status) return(status);
     
  /* Do multiple passes for connections farther apart in Z */
  for (zpass = 1; zpass <= skipPasses; zpass++) {

    /* go through all contours and find connections */
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      if (!cont->psize)
        continue;
          
      /* use type to tell if already connected. 
       * type = 0 (contour is not connected to any other contour. 
       * type = 1 connected on top.            
       * type = 2 connected on bottom.         
       * type = 3 connected on top and bottom. 
       *  connect from top of current, bottom contour to other
       *  contours on top.
       */
      if (connectTop(cont->flags))
        continue;

      /*  Set the connection on top and define the top contour
       *  as empty.  We are going to find a top contour (tcont) to connect
       *  to this contour (cont) if we can.
       */
      cont->flags |=  CONNECT_TOP;

      /* The current contour is now the bottom contour (bcont)    */
      bcont = cont;

      /* define z-sections that connections will be made to; step farther
         in Z for each pass */
      nextz = contz[co];
      for (inl = 0; inl < zpass; inl++) {
        if (flags & IMESH_MK_SKIP) {
          nextz = getnextz(zlist, zlsize, nextz);
        }else{
          nextz += incz;
        }
        if (nextz > zlist[zlsize - 1])
          break;
      }
      prevz = contz[co];
      found_data = 1;

      nbot = 1;
      ntop = 0;
      blist[0] = co;
      foundInside = 0;
      if (nestind[co] >= 0) {
                                        
        /* If inside/outside, find outermost */
        foundInside = 1;
        add_whole_nest(nestind[co], obj, nests, nestind, CONNECT_TOP,
                       blist, &nbot);
      }
          
      /* find all contours that link to current contour.
       * If we find a contour we have to look again, because
       * we may have to join contours to the one we just found.
       */

      while(found_data) {
        found_data = 0;
               
        for (lis = 0; lis < numatz[nextz - zmin]; lis++) {
          tco = contatz[nextz - zmin][lis];
                    
          /* The current contour we are going to try to join. */
          jcont = &(obj->cont[tco]);
                    
          /* can this be a top contour? 
           * if so add it to top list.
           */
          /* Skip contours that are already connected */
          if (!jcont->psize)
            continue;
          if (connectBottom(jcont->flags))
            continue;
          if ((flags & IMESH_MK_SURF) && bcont->surf != jcont->surf)
            continue;
          if ((flags & IMESH_MK_TIME) && bcont->time != jcont->time)
            continue;
                         
          /* See if it overlaps any contour in bottom list */
          tCnctNum = istoreConnectNumber(obj->store, tco);
          for (inl = 0; inl < nbot; inl++) {
            lind = blist[inl];

            /* Skip if they both have connection numbers and don't match, or
               force connection if they match */
            bCnctNum = istoreConnectNumber(obj->store, lind);
            if (tCnctNum >= 0 && bCnctNum >= 0 && tCnctNum != bCnctNum)
              continue;
            force = (tCnctNum >= 0 && bCnctNum >= 0 && tCnctNum == bCnctNum) 
              ? 1 : 0;

            if (imodel_scans_overlap(scancont[lind], pmin[lind], pmax[lind],
                                     scancont[tco], pmin[tco], pmax[tco]) ||
                force) {
              if (!force && overlap != 0.0) {
                /* contours must overlap by given fraction*/
                imodel_overlap_fractions(&scancont[lind], pmin[lind], 
                                         pmax[lind], &scancont[tco],
                                         pmin[tco], pmax[tco], &frac1, &frac2);
                if (frac1 <= overlap && frac2 <= overlap)
                  continue;
              }
              found_data = 1;
              jcont->flags |= CONNECT_BOTTOM;
              tlist[ntop++] = tco;
              if (nestind[tco] >= 0) {

                /* If inside/outside, find outermost */
                foundInside = 1;
                add_whole_nest(nestind[tco], obj, nests, nestind,
                               CONNECT_BOTTOM, tlist, &ntop);
              }
              break;
            }
          }
        }

        /* if any new top contours were found, need to scan for more
           bottom contours */
        if (!found_data)
          break;
        for (lis = 0; lis < numatz[prevz - zmin]; lis++) {
          tco = contatz[prevz - zmin][lis];
                    
          /* The current contour we are going to try to join. */
          jcont = &(obj->cont[tco]);
                    
          /* can this be an addition to the bottom?
           * if so add to list on bottom
           */
          /* Skip contours that are already connected */
          if (!jcont->psize)
            continue;
          if (connectTop(jcont->flags))
            continue;
                                   
          if ((flags & IMESH_MK_SURF) && bcont->surf != jcont->surf)
            continue;
          if ((flags & IMESH_MK_TIME) && bcont->time != jcont->time)
            continue;

          /* See if it overlaps any contour in top list */
          bCnctNum = istoreConnectNumber(obj->store, tco);
          for (inl = 0; inl < ntop; inl++) {
            lind = tlist[inl];
            tCnctNum = istoreConnectNumber(obj->store, lind);
            if (tCnctNum >= 0 && bCnctNum >= 0 && tCnctNum != bCnctNum)
              continue;
            force = (tCnctNum >= 0 && bCnctNum >= 0 && tCnctNum == bCnctNum) 
              ? 1 : 0;

            if (imodel_scans_overlap(scancont[lind], pmin[lind], pmax[lind],
                                     scancont[tco], pmin[tco], pmax[tco]) ||
                force) {

              if (!force && overlap != 0.0) {
                /* contours must overlap by given fraction*/
                imodel_overlap_fractions(&scancont[lind], pmin[lind],
                                         pmax[lind], &scancont[tco],
                                         pmin[tco], pmax[tco], &frac1, &frac2);
                if (frac1 <= overlap && frac2 <= overlap)
                  continue;
              }
              found_data = 1;
              jcont->flags |= CONNECT_TOP;
              blist[nbot++] = tco;
              if (nestind[tco] >= 0) {
                                   
                /* If inside/outside, find outermost */
                foundInside = 1;
                add_whole_nest(nestind[tco], obj, nests, nestind, CONNECT_TOP,
                               blist, &nbot);
              }
              break;
            }
          }
        }
      }
          
      if (ntop && !foundInside) {
        bcont = join_all_contours(obj, blist, nbot, nbot, 1, tlist,
                                  ntop, ntop, NULL);
        tcont = join_all_contours(obj, tlist, ntop, ntop, -1, blist,
                                  nbot, nbot, NULL);
        if (mesh_contours(obj, bcont, tcont, cont->surf, cont->time, scale, 0,
                          blist[0], tlist[0]))
          return (-1);
      } else if (ntop) {
        /* There are inside contours to resolve */
        /* declarations for this big section */
        float *maxfrac, *minfrac;
        int *tinlist, *binlist, *tjustinl, *bjustinl;
        int *toutlist, *boutlist, *tused, *bused, *tlevels, *blevels;
        Icont *boutcont, *toutcont;
        Nesting *bnest;
        float fracmax;
        int imax, jmax, tlevel, blevel, nbjustin, ntjustin, found;
        int ntopout, nbotout, ntopin, nbotin, istr, icur, k;
        int ntopsame, nbotsame, oddeven;
        Ipoint bpmin, bpmax, tpmin, tpmax; 

        minfrac = (float *)malloc(ntop * nbot * sizeof(float));
        maxfrac = (float *)malloc(ntop * nbot * sizeof(float));
        tinlist = (int *)malloc(ntop * sizeof(int));
        binlist = (int *)malloc(nbot * sizeof(int));
        tjustinl = (int *)malloc(ntop * sizeof(int));
        bjustinl = (int *)malloc(nbot * sizeof(int));
        toutlist = (int *)malloc(ntop * sizeof(int));
        boutlist = (int *)malloc(nbot * sizeof(int));
        tused = (int *)malloc(ntop * sizeof(int));
        bused = (int *)malloc(nbot * sizeof(int));
        tlevels = (int *)malloc(ntop * sizeof(int));
        blevels = (int *)malloc(nbot * sizeof(int));

        if (!minfrac || !tinlist || !binlist || !tjustinl || !bjustinl
            || !toutlist || !boutlist || !tused || !bused || !tlevels
            || !blevels || !maxfrac) {
          b3dError(stderr, "Fatal Error: "
                  "Not enough memory to get in/out arrays.\n");
          return(-1);
        }
                   
        /* dump_lists("initial", blist, nbot, tlist, ntop, NULL, NULL);*/
        /* Make tables of overlap fractions between top and bottom */
        for (i = 0; i < ntop; i++) {
          tused[i] = 0;
          tlevels[i] = 1;
          tcont = scancont[tlist[i]];
          tpmin = pmin[tlist[i]];
          tpmax = pmax[tlist[i]];
          if (nestind[tlist[i]] >= 0) {
            nest = &nests[nestind[tlist[i]]];
            tlevels[i] = nest->level;
            /* DNM 12/18/01: Need to include even levels also */
            if (/*nest->level % 2 &&*/ nest->ninside) {
              tcont = nest->inscan;
              imodContourGetBBox(tcont, &tpmin, &tpmax);
            }
          }
          for (j = 0; j < nbot; j++) {
            bused[j] = 0;
            blevels[j] = 1;
            bcont = scancont[blist[j]];
            bpmin = pmin[blist[j]];
            bpmax = pmax[blist[j]];
            if (nestind[blist[j]] >= 0) {
              bnest = &nests[nestind[blist[j]]];
              blevels[j] = bnest->level;
              if (/*bnest->level % 2 &&*/ bnest->ninside) {
                bcont = bnest->inscan;
                imodContourGetBBox(bcont, &bpmin, &bpmax);
              }
            }
            imodel_overlap_fractions(&bcont, bpmin, bpmax, &tcont, 
                                     tpmin, tpmax, &frac1, &frac2);
            if (frac1 > frac2) {
              maxfrac[i * nbot + j] = frac1;
              minfrac[i * nbot + j] = frac2;
            } else {
              maxfrac[i * nbot + j] = frac2;
              minfrac[i * nbot + j] = frac1;
            }
          }
        }

        /* Loop, starting with the maximum overlap pair of odd levels */
        oddeven = 1;
        for (;;) {
          fracmax = -1.;
          for (i = 0; i < ntop; i++) {
            if (!tused[i] && (tlevels[i] % 2 == oddeven))
              for (j = 0; j < nbot; j++) {
                if (!bused[j] && 
                    (blevels[j] % 2 == oddeven)) {
                  frac1 = minfrac[i * nbot + j];
                  if (frac1 > fracmax) {
                    fracmax = frac1;
                    imax = i;
                    jmax = j;
                  }
                }
              }
          }

          /* If there is no overlap, switch to doing even levels and
             restart, or break out if evens are done also */
          if (fracmax <= overlap) {
            if (oddeven) {
              oddeven = 0;
              continue;
            } else
              break;
          }

          /* Start new lists with best pair, take them off original
             lists */
          tlevel = tlevels[imax];
          blevel = blevels[jmax];
          ntopout = 1;
          nbotout = 1;
          toutlist[0] = imax;
          boutlist[0] = jmax;
          tused[imax] = 1;
          bused[jmax] = 1;
                         
          do {
            found_data = 0;
            /* check every available top contour of same level
               against the bottom list, add to top list if
               overlap */
            for (i = 0; i < ntop; i++) {
              if (!tused[i] && tlevels[i] == tlevel) {
                for (j = 0; j < nbotout; j++) {
                  if (maxfrac[i * nbot + boutlist[j]] > overlap) {
                    toutlist[ntopout++] = i;
                    tused[i] = 1;
                    found_data = 1;
                    break;
                  }
                }
              }
            }
            /* Do the same for bottom contours against top list */
            for (j = 0; j < nbot; j++) {
              if (!bused[j] && blevels[j] == blevel) {
                for (i = 0; i < ntopout; i++) {
                  if (maxfrac[toutlist[i] * nbot + j] > overlap) {
                    boutlist[nbotout++] = j;
                    bused[j] = 1;
                    found_data = 1;
                    break;
                  }
                }
              }
            }
          } while(found_data);

          /*dump_lists("outside", boutlist, nbotout, toutlist, ntopout,blist, tlist);*/
          /* Next, build lists of contours just inside 
             this set of contours */
          nbjustin = 0;
          ntjustin = 0;
          for (i = 0; i < ntop; i++) {
            if (!tused[i] && tlevels[i] == tlevel + 1) {
              nest = &nests[nestind[tlist[i]]];
              for (k = 0; k < nest->noutside; k++) {
                found = 0;
                for (j = 0; j < ntopout; j++) {
                  if (nest->outside[k] == tlist[toutlist[j]]) {
                    found = 1;
                    tjustinl[ntjustin++] = i;
                    break;
                  }
                }
                if (found)
                  break;
              }
            }
          }
          for (i = 0; i < nbot; i++) {
            if (!bused[i] && blevels[i] == blevel + 1) {
              nest = &nests[nestind[blist[i]]];
              for (k = 0; k < nest->noutside; k++) {
                found = 0;
                for (j = 0; j < nbotout; j++) {
                  if (nest->outside[k] == blist[boutlist[j]]) {
                    found = 1;
                    bjustinl[nbjustin++] = i;
                    break;
                  }
                }
                if (found)
                  break;
              }
            }
          }
          /*dump_lists("just inside", bjustinl, nbjustin, tjustinl, 
            ntjustin, blist, tlist);*/
                    
          /* Now iteratively find overlapping sets of inside conts */
          for (istr = 0; istr < nbjustin; istr++) {
            if (!bused[bjustinl[istr]]) {
              ntopin = 0;
              nbotin = 1;
              binlist[0] = bjustinl[istr];
              bused[bjustinl[istr]] = 1;
              do {
                found_data = 0;
                /* check on top list of just in contours
                   for ones that overlap list of overlapping
                   bottom conts */
                for (j = 0; j < ntjustin; j++) {
                  icur = tjustinl[j];
                  if (!tused[icur])
                    for (k = 0; k < nbotin; k++) {
                      if (maxfrac[icur * nbot + binlist[k]] > overlap) {
                        found_data = 1;
                        tinlist[ntopin++] =icur;
                        tused[icur] = 1;
                        break;
                      }
                    }
                }
                /* check on bottom list of just in contours
                   for ones that overlap list of overlapping
                   top conts */
                for (j = 0; j < nbjustin; j++) {
                  icur = bjustinl[j];
                  if (!bused[icur])
                    for (k = 0; k < ntopin; k++) {
                      if (maxfrac[tinlist[k] * nbot + icur] > overlap) {
                        found_data = 1;
                        binlist[nbotin++]= icur;
                        bused[icur] = 1;
                        break;
                      }
                    }
                }
              } while(found_data);

              /*dump_lists("inside", binlist, nbotin, tinlist, 
                ntopin, blist, tlist);*/
              /* If got top contours, process and mesh the
                 set of top and bottoms */
              if (ntopin) {
                nbotsame = nbotin;
                ntopsame = ntopin;
                /* but first, look for contours just inside
                   these and add them to lists if they
                   overlap the interior area of the other
                   contours or ones already added to list */

                do {
                  found_data = 0;
                  for (i = 0; i < ntop; i++) {
                    if (tused[i] || tlevels[i] != tlevel + 2)
                      continue;

                    /* see if it's inside */
                    nest = &nests[nestind[tlist[i]]];
                    found = 0;
                    for (k = 0; k < nest->noutside; k++)
                      for (j = 0; j < ntopin; j++) 
                        if (nest->outside[k] == tlist[tinlist[j]]) 
                          found = 1;
                    if (!found)
                      continue;

                    /* see if overlap outsides */
                    found = 0;
                    for (k = 0; k < nbotout; k++)
                      if (maxfrac[i * nbot + boutlist[k]] > overlap) 
                        found = 1;

                    /* see if overlap added ones */
                    for (k = nbotsame; k < nbotin; k++)
                      if (maxfrac[i * nbot + binlist[k]] > overlap)
                        found = 1;
                    if (found) {
                      found_data = 1;
                      tinlist[ntopin++] =i;
                      tused[i] = 1;
                    }

                  }

                  for (i = 0; i < nbot; i++) {
                    if (bused[i] || blevels[i] != blevel + 2)
                      continue;

                    /* see if it's inside */
                    nest = &nests[nestind[blist[i]]];
                    found = 0;
                    for (k = 0; k < nest->noutside; k++)
                      for (j = 0; j < nbotin; j++) 
                        if (nest->outside[k] == blist[binlist[j]]) 
                          found = 1;
                    if (!found)
                      continue;

                    /* see if overlap outsides */
                    found = 0;
                    for (k = 0; k < ntopout; k++)
                      if (maxfrac[toutlist[k] * nbot + i] > overlap) 
                        found = 1;

                    /* see if overlap added ones */
                    for (k = ntopsame; k < ntopin; k++)
                      if (maxfrac[tinlist[k] * nbot +  i] > overlap)
                        found = 1;
                    if (found) {
                      found_data = 1;
                      binlist[nbotin++] =i;
                      bused[i] = 1;
                    }
                  }

                } while(found_data);

                /*dump_lists("inside enhanced", binlist, 
                  nbotin, tinlist, ntopin, blist, tlist); */
                for (i = 0; i < ntopin; i++)
                  tinlist[i] = tlist[tinlist[i]];
                for (i = 0; i < nbotin; i++)
                  binlist[i] = blist[binlist[i]];

                bcont = join_all_contours(obj, binlist, nbotin, nbotsame,
                                          1, tinlist, ntopin, ntopsame, NULL);
                tcont = join_all_contours(obj, tinlist, ntopin, ntopsame,
                                          -1, binlist, nbotin, nbotsame, NULL);
                if (mesh_contours(obj, bcont, tcont, cont->surf, cont->time,
                                  scale, oddeven, binlist[0], tinlist[0]))
                  return (-1);
              } else
                bused[bjustinl[istr]] = 0;
            }
          }

          /* There may now be orphan inside contours left.  At this
             point, convert the outside lists and join into single
             outside contours */
          for (i = 0; i < ntopout; i++)
            toutlist[i] = tlist[toutlist[i]];
          for (i = 0; i < nbotout; i++)
            boutlist[i] = blist[boutlist[i]];

          boutcont = join_all_contours(obj, boutlist, nbotout, nbotout, 1,
                                       toutlist, ntopout, ntopout, NULL);
          toutcont = join_all_contours(obj, toutlist, ntopout, ntopout, -1,
                                       boutlist, nbotout, nbotout, NULL);
          if (!boutcont || !toutcont)
            return (-1);

          boutcont = connect_orphans(obj, boutcont, tlist, tused, tjustinl,
                                     ntjustin, tinlist, -1, oddeven,
                                     cont->surf, cont->time, 
                                     scale, scancont, pmin, pmax);
          toutcont = connect_orphans(obj, toutcont, blist, bused,
                                     bjustinl, nbjustin, binlist, 
                                     1, oddeven, cont->surf, cont->time,
                                     scale, scancont, pmin, pmax);
          /* finally ready to mesh the outside contours, possibly
             changed by orphan handling */
          if (mesh_contours(obj, boutcont, toutcont, cont->surf, cont->time,
                            scale, 1 - oddeven, boutlist[0], toutlist[0]))
            return (-1);

          /* All of this continues until no more overlapping outside
             contours are found at any level */
        }

        /* Need to unmark connections for any that weren't used */
        for (i = 0; i < nbot; i++)
          if (bused[i] <= 0)
            obj->cont[blist[i]].flags &= ~CONNECT_TOP;
        for (i = 0; i < ntop; i++)
          if (tused[i] <= 0)
            obj->cont[tlist[i]].flags &= ~CONNECT_BOTTOM;


        /* clean up inside-outside arrays */
        free(minfrac);
        free(maxfrac);
        free(tinlist);
        free(binlist);
        free(tjustinl);
        free(bjustinl);
        free(toutlist);
        free(boutlist);
        free(tused);
        free(bused);
        free(blevels);
        free(tlevels);

      } else {
        /* We didn't find a contour to connect to - remove marks. */
        for (i = 0; i < nbot; i++)
          obj->cont[blist[i]].flags &= ~CONNECT_TOP;
      }
    }
  }   /* end of zpass loop */
     
  /* force connection of stray contours to nearest contour */
  if (flags & IMESH_MK_STRAY) {
    for (indz = 0; indz < zmax + 1 - zmin; indz++) {

      /* Loop on Z and get next Z value for connection */
      if (flags & IMESH_MK_SKIP)
        nextz = getnextz(zlist, zlsize, indz + zmin);
      else
        nextz = indz + zmin + incz;
      if (nextz > zlist[zlsize - 1])
        continue;

      /* Loop on pairs of contours between the two sections */
      do {
        tcont = NULL;
        for (kis = 0; kis < numatz[indz]; kis++) {
          co = contatz[indz][kis];
          cont = &(obj->cont[co]);
          if (connectTop(cont->flags))
            continue;

          bCnctNum = istoreConnectNumber(obj->store, co);
          robustCenterOfMass(cont, &bcm);
          minDist = 1.e30;

          for (lis = 0; lis < numatz[nextz - zmin]; lis++) {
            eco = contatz[nextz - zmin][lis];
            jcont = &(obj->cont[eco]);
            if (jcont->flags & CONNECT_BOTTOM)
              continue;
            if ((flags & IMESH_MK_SURF) && cont->surf != jcont->surf)
              continue;
            if ((flags & IMESH_MK_TIME) && cont->time != jcont->time)
              continue;
            if (cont->psize == 1 && jcont->psize == 1)
              continue;

            /* Do not connect if they have non-matching connection numbers */
            tCnctNum = istoreConnectNumber(obj->store, eco);
            if (tCnctNum >= 0 && bCnctNum >= 0 && tCnctNum != bCnctNum)
              continue;

            robustCenterOfMass(jcont, &tcm);
            dist = (tcm.x - bcm.x) * (tcm.x - bcm.x) + 
              (tcm.y - bcm.y) * (tcm.y - bcm.y);

            /* Keep track of closest pair of contours */
            if (dist < minDist) {
              tcont = jcont;
              bcont = cont;
              minDist = dist;
              bco = co;
              tco = eco;
            }
          }
        }

        if (tcont) {
          int intmp = 0;
          if (obj->flags & IMOD_OBJFLAG_OUT)
            intmp = 1 - intmp;
          nmesh = imeshContoursCost(obj, bcont, tcont, scale, intmp, bco, tco);
          if (nmesh) {
            obj->mesh = imodel_mesh_add
              (nmesh, obj->mesh, &(obj->meshsize));
            free(nmesh);
          }
          bcont->flags |= CONNECT_TOP;
          tcont->flags |= CONNECT_BOTTOM;
        }
               
      } while (tcont);
    }
  }     

  skin_report_time("Connected contours");
  /*
   * Clean up scan conversions.
   */
  for (co = 0; co < obj->contsize; co++) {
    if (scancont[co])
      imodContourDelete(scancont[co]);
  }
  free(scancont);

  status = (*cb)(50); /* half way done. */     
  status = (*cb)(1);
  if (status) return(status);     


  /*     printf(".");fflush(stdout); */
  if (cap) {
    int skipz;
    int inside;
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      inside = 0;

      /* if the contour has two or less points then the
       * surface must already be capped. Only cap if contour
       * isn't connected on top or bottom.
       */
      if ((cont->psize > 2) && (!connectBoth(cont->flags))) {

        /* But don't cap if the thickness is nil, this was already
           meshed as a complex cap */
        if (imodContourArea(cont) / imodContourLength(cont, 1)
            < 0.01)
          continue;

        /* Don't cap an open contour with phantom extensions */
        if ((cont->flags & ICONT_OPEN) && istorePointIsGap(cont->store, 0) &&
            istorePointIsGap(cont->store, cont->psize - 2))
          continue;
                
        if (nestind[co] >= 0)
          if (nests[nestind[co]].level % 2 == 0)
            inside = 1;

        if (obj->flags & IMOD_OBJFLAG_OUT)
          inside = 1 - inside;

        /* Skip capping if the Z value is in list to exclude */
        if (connectBottom(cont->flags) || cont->pts->z == zmax)
          direction = 1;
        else
          direction = -1;
        skipz = 0;
        for (i = 0; i < cap_skip_nz; i++)
          if (floor((double)(cont->pts->z + direction + 0.5)) ==
              cap_skip_zlist[i]) {
            skipz = 1;
            break;
          }
        if (skipz)
          continue;

        if (cap == IMESH_CAP_ALL) {

          if (connectBottom(cont->flags))
            direction = 1;
          if (connectTop(cont->flags))
            direction = -1;
          nmesh = imeshContourCap(obj, cont, co, direction, inside, scale);
          addMeshToObject(obj, cont, nmesh, flags);
          continue;
        }

        if (cont->pts->z == zmin) {
          nmesh = imeshContourCap(obj, cont, co, -1, inside, scale); 
          addMeshToObject(obj, cont, nmesh, flags);
        }
        if (cont->pts->z == zmax) {
          nmesh = imeshContourCap(obj, cont, co, 1, inside, scale); 
          addMeshToObject(obj, cont, nmesh, flags);
        }
      }
    }

  }
          
               
  skin_report_time("Capped");
  for (m = 0; m < obj->meshsize; m++) {
    if (!(flags & IMESH_MK_SURF))
      obj->mesh[m].surf = 0;
    if (!(flags & IMESH_MK_TIME))
      obj->mesh[m].time = 0;
  }
     
  if (flags & IMESH_MK_NORM) {
    obj->mesh = imeshReMeshNormal(obj->mesh, &(obj->meshsize), scale,
                                  0); 
    if (!obj->mesh)
      obj->meshsize = 0;
  }       

  imodContourFreeNests(nests, numnests);
  imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);

  free(nestind);

  free(tlist);
  free(blist);
  free(pmin);
  free(pmax);
  return(0);
}

static void add_whole_nest(int nind, Iobj *obj, Nesting *nests, int *nestind,
                           int flag, int *tlist, int *ntop)
{
  int i, oind;
  Icont *jcont;
  Nesting *nest = &nests[nind];
  for (i = 0; i < nest->noutside; i++) {
    oind = nestind[nest->outside[i]];
    if (nests[oind].level == 1) {
      nind = oind;
      nest = &nests[nind];
      break;
    }
  }

  /* Add it to list if not on it */
  jcont = &obj->cont[nest->co];
  if (!(jcont->flags & flag)) {
    jcont->flags |= flag;
    tlist[(*ntop)++] = nest->co;
  }

  /* Add all inside contours to list */
  for (i = 0; i < nest->ninside; i++) {
    jcont = &obj->cont[nest->inside[i]];
    if (!(jcont->flags & flag)) {
      jcont->flags |= flag;
      tlist[(*ntop)++] = nest->inside[i];
    }
  }
}

/* Routine to connect orphan contours */
/* DNM 12/18/01: Add the oddeven argument, although it has not been verified 
   that it works for even ones */

#define NSECT 38
#define MAXTEST 100
#define NSEARCH  13
static Icont *connect_orphans(Iobj *obj, Icont *cout, int *list, int *used,
                              int *justinl, int njustin, int *inlist, 
                              int joindir, int oddeven, int surf, int time,
                              Ipoint *scale, Icont **scancont, Ipoint *pmin,
                              Ipoint *pmax)
{
  Ipoint pminout, pmaxout, pminin, pmaxin;
  Icont *outscan, *inscan, *c1, *c2, *cinmin, *coutmin, *cont;
  Ipoint *pts;
  int co, inl, st1, st2, ninlist, i, j, pt, next, st1min, st2min;
  float frac1, frac2, fullarea, area1, area2;
  float ratio, minratio, totlen, minpath, curpath;
  int found_data, ncand, sweepdir, norder;
  int outcand[NSECT], indorder[NSECT * NSECT];
  float pathlen[NSECT * NSECT];
  float forlen[NSECT], nextlen;
  double ddx, ddy, ddist;
  int st1test[MAXTEST], st2test[MAXTEST];
  float testratio [MAXTEST];
  int ntest, nexttest, outsize, dstdir, found, indmin;
  int dst1[NSEARCH] = {0, 1, 1, 1, 2, 2, 0, 2, -1, 0, -1, -1, 1}; 
  int dst2[NSEARCH] = {1, 0, 1, 2, 1, 2, 2, 0, 0, -1, -1, 1, -1};
  int icand1, icand2, iscan, instart, inend, nforward, scandir;
  int inmatch[NSECT];
  float seglen, innerarea, maxarea;
  int nscan = 2;

  found_data = 0;
  for (inl = 0; inl < njustin; inl++)
    if (!used[justinl[inl]])
      found_data = 1;
  if (!found_data)
    return cout;

  outscan = imodel_contour_scan(cout);
  if (!outscan)
    return NULL;
  imodContourGetBBox(cout, &pminout, &pmaxout);

  /* go through and mark ones that will get capped first */
  for (inl = 0; inl < njustin; inl++) {
    /* if it is an orphan, get the overlap fractions with the outer
       contour.  Large fraction means it will get capped; small fraction
       means we need to create a contour around a hole if possible */
    if (!used[justinl[inl]]) {
      co = list[justinl[inl]];
      imodel_overlap_fractions(&scancont[co], pmin[co], pmax[co],
                               &outscan, pminout, pmaxout,
                               &frac1, &frac2);
      if (frac1 > 0.8)    /* What to set this to as capping thresh? */
        used[justinl[inl]] = -1;
    }
  }
  imodContourDelete(outscan);
       
  for (inl = 0; inl < njustin; inl++) {
    if (used[justinl[inl]])
      continue;
    fullarea = imodContourArea(cout);
    totlen = imodContourLength(cout, 1);

    /* start with whole inner contour, then iterate on a segment */
    co = list[justinl[inl]];
    cont = &obj->cont[co];
    instart = 0;
    inend = 0;
    for (iscan = 0; iscan < nscan; iscan++) {
      pt = instart; 
      ddist = 0;
      pts = cont->pts;
      do {
        next = (pt + 1) % cont->psize;
        ddx = pts[next].x - pts[pt].x;
        ddy = pts[next].y - pts[pt].y;
        ddist += sqrt(ddx * ddx + ddy * ddy);
        pt = next;
      } while (next != inend);
      seglen = ddist;

      /* go to equally spaced points around the segment of the inner
         contour and make a list of nearest points in the outer 
         contour */
      ddist = 0.;
      nextlen = seglen / (NSECT - 2);
      ncand = 1;
      nforward = 0;
      pt = instart;
      inmatch[0] = instart;
      outcand[0] = imodContourNearest(cout, &pts[pt]);
      do {
        next = (pt + 1) % cont->psize;
        ddx = pts[next].x - pts[pt].x;
        ddy = pts[next].y - pts[pt].y;
        ddist += sqrt(ddx * ddx + ddy * ddy);
        if (ddist > nextlen || next == inend) {
          nextlen += seglen / (NSECT - 2);
          inmatch[ncand] = next;
          outcand[ncand++] = imodContourNearest(cout, &pts[next]);
          if (outcand[ncand - 1] > outcand[ncand - 2])
            nforward++;
          if (ncand == NSECT)
            break;
        }
        pt = next;
      } while (next != inend);
      scandir = (nforward > ncand / 2.) ? 1 : -1;
          
      /* order the candidates and eliminate duplicates */
      for (i = 0; i < ncand - 1; i++)
        for (j = i + 1; j < ncand; j++)
          if (outcand[i] > outcand[j]) {
            pt = outcand[i];
            outcand[i] = outcand[j];
            outcand[j] = pt;
            pt = inmatch[i];
            inmatch[i] = inmatch[j];
            inmatch[j] = pt;
          }
      j = 1;
      for (i = 1; i < ncand; i++)
        if (outcand[j - 1] != outcand[i]) {
          inmatch[j] = inmatch[i];
          outcand[j++] = outcand[i];
        }
      ncand = j;
      if (ncand < 2)
        break;

      /* get forward distances */
      pts = cout->pts;
      for (i = 0; i < ncand - 1; i++) {
        ddist = 0.;
        for (pt = outcand[i]; pt < outcand[i + 1]; pt++) 
          ddist += imodPointDistance(&pts[pt], &pts[pt + 1]);
        forlen[i] = ddist;
      }
          

      /* compute minimum path length between each pair of
         candidates */
      norder = 0;
      for (i = 0; i < ncand - 1; i++) {
        ddist = 0.;
        for (j = i + 1; j < ncand; j++) {
          ddist += forlen[j - 1];
          if (ddist < totlen / 2.) {
            pathlen[i * ncand + j] = ddist;
            pathlen[j * ncand + i] = 1.;
          } else {
            pathlen[i * ncand + j] = totlen - ddist;
            pathlen[j * ncand + i] = -1.;
          }
          indorder[norder++] = i * ncand + j;
        }
      }

      /* order by decreasing separation */
      for (i = 0; i < norder - 1; i++)
        for (j = i; j < norder; j++)
          if (pathlen[indorder[i]] < pathlen[indorder[j]]) {
            pt = indorder[i];
            indorder[i] = indorder[j];
            indorder[j] = pt;
          }

      /* run through the pairs of candidates finding one with a 
         maximum inner area; if inner area is equal to current 
         maximum, then minimize ratio as well */
      maxarea = -1.;
      for (i = 0; i < norder; i++) {
        st1 = outcand[indorder[i] / ncand];
        st2 = outcand[indorder[i] % ncand];
        ratio = evaluate_break(cout, list, used, justinl, njustin, scancont,
                               pmin, pmax, st1, st2, maxarea, 
                               pathlen[indorder[i]], fullarea,
                               joindir, 1, &innerarea);
        if (ratio > 1.e25)
          return NULL;
        if (innerarea > maxarea) {
          minratio = ratio;
          maxarea = innerarea;
          indmin = indorder[i];
        } else if (innerarea == maxarea && ratio < minratio) {
          minratio = ratio;
          indmin = indorder[i];
        }
      }

      /* set up for another scan between these points */
      icand1 = indmin / ncand;
      icand2 = indmin % ncand;
      if (scandir * pathlen[icand2 * ncand + icand1] > 0) {
        instart = inmatch[icand2];
        inend = inmatch[icand1];
      } else {
        instart = inmatch[icand1];
        inend = inmatch[icand2];
      }
    }
    if (ncand < 2)
      continue;

    /* Set up for search for somewhat local minimum */
    minpath = pathlen[indmin];
    st1min = outcand[icand1];
    st2min = outcand[icand2];
    st1test[0] = st1min;
    st2test[0] = st2min;
    testratio[0] = minratio;
    ntest = 1;
    nexttest = 1;
    outsize = cout->psize;
    sweepdir = -pathlen[icand2 * ncand + icand1];
    do {
      found_data = 0;
      for (i = 0; i < NSEARCH; i++) {
        curpath = minpath;
        st1 = (st1min + dst1[i] * sweepdir + outsize) % outsize;
        dstdir = dst1[i] > 0 ? 1 : -1;
        pt = st1;
        for (j = 0; j < dstdir * dst1[i]; j++) {
          next = (pt + dstdir * sweepdir + outsize) % outsize;
          curpath += dstdir * imodPointDistance(&pts[pt], &pts[next]);
          pt = next;
        }
        st2 = (st2min - dst2[i] * sweepdir + outsize) % outsize;
        dstdir = dst2[i] > 0 ? 1 : -1;
        pt = st2min;
        for (j = 0; j < dstdir * dst2[i]; j++) {
          next = (pt + dstdir * sweepdir + outsize) % outsize;
          curpath += dstdir * imodPointDistance(&pts[pt], &pts[next]);
          pt = next;
        }

        if (curpath <= 0.)
          pt = next;

        /* search on the test list */
        found = 0;
        for (j = 0; j < ntest; j++)
          if (st1test[j] == st1 && st2test[j] == st2) {
            found = 1;
            ratio = testratio[j];
            break;
          }
        if (!found) {
          ratio = evaluate_break(cout, list, used, justinl, njustin, scancont,
                                 pmin, pmax, st1, st2, minratio, curpath,
                                 fullarea, joindir, 0, &innerarea);
          if (ratio > 1.e25)
            return NULL;
          st1test[nexttest] = st1;
          st2test[nexttest] = st2;
          testratio[nexttest++] = ratio;
          if (ntest < nexttest)
            ntest = nexttest;
          if (nexttest == MAXTEST)
            nexttest = 0;
        }
        if (ratio < minratio) {
          minratio = ratio;
          st1min = st1;
          st2min = st2;
          found_data = 1;
          break;
        }
      }
    } while(found_data);

    if (minratio < 1.e20) {
      if (break_contour_inout(cout, st1min, st2min, -joindir, &c1, &c2))
        return NULL;
      area1 = imodContourArea(c1);
      area2 = imodContourArea(c2);
      if (area2 < area1) {
        cinmin = c2;
        coutmin = c1;
      } else {
        cinmin = c1;
        coutmin = c2;
      }

      inscan = imodel_contour_scan(cinmin);
      if (!inscan)
        return NULL;
      imodContourGetBBox(cinmin, &pminin, &pmaxin);

      /* build list of ones that overlap new inner contour */
      ninlist = 0;
      for (i = 0; i < njustin; i++) {
        if (used[justinl[i]])
          continue;
        co = list[justinl[i]];
        if (imodel_scans_overlap(scancont[co], pmin[co], pmax[co],
                                 inscan, pminin, pmaxin)) {
          inlist[ninlist++] = co;
          used[justinl[i]] = 1;
        }
      }

      /* Join these contours, mesh to the created inside one, delete
         them both and the scan; replace the outer contour with the
         new one */
      c1 = join_all_contours(obj, inlist, ninlist, ninlist, joindir, 
                             NULL, 0, 0, cinmin);
      if (joindir < 0) {
        c2 = c1;
        c1 = cinmin;
        cinmin = c2;
      }

      // TODO: get a contour number from cout and manage it
      if (mesh_contours(obj, c1, cinmin, surf, time, scale, oddeven, inlist[0],
                        inlist[0]))
        return NULL;
      imodContourDelete(cout);
      cout = coutmin;
      imodContourDelete(inscan);
    }
  }
  return cout;
}


/* cross_cont tests whether contour cout is crossed by the line segment between
   x1s,y1s and x1e, y1e.  It omits segments containing points st1 and st2 */

static int cross_cont(Icont *cout, float x1s, float y1s, float x1e, float y1e,
                      int st1, int st2)
{
  float dx1, dy1, x2s, x2e, y2s, y2e, dx2, dy2, dxs, dys, tnum, unum, den;
  int pt, next;

  dx1 = x1e - x1s;
  dy1 = y1e - y1s;
  for (pt = 0; pt < cout->psize; pt++) {
    next = (pt + 1) % cout->psize;
    if (pt == st1 || pt == st2 || next == st1 || 
        next == st2)
      continue;
    x2s = cout->pts[pt].x;
    x2e = cout->pts[next].x;
    y2s = cout->pts[pt].y;
    y2e = cout->pts[next].y;
    dx2 = x2s - x2e;
    dy2 = y2s - y2e;
    dxs = x2s - x1s;
    dys = y2s - y1s;
    den = dx1 * dy2 - dy1 * dx2;
    tnum = dxs * dy2 - dys * dx2;
    unum = dx1 * dys - dy1 * dxs;
    if (den < 0) {
      if (tnum <= 0. && unum <= 0. && tnum >= den && unum >= den)
        return 1;
    } else {
      if (tnum >= 0. && unum >= 0. && tnum <= den && unum <= den)
        return 1;
    }
  }
  return 0;
}

static float evaluate_break(Icont *cout, int *list, int *used,
                            int *justinl, int njustin, Icont **scancont,
                            Ipoint *pmin, Ipoint *pmax, int st1, int st2,
                            float curmin, float pathlen, float fullarea,
                            int joindir, int areaonly, float *innerarea)
{
  Ipoint pminin, pmaxin;
  Icont *inscan, *c1, *c2, *cintst, *couttst;
  int co, i;
  float frac1, frac2, areasum, inarea, area1, area2;
  float x1s, x1e, y1s, y1e;
  float dist, ratio, maxarea;
  double areapow = 2.0;

  /* does the biggest possible area, implied by distamce and pathlen,
     not enough to give a lower ratio? */
  dist = imodPointDistance(&cout->pts[st1], &cout->pts[st2]);
  maxarea = (dist + pathlen) * (dist + pathlen) / 12.5664;
  ratio = 1.e20;
  if (maxarea > 0)
    ratio = dist / pow((double)maxarea, areapow);
  if ((!areaonly && ratio > curmin) || (areaonly && maxarea < curmin)) {
    *innerarea = maxarea;
    return (ratio);
  }

  /* second test: does connector cross contour? */
  x1s = cout->pts[st1].x;
  x1e = cout->pts[st2].x;
  y1s = cout->pts[st1].y;
  y1e = cout->pts[st2].y;

  *innerarea = 0.;
  if (cross_cont(cout, x1s, y1s, x1e, y1e, st1, st2))
    return (1.e20);

  /* Make two new contours, require that one be bigger than
     the original; take the other as test inner one */
  if (break_contour_inout(cout, st1, st2, -joindir, &c1, &c2))
    return (1.e30);
  area1 = imodContourArea(c1);
  area2 = imodContourArea(c2);
  if (area1 > fullarea && area2 < area1) {
    cintst = c2;
    couttst = c1;
    inarea = area2;
  } else if (area2 > fullarea) {
    cintst = c1;
    couttst = c2;
    inarea = area1;
  } else {
    imodContourDelete(c1);
    imodContourDelete(c2);
    return (1.e20);
  }

  ratio = 1.e20;
  if (inarea > 0.)
    ratio = dist / pow((double)inarea, areapow);
  if ((!areaonly && ratio > curmin) || (areaonly && inarea < curmin)) {
    *innerarea = inarea;
    imodContourDelete(c1);
    imodContourDelete(c2);
    return (ratio);
  }

  /* scan convert the inner one for testing overlaps */
  inscan = imodel_contour_scan(cintst);
  if (!inscan)
    return (1.e30);
  imodContourGetBBox(cintst, &pminin, &pmaxin);

  /* sum the area of orphans overlapping this contour */
  areasum = 0;
  for (i = 0; i < njustin; i++) {
    if (used[justinl[i]])
      continue;
    co = list[justinl[i]];
    imodel_overlap_fractions(&scancont[co], pmin[co],
                             pmax[co], &inscan, pminin,
                             pmaxin, &frac1, &frac2);
    areasum += frac2 * inarea;
  }
                    
  ratio = 1.e20;
  if (areasum > 0.)
    ratio = dist / pow((double)areasum, areapow);
  *innerarea = areasum;
  imodContourDelete(c1);
  imodContourDelete(c2);
  imodContourDelete(inscan);
  return (ratio);
}

/* Get center of mass; if it fails, take midpoint of bounding box */
static int robustCenterOfMass(Icont *cont, Ipoint *cm)
{
  Ipoint  ll, ur;
  if (imodContourCenterOfMass(cont, cm)) {
    imodContourGetBBox(cont, &ll, &ur);
    cm->x = (ll.x + ur.x) / 2.;
    cm->y = (ll.y + ur.y) / 2.;
    cm->z = (ll.z + ur.z) / 2.;
    return 1;
  }
  return 0;
}


/*
 * Creates a cap mesh on a contour.
 * side has two values: 1 cap on top, -1 cap on bottom.
 *
 */
static Imesh *imeshContourCap(Iobj *obj, Icont *cont, int coNum, int side,
                              int inside, Ipoint *scale)
{
  int     pt;
  int     meshdir = 0;
  /* IMOD_CONTOUR_CLOCKWISE, IMOD_CONTOUR_COUNTER_CLOCKWISE */
  int     direction; 
  int     failedcm = 0;
  DrawProps contProps, objProps;
  int contState, surfState;
  int stateTest = CHANGED_COLOR | CHANGED_FCOLOR | CHANGED_3DWIDTH | 
    CHANGED_TRANS;

  Ipoint  cm;
  Imesh  *m;
  Icont *ncont, *scont;
  double rotation;
  float ratio, length, dely, nexty;

  failedcm = robustCenterOfMass(cont, &cm);
  direction = imodContZDirection(cont);
  if (side == 1)
    cm.z += 0.5f;
  else
    cm.z -= 0.5f;

  /* evaluate elongation and need for a complex cap.
     Find the long axis with a certain precision, limit the ratio to
     half the length so that delta y is at least 2 */
  rotation = 3.14159 / 2. - imodContourLongAxis(cont, 5., &ratio, &length);
  if (ratio > length / 2.)
    ratio = length / 2.;
  if (ratio >= 2.0 && cont->psize > 4 && !failedcm) {
    ncont = imodContourDup(cont);
    if (!ncont)
      return NULL;
    imodContourRotateZ(ncont, rotation);
    scont = imodel_contour_scan(ncont);
    if (!scont)
      return NULL;
    imodContourDelete(ncont);
    ncont = imodContourNew();
    if (!ncont)
      return NULL;
    dely = length / ratio;
    nexty = scont->pts->y + dely / 2.;

    /* find the midpoint of each scan line at the next desired Y */
    for (pt = 0; pt < scont->psize; pt += 2)
      if (scont->pts[pt].y > nexty) {
        cm.x = (scont->pts[pt].x + scont->pts[pt+1].x) / 2.;
        cm.y = nexty;
        nexty += dely;
        imodPointAppend(ncont, &cm);
      }

    /* loop the contour back at the end to make complex cap */
    for (pt = ncont->psize - 2; pt > 0; pt--)
      imodPointAppend(ncont, &ncont->pts[pt]);

    imodContourRotateZ(ncont, -rotation);

    ncont->surf = cont->surf;
    if (side == 1)
      m = imeshContoursCost(obj, cont, ncont, scale, inside, coNum, coNum);
    else
      m = imeshContoursCost(obj, ncont, cont, scale, inside, coNum, coNum);

    imodContourDelete(ncont);
    imodContourDelete(scont);
    return (m);
  }

  /* ccw on top , cw on bottom (unless inside!)*/
  if (( (side < 0) && (direction == IMOD_CONTOUR_COUNTER_CLOCKWISE)) ||
      ( (side > 0) && (direction == IMOD_CONTOUR_CLOCKWISE)))
    meshdir = 1;
  if (inside)
    meshdir = 1 - meshdir;

  pt = istoreContSurfDrawProps(obj->store, &objProps, &contProps, coNum,
                               cont->surf, &contState, &surfState);
  if (!(skinFlags & IMESH_MK_SURF))
    contState = pt;

  return(makeCapMesh(cont, &cm, meshdir, &contProps, contState, stateTest));
}

static void interpolate_point(Ipoint pt1, Ipoint pt2, float frac, Ipoint *pt3)
{
  pt3->x = frac * pt2.x + (1 - frac) * pt1.x;
  pt3->y = frac * pt2.y + (1 - frac) * pt1.y;
  pt3->z = frac * pt2.z + (1 - frac) * pt1.z;
}

static void backoff_overlap(Icont *c1, Icont *c2)
{
  Ipoint pt3, ptbefore, ptafter;
  int firstout, lastout, firstin, lastin;
  int i, before, after;
     
  /* find first point outside */
  for (firstout = 0; firstout < c1->psize; firstout++) {
    if (!imodPointInsideCont(c2, &c1->pts[firstout]))
      break;
  }

  /* give up if never got out */
  if (firstout == c1->psize)
    return;

  /* now find last point out */
  lastout = firstout;
  firstin = lastout + 1;
  for (i = 0; i < c1->psize; i++) {
    if (firstin == c1->psize)
      firstin = 0;
    if (imodPointInsideCont(c2, &c1->pts[firstin]))
      break;
    lastout = firstin;
    firstin++;
  }

  /* give up if never got in */
  if (i == c1->psize)
    return;

  /* find first interpolated point that's outside and go 1 more tenth out*/
  before = 0;
  for (i = 9; i > 1; i--) {
    interpolate_point(c1->pts[lastout], c1->pts[firstin], 0.1 * i, &pt3);
    if (!imodPointInsideCont(c2, &pt3)) {
      interpolate_point(c1->pts[lastout], c1->pts[firstin], 
                        0.1 * (i - 1), &ptbefore);
      /*               printf ("before frac %d\n", i-1); */
      before = 1;
      break;
    }
  }

  /* Now find first point out from here */
  lastin = firstin;
  firstout = lastin + 1;
  for (i = 0; i < c1->psize; i++) {
    if (firstout == c1->psize)
      firstout = 0;
    if (!imodPointInsideCont(c2, &c1->pts[firstout]))
      break;
    lastin = firstout;
    firstout++;
  }

  /* find first interpolated point that's outside and go 1 more tenth out*/
  after = 0;
  for (i = 9; i > 1; i--) {
    interpolate_point(c1->pts[firstout], c1->pts[lastin], 0.1 * i, &pt3);
    if (!imodPointInsideCont(c2, &pt3)) {
      interpolate_point(c1->pts[firstout], c1->pts[lastin], 
                        0.1 * (i - 1), &ptafter);
      /*               printf ("after frac %d\n", i-1); */
      after = 1;
      break;
    }
  }
  /*     printf ("adding at %d\n", lastout + 1); */

  /* Add the two points after the last one out */
  if (before) {
    lastout++;
    imodPointAdd(c1, &ptbefore, lastout);
  }
  if (after) {
    lastout++;
    imodPointAdd(c1, &ptafter, lastout);
  }

  /* Just delete all points inside */
  for (i = 0; i < c1->psize; i++) {
    if (imodPointInsideCont(c2, &c1->pts[i])) {
      imodPointDelete(c1, i);
      /*               printf ("deleting %d\n", i); */
      i--;
    }
  }
}



static int eliminate_overlap(Icont *c1, Icont *c2)
{
  Icont *c1cp;
  int i;
  for (i = 0; i < 2; i++) {
    if (!imodel_contour_overlap(c1, c2))
      return i;
    c1cp = imodContourDup(c1);
    backoff_overlap(c1, c2);
    backoff_overlap(c2, c1cp);
    imodContourDelete(c1cp);
  }
  return 1;
}

#define CHECK_DIVISIONS 0
#define CHECK_MIN_FRACTION 0.45
#define CHECK_MIN_DISTANCE 2.
#define INSIDE_MAX_DISTANCE 0.1
#define INSIDE_MAX_FRACTION 0.1
static int check_legal_joiner(Ipoint pt1, Ipoint pt2, Icont *c1, Icont *c2,
                              int samelevel, Iobj *obj, int *olist,
                              int nother, int nothersame, Icont *other)
{
  Ipoint mid;
  int i, j, retval;
  float len, minfrac, frac, xs1, xs2, ys1, ys2;
  int ncheck = CHECK_DIVISIONS;
  len = sqrt((double)(pt1.x - pt2.x) * (pt1.x - pt2.x) +
             (pt1.y - pt2.y) * (pt1.y - pt2.y));

  /* check that a connector trimmed by 0.1 pixel or 0.1 of length does not
     cross either contour */
  if (len > 1.e-5) {
    frac = INSIDE_MAX_DISTANCE / len;
    if (frac > INSIDE_MAX_FRACTION)
      frac = INSIDE_MAX_FRACTION;
    xs1 = (1. - frac) * pt1.x + frac * pt2.x;
    ys1 = (1. - frac) * pt1.y + frac * pt2.y;

    xs2 = (1. - frac) * pt2.x + frac * pt1.x;
    ys2 = (1. - frac) * pt2.y + frac * pt1.y;
    if (cross_cont(c1, xs1, ys1, xs2, ys2, -1, -1))
      return 0;
    if (cross_cont(c2, xs1, ys1, xs2, ys2, -1, -1))
      return 0;
  }

  /* check one or more points in middle of connector to verify proper
     relation to contours on next slice */
  minfrac = 0.45;
  if (len > 1.e-5)
    minfrac = CHECK_MIN_DISTANCE / len;
  if (minfrac < CHECK_MIN_FRACTION)
    minfrac = CHECK_MIN_FRACTION;
  if (minfrac > 0.5)
    minfrac = 0.5;

  for (j = 0; j <= CHECK_DIVISIONS; j++) {
    /* If contours are at same level, midpoint should be inside the
       basic contours of other level and outside any extra inner 
       contours.  Finding the point inside any one contour is conclusive
       on legality or illegality, respectively */
    retval = 1;
    if (!samelevel)
      retval = 0;

    frac = 0.5;
    if (ncheck > 0)
      frac = minfrac + j * (1. - 2 * minfrac) / ncheck;
    mid.x = (1. - frac) * pt1.x + frac * pt2.x;
    mid.y = (1. - frac) * pt1.y + frac * pt2.y;
    if (other)
      if (imodPointInsideCont(other, &mid))
        return retval;
    for (i = 0; i < nothersame; i++)
      if (imodPointInsideCont(&obj->cont[olist[i]], &mid))
        return retval;
    retval = 1 - retval;
    for (i = nothersame; i < nother; i++)
      if (imodPointInsideCont(&obj->cont[olist[i]], &mid))
        return retval;
  }
  return (retval);
}

static void segment_mm(Icont *cont, int ptst, int ptnd, float *xmin, 
                       float *xmax, float *ymin, float *ymax)
{
  int pt = ptst - 1;
  Ipoint *pts = cont->pts;
  *xmin = pts[ptst].x;
  *ymin = pts[ptst].y;
  *xmax = *xmin;
  *ymax = *ymin;
  do {
    pt = (pt + 1) % cont->psize;
    if (*xmin > pts[pt].x)
      *xmin = pts[pt].x;
    if (*xmax < pts[pt].x)
      *xmax = pts[pt].x;
    if (*ymin > pts[pt].y)
      *ymin = pts[pt].y;
    if (*ymax < pts[pt].y)
      *ymax = pts[pt].y;
  } while (pt != ptnd);
}



#define SEGSCAN_DIV 30
static void scan_points_to_segments(Icont *c1, Icont *c2, double *legalmin,
                                    double *dsqrmin, int *pt1, int *pt2, 
                                    float *tbest, Ipoint *close,
                                    int docheck, int samelevel, Iobj *obj,
                                    int *olist, int nother, int nothersame,
                                    Icont *other)
{
  float dx, dy, xs, ys, xe, ye, x0, y0, dsqr, t, sep;
  int pta, ptb, next, legal, i, j, ind;
  int ndiv1, ndiv2, nperdiv1, nperdiv2, ptst, ptnd, ptand;
  Ipoint *pts1, *pts2;
  Ipoint cltmp;
  float xmin1[SEGSCAN_DIV], xmax1[SEGSCAN_DIV];
  float ymin1[SEGSCAN_DIV], ymax1[SEGSCAN_DIV];
  float xmin2[SEGSCAN_DIV], xmax2[SEGSCAN_DIV];
  float ymin2[SEGSCAN_DIV], ymax2[SEGSCAN_DIV];
  float bbsep[SEGSCAN_DIV * SEGSCAN_DIV];
  int indorder[SEGSCAN_DIV * SEGSCAN_DIV];

  /* divide each contour into segments and get the mins and maxes of each */
  ndiv1 = SEGSCAN_DIV;
  if (c1->psize < ndiv1)
    ndiv1 = c1->psize;
  nperdiv1 = (c1->psize + ndiv1 - 1) / ndiv1;
  ndiv1 = (c1->psize + nperdiv1 - 1) / nperdiv1;
  for (j = 0; j < ndiv1; j++) {
    ptst = nperdiv1 * j;
    ptnd = ptst + nperdiv1 - 1;
    if (ptnd >= c1->psize)
      ptnd = c1->psize - 1;
    segment_mm(c1, ptst, ptnd, &xmin1[j], &xmax1[j], &ymin1[j],
               &ymax1[j]);
  }

  ndiv2 = SEGSCAN_DIV;
  if (c2->psize < ndiv2)
    ndiv2 = c2->psize;
  nperdiv2 = (c2->psize + ndiv2 - 1) / ndiv2;
  ndiv2 = (c2->psize + nperdiv2 - 1) / nperdiv2;
  for (j = 0; j < ndiv2; j++) {
    ptst = nperdiv2 * j;
    ptnd = ptst + nperdiv2;
    if (ptnd >= c2->psize)
      ptnd = 0;
    segment_mm(c2, ptst, ptnd, &xmin2[j], &xmax2[j], &ymin2[j],
               &ymax2[j]);
  }

  /* measure separations between divisions, remember to square them! */
  for (i = 0; i < ndiv1; i++)
    for (j = 0; j < ndiv2; j++) {
      ind = i * ndiv2 + j;
      sep = 0;
      if (sep < xmin1[i] - xmax2[j])
        sep = xmin1[i] - xmax2[j];
      if (sep < xmin2[j] - xmax1[i])
        sep = xmin2[j] - xmax1[i];
      if (sep < ymin1[i] - ymax2[j])
        sep = ymin1[i] - ymax2[j];
      if (sep < ymin2[j] - ymax1[i])
        sep = ymin2[j] - ymax1[i];
      bbsep[ind] = sep * sep;
      indorder[ind] = ind;
    }

  /* order by increasing separation */
  for (i = 0; i < ndiv1 * ndiv2 - 1; i++)
    for (j = i + 1; j < ndiv1 * ndiv2; j++)
      if (bbsep[indorder[i]] > bbsep[indorder[j]]) {
        ind = indorder[i];
        indorder[i] = indorder[j];
        indorder[j] = ind;
      }

  pts1 = c1->pts;
  pts2 = c2->pts;

  /* loop on the divisions in that order, skip ones above current minimum */
  for (i = 0; i < ndiv1 * ndiv2; i++) {
    if (bbsep[indorder[i]] >= *legalmin)
      continue;
    ind = indorder[i] / ndiv2;
    pta = ind * nperdiv1;
    ptand = pta + nperdiv1;
    if (ptand > c1->psize)
      ptand = c1->psize;
    ind = indorder[i] % ndiv2;
    ptst = ind * nperdiv2;
    ptnd = ptst + nperdiv2;    /* DNM fixed 4/9/02, ptb -> ptst */
    if (ptnd > c2->psize)
      ptnd = c2->psize;
    for (; pta < ptand; pta++) {
      x0 = pts1[pta].x;
      y0 = pts1[pta].y;
      xs = pts2[ptst].x;
      ys = pts2[ptst].y;
      for (ptb = ptst; ptb < ptnd; ptb++) {

        /* determine closest approach between point in c1 and
           segment in c2 */
        next = (ptb + 1) % c2->psize;
        xe = pts2[next].x;
        ye = pts2[next].y;
        dx = xe - xs;
        dy = ye - ys;
        t = 0.;
        if (dx != 0. || dy != 0.) {
          t = ((x0 - xs) * dx + (y0 - ys) * dy) /
            (dx * dx + dy * dy);
          if (t < 0.)
            t = 0.;
          if (t > 1.)
            t  = 1.;
        }
        cltmp.x = xs + t * dx;
        cltmp.y = ys + t * dy;
        dx = cltmp.x - x0;
        dy = cltmp.y - y0;
        dsqr = dx * dx + dy * dy;
        if (dsqr < *legalmin) {

          /* if less than the current min, check legality if
             required on this call */
          legal = 1;
          if (docheck)
            legal = check_legal_joiner(pts1[pta], cltmp, c1,
                                       c2, samelevel, obj,
                                       olist, nother,
                                       nothersame, other);
          /* if still legal, save new min */
          if (legal) {
            *legalmin = dsqr;
            *pt1 = pta;
            *pt2 = ptb;
            *tbest = t;
            cltmp.z = pts2[ptb].z;
            *close = cltmp;
                             
          }
        }
        if (*legalmin > 1.e20 && dsqr < *dsqrmin) {

          /* or, if no legal min found yet, keep track of
             nonlegal minimum as a fallback */
          *dsqrmin = dsqr;
          *pt1 = pta;
          *pt2 = ptb;
          *tbest = t;
          cltmp.z = pts2[ptb].z;
          *close = cltmp;
        }
        xs = xe;
        ys = ye;
      }
    }
  }
}

static int find_closest_contour(Icont *tcont, Icont *onecont, Iobj *obj, 
                                int *list, int ncont,
                                int *used, int *pt1min, int *pt2min, 
                                int *firstbest, float *tbest, Ipoint *close,
                                int *olist, int nother, int nothersame, 
                                Icont *other, int nsamelevel)
{
  Icont *jcont;
  Ipoint testpt1, testpt2, closet;
  int jmin, samelevel, j;
  double legalmin, distmin, lastmin;
  int pt1mint, pt2mint;

  /* first pass: find contour with absolute closest approach regardless
     of legality of connector */
  legalmin = 1.e30;
  lastmin = 1.e30;
  distmin = 1.e30;
  if (onecont)
    ncont = 2;
  for (j = 1; j < ncont; j++) {
    jcont = onecont;
    if (!onecont) {
      if (used[j])
        continue;
      jcont = &(obj->cont[list[j]]);
    }

    /* first measure points of tcont to segments of jcont; then 
       vice versa */
    /* Have to use temporary variables for pt1min, pt2min, and close
       because on the PC, scan_points_to_segments may find a new minimum
       and change these values without this calling routine detecting a
       change in legalmin.  Only when a change is detected out here are
       the actual pt1min, pt2min and close modified */
    scan_points_to_segments(tcont, jcont, &legalmin, &distmin,
                            &pt1mint, &pt2mint, tbest, &closet,
                            0, 0, obj, olist, nother, nothersame, other);
    if (legalmin < lastmin) {
      lastmin = legalmin;
      *firstbest = 1;
      *pt1min = pt1mint;
      *pt2min = pt2mint;
      *close = closet;
      jmin = j;
      testpt1 = tcont->pts[*pt1min];
      testpt2 = *close;
    }
    scan_points_to_segments(jcont, tcont, &legalmin, &distmin,
                            &pt2mint, &pt1mint, tbest, &closet,
                            0, 0, obj, olist, nother, nothersame, other);
    if (legalmin < lastmin) {
      lastmin = legalmin;
      *firstbest = 0;
      *pt1min = pt1mint;
      *pt2min = pt2mint;
      *close = closet;
      jmin = j;
      testpt2 = jcont->pts[*pt2min];
      testpt1 = *close;
    }
  }

  samelevel = 1;
  jcont = onecont;
  if (!onecont) {
    jcont = &(obj->cont[list[jmin]]);
    if (jmin >= nsamelevel)
      samelevel = 0;
  }
  if (!check_legal_joiner(testpt1, testpt2, tcont, jcont, samelevel,
                         obj, olist, nother, nothersame, other)) {

    /* if first pass didn't give legal connector, then find closest
       legal connector by brute force, checking each new candidate
       closest distance */
    distmin = 1.e30;
    legalmin = 1.e30;
    lastmin = 1.e30;
    for (j = 1; j < ncont; j++) {
      samelevel = 1;
      jcont = onecont;
      if (!onecont) {
        if (used[j])
          continue;
        jcont = &(obj->cont[list[j]]);
        if (j >= nsamelevel)
          samelevel = 0;
      }

      scan_points_to_segments(tcont, jcont, &legalmin, &distmin,
                              &pt1mint, &pt2mint, tbest, &closet,
                              1, samelevel, obj, olist, nother,
                              nothersame, other);
      if (legalmin < lastmin || 
          (legalmin > 1.e20 && distmin < lastmin)) {
        lastmin = legalmin;
        if (legalmin > 1.e20)
          lastmin = distmin;
        *firstbest = 1;
        *pt1min = pt1mint;
        *pt2min = pt2mint;
        *close = closet;
        jmin = j;
      }
      scan_points_to_segments(jcont, tcont, &legalmin, &distmin,
                              &pt2mint, &pt1mint, tbest, &closet,
                              1, samelevel, obj, olist, nother,
                              nothersame, other);
      if (legalmin < lastmin || 
          (legalmin > 1.e20 && distmin < lastmin)) {
        lastmin = legalmin;
        if (legalmin > 1.e20)
          lastmin = distmin;
        *firstbest = 0;
        *pt1min = pt1mint;
        *pt2min = pt2mint;
        *close = closet;
        jmin = j;
      }
    }
  }

  return(jmin);
}


static Icont *join_all_contours(Iobj *obj, int *list, int ncont, 
                                int nsamelevel, int fill, int *olist, 
                                int nother, int nothersame, Icont *other)
{
  Icont *tcont, *jcont;
  Icont *tempcont;
  int i, j, jmin, tptmin, jptmin, firstbest, counterdir;
  int *used;
  float tbest;
  Ipoint close;
  DrawProps contProps, objProps, ptProps;
  int contState, surfState;
  int genFlags = CHANGED_COLOR | CHANGED_FCOLOR | CHANGED_3DWIDTH | 
    CHANGED_TRANS;

  /* start with first contour */
  tcont = imodContourDup(&(obj->cont[list[0]]));
  if (!tcont)
    return(NULL);
  if (ncont < 2)
    return(tcont);

  /* If there are contour or surface properties, convert them to a property
     for the first point of the contour */
  j = istoreContSurfDrawProps(obj->store, &objProps, &contProps, list[0],
                               tcont->surf, &contState, &surfState);
  if (!(skinFlags & IMESH_MK_SURF))
    contState = j;
  if (contState) {
    j = istoreListPointProps(tcont->store, &contProps, &ptProps, 0);
    contState &= ~j;
    istoreGenerateItems(&tcont->store, &contProps, contState, 0, genFlags);
  }

  used = (int *)malloc(ncont * sizeof(int));
  if (!used)
    return (NULL);
  for (i = 0; i < ncont; i++)
    used[i] = 0;

  /* loop to connect one more contour each time */
  for (i = 1; i < ncont ; i++) {

    jmin = find_closest_contour(tcont, NULL, obj, list, ncont, used,
                                &tptmin, &jptmin, &firstbest, &tbest, 
                                &close, olist, nother, nothersame,
                                other, nsamelevel);

    jcont = imodContourDup(&(obj->cont[list[jmin]]));
    if (!jcont)
      return(NULL);
    used[jmin] = 1;

  /* Transfer contour or surface properties to the first point of contour */
    j = istoreContSurfDrawProps(obj->store, &objProps, &contProps, list[jmin],
                                jcont->surf, &contState, &surfState);
    if (!(skinFlags & IMESH_MK_SURF))
      contState = j;
    if (contState) {
      j = istoreListPointProps(jcont->store, &contProps, &ptProps, 0);
      contState &= ~j;
      istoreGenerateItems(&jcont->store, &contProps, contState, 0, genFlags);
    }

    /* eliminate overlap for contours on the basic level.
       If eliminating overlap actually moved some points, reevaluate
       closest points */
    if (jmin < nsamelevel)
      if (eliminate_overlap(tcont, jcont)) 
        find_closest_contour(tcont, jcont, obj, list, ncont, used,
                             &tptmin, &jptmin, &firstbest, &tbest,
                             &close, olist, nother, nothersame,
                             other, nsamelevel);

    /* Add the connector point if it is inside a segment */
    if (tbest > 0. && tbest < 1.) {
      if (firstbest) {
        jptmin++;
        imodPointAdd(jcont, &close, jptmin);
      } else {
        tptmin++;
        imodPointAdd(tcont, &close, tptmin);
      }
    }    

    counterdir = 0;
    if (jmin >= nsamelevel)
      counterdir = 1;

    tempcont = tcont;
    tcont = imodContourJoin(tempcont, jcont, tptmin, jptmin, fill, 
                            counterdir);
    if (!tcont)
      return(NULL);
    imodContourDelete(tempcont);
    imodContourDelete(jcont);
  }
  /*     for (i = 0; i < tcont->psize; i++)
         printf ("%.1f %.1f %.1f\n", tcont->pts[i].x, tcont->pts[i].y,
         tcont->pts[i].z); */


  free(used);
  return (tcont);
}

static void subtract_scan_contours(Icont *cs1, Icont *cs2)
{
  int i, j, jstrt;
  float e1, s1, e2, s2;

  if (!cs1) return;
  if (!cs2) return;

  jstrt = 0;
  for (i = 0; i < cs1->psize - 1; i+=2)
    for (j = jstrt; j < cs2->psize - 1; j+=2) {
      if (cs1->pts[i].y == cs2->pts[j].y) {

        s1 = cs1->pts[i].x;
        e1 = cs1->pts[i+1].x;
        s2 = cs2->pts[j].x;
        e2 = cs2->pts[j+1].x;
                    
        if (s1 < e2 && s2 < e1) {
          if (s1 < s2 && e1 <= e2)
            cs1->pts[i+1].x = s2;   /* truncate right end */
          else if ( s1 >= s2 && e1 > e2)
            cs1->pts[i].x = e2;     /* truncate left end */
          else if (s1 >= s2 && e1 < e2) {
            /* wipe out the line completely */
            imodPointDelete(cs1, i);
            imodPointDelete(cs1, i);
            i -= 2;
            break;
          } else {
            /* split the line in two */
            imodPointAdd(cs1, &cs2->pts[j], i + 1);
            imodPointAdd(cs1, &cs2->pts[j+1], i + 2);
          }
        }

      } else if (cs1->pts[i].y > cs2->pts[j].y)
        jstrt = j;
      else
        break;
    }
}     

static int mesh_contours(Iobj *obj, Icont *bcont, Icont *tcont, int surf,
                         int time, Ipoint *scale, int inside, int bco, int tco)
{
  Imesh *nmesh;
  if (!tcont || !bcont) {
    b3dError(stderr, "Fatal Error: "
            "Not enough memory to get new contour.\n");
    return(-1);
  }

  if (bcont->psize == 1) imodPointAppend(bcont, bcont->pts);
  if (tcont->psize == 1) imodPointAppend(tcont, tcont->pts);

  if (obj->flags & IMOD_OBJFLAG_OUT)
    inside = 1 - inside;

  nmesh = imeshContoursCost(obj, bcont, tcont, scale, inside, bco, tco);
  if (nmesh) {
    nmesh->surf = surf;
    nmesh->time = time;
    obj->mesh = imodel_mesh_add(nmesh, obj->mesh, &(obj->meshsize));
    free(nmesh);
  }
  imodContourDelete(tcont);
  imodContourDelete(bcont);
  return 0;
}


/*********************************************************/
/* Break a contour into two contours at st1 and st2      */
/* fill =  1, fill in the line connecting contours up.   */
/* fill = -1, fill in the line connecting contours down. */
/* fill =  0, dont fill.                                 */
static int break_contour_inout(Icont *cin, int st1, int st2,  int fill,
                               Icont **cout1, Icont **cout2)
{
  Ipoint point;
  Icont *c1, *c2;
  int pt, pto;
  int reversed = 0;

  if (st2 < st1) {
    reversed = st1;
    st1 = st2;
    st2 = reversed;
    reversed = 1;
  }

  if (!cin)
    return 1;

  /* set up new contour and fill point. */
  c1 = imodContourNew();
  c2 = imodContourNew();
  if (!c1 || !c2)
    return 1;

  /* transfer an open flag to the c1 contour, which will acquire the
     endpoints and opening of cin */
  if (cin->flags & ICONT_OPEN)
    c1->flags |= ICONT_OPEN;

  point.x = (cin->pts[st1].x + cin->pts[st2].x) * 0.5f;
  point.y = (cin->pts[st1].y + cin->pts[st2].y) * 0.5f;
  point.z = (cin->pts[st1].z + cin->pts[st2].z) * 0.5f;
  if (fill == 1)
    point.z += 0.75f;
  if (fill == -1)
    point.z -= 0.75f;

  c1->psize = st1 + 1 + cin->psize - st2;
  c2->psize = st2 + 1 - st1;
  if (fill) {
    c1->psize++;
    c2->psize++;
  }
  c1->pts = (Ipoint *)malloc(c1->psize * sizeof(Ipoint));
  c2->pts = (Ipoint *)malloc(c2->psize * sizeof(Ipoint));

  /* add points to new contours */
  pto = 0;
  for (pt = 0; pt <= st1; pt++)
    c1->pts[pto++] = cin->pts[pt];
  if (fill)
    c1->pts[pto++] = point;
  for (pt = st2; pt < cin->psize; pt++)
    c1->pts[pto++] = cin->pts[pt];
  pto = 0;
  if (fill)
    c2->pts[pto++] = point;
  for (pt = st1; pt <= st2; pt++)
    c2->pts[pto++] = cin->pts[pt];

  /* printf("Break in out size %d  st1 %d  st2 %d new sizes %d %d\n", 
     cin->psize, st1, st2, c1->psize, c2->psize);
     istoreDump(cin->store); */
  /* Transfer properties */
  if (istoreExtractChanges(cin->store, &c1->store, 0, st1, 0, cin->psize))
    return 1;
  if (fill && istoreExtractChanges(cin->store, &c1->store, st1, st1, st1 + 1,
                                   cin->psize))
    return 1;

  if (istoreExtractChanges(cin->store, &c1->store, st2, cin->psize - 1, 
                           st1 + (fill ? 2 : 1), cin->psize))
    return 1;

  if (fill &&istoreExtractChanges(cin->store, &c2->store, st1, st1, 0,
                                  cin->psize))
    return 1;
  
  if (istoreExtractChanges(cin->store, &c2->store, st1, st2, fill ? 1 : 0,
                           cin->psize))
    return 1;

  /* istoreDump(c1->store);
     istoreDump(c2->store); */
  
  *cout1 = c1;
  *cout2 = c2;
  if (reversed) {
    *cout1 = c2;
    *cout2 = c1;
  }
  return 0;
}


/* 

mkmesh.c got the big log from before the split
$Log$
Revision 1.9  2010/04/01 04:12:27  mast
Return empty mesh when object has no contours

Revision 1.8  2008/11/15 21:55:27  mast
Implemented dome caps, fixed bug in getting next Z

Revision 1.7  2008/09/21 17:59:01  mast
Rationalized value range for using sphere or symbold size for tube diameter

Revision 1.6  2008/06/17 20:15:54  mast
Added multiple ways to change tube diameter in a contour

Revision 1.5  2007/10/09 16:45:08  mast
Moved inside_cont to imod library

Revision 1.4  2007/03/31 03:50:33  mast
In meshing open contours, made it ignore a single point in favor of other
contours if there are any.

Revision 1.3  2006/11/05 01:00:32  mast
Added time to tube meshes

Revision 1.2  2006/10/31 15:35:06  mast
Propagated properties into fill point when breaking a contour in/out

Revision 1.1  2006/09/12 14:58:19  mast
Split up and made into new library


*/
