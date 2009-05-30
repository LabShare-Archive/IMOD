/* 
 *  mkmesh.c -- mesh making fuctions.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 * Log at end
 */

#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "mkmesh.h"
#include "istore.h"

typedef struct connect_struct {
  int b1;
  int t1;
  int b2;
  int t2;
  int gap;
  int connect;
  int skipToNext;
  int skipToEnd;
  int skipFromStart;
  int skipIndex;
} Connector;



static void cost_from_area_matrices(float *up, float *down, float *cost,
                                    char *path, int bdim, int tdim, int bsize,
                                    int sb, int st, int bmax,
                                    int tmax, float curmin);
static void build_area_matrices(Icont *bc, int bdir,
                                Icont *tc, int tdir, Ipoint *scale,
                                float *up, float *down);
static Connector *makeConnectors(Icont *bc, Icont *tc, int *numCon, 
                                 int closedObj, int dirProduct);
static int outsideMeshLimits(Ipoint *p1, Ipoint *p2, Ipoint *p3);
static void chunkAddTriangle(Imesh *mesh, int i1, int i2, int i3, int *maxsize,
                             int inside);
static void invertConnectors(Connector *connects, int numCon,
                             int direction[2]);

static int fastmesh = 0;
static  Ipoint meshMin = {-1.e30, -1.e30, -1.e30};
static  Ipoint meshMax = {1.e30, 1.e30, 1.e30};
static unsigned int skinFlags;

void imeshSetMinMax(Ipoint inMin, Ipoint inMax)
{
  meshMin = inMin;
  meshMax = inMax;
}
void imeshSetSkinFlags(int inFlag, int inFast)
{
  skinFlags = inFlag;
  fastmesh = inFast;
}

/*!
 * Creates and returns a mesh between two contours using a minimum area
 * cost analysis.  The bottom and top (low Z and high Z) contours are [bc] and
 * [tc], and [obj] is the object containing them, and their contour indices are
 * [bco] and [tco].  These indices are used to look up general storage 
 * properties of the contours in the object.  [scale] contains the X, Y, 
 * and Z scale, typically 1, 1, and the model Z scale.  A non-zero value of 
 * [inside] indicates that the contours are inside others, at an even nesting 
 * level.  Contours from open contour objects should have the flag ICONT_OPEN
 * set before calling.  Returns NULL for error.
 */
Imesh *imeshContoursCost(Iobj *obj, Icont *bc, Icont *tc, Ipoint *scale,
                                int inside, int bco, int tco)
{
  Imesh *mesh;
  int i, j, k, pt, jo, io, pt1, pt2, pt3;
  int tdim, bdim, tlen, blen;
  int bsize, tsize, csize, totind;
  int dofast;
  int bsi, tsi, li, lj, step, iskip, jskip, endb, endt, jobase, lc;
  int maxsize;
  float mincost, ccost;
  float *up, *down, *cost;
  char *path;
  Ipoint minp, maxp;
  float dista, distb, valmin, valmax;
  DrawProps objProps, bcProps, tcProps, ptProps;
  DrawProps *props3;
  Ilist *store3;
  int surfState, bcState, tcState, state3, anyTrans, transMax;
  int  p1State, p2State, p3State, k2, k3, trans1, trans2, trans3;
  int stateTest = CHANGED_COLOR | CHANGED_FCOLOR | CHANGED_3DWIDTH;
  Connector *connects;
  int numCon, curCon, nextCon, startedAtCon = 0, bothOpen;
  int startsConnected = 0, endsConnected = 0;

  /* Index [0] is bottom contour, Index [1] is top contour. */
  int si[2];                /* start index */
  int direction[2];        /* contour direction. */
  int /*siFirst[2],*/ siCopy[2];
     
  /* Check input data. */
  if ((bc == NULL) || (!bc->psize) || (tc == NULL) || (!tc->psize))
    return(NULL);

  /* Init internal data. */
  mesh = imodMeshNew();
  if (!mesh) 
    return(NULL);
  maxsize = 0;
  if (istoreGetMinMax(obj->store, obj->contsize, GEN_STORE_MINMAX1, &valmin, 
                   &valmax))
    stateTest |= CHANGED_VALUE1;

  /* Contours from an open contour object should be marked as open */
  bothOpen = ((bc->flags & ICONT_OPEN) && (tc->flags & ICONT_OPEN)) ? 1 : 0;
  tsize = tc->psize;
  tdim = tsize;
  bsize = bc->psize;
  bdim = bsize;
  if (bothOpen) {
    tdim--;
    bdim--;
  }
  csize = (tdim + 1) * (bdim + 1);
  totind = csize - 1;

  up = (float *)malloc(sizeof(float) * bsize * tsize);
  down = (float *)malloc(sizeof(float) * tsize * bsize);
  cost = (float *)malloc(sizeof(float) * csize);
  path = (char *)malloc(sizeof(char) * csize);
  if (!down || !up || !cost || !path) {
    free(mesh);
    if (up)
      free(up);
    if (down)
      free(down);
    if (cost)
      free(cost);
    if (path)
      free(path);
    return(NULL);
  }

  /* Get default drawing properties for each contour, and flags for what is
     already changed from default.  If connecting by surface, leave the surface
     properties out of the contour flags, but otherwise use the whole set of
     flags to incorporate surface info into the mesh. */
  istoreDefaultDrawProps(obj, &objProps);
  i = istoreContSurfDrawProps(obj->store, &objProps, &bcProps, bco, bc->surf, 
                          &bcState, &surfState);
  j = istoreContSurfDrawProps(obj->store, &objProps, &tcProps, tco, tc->surf, 
                          &tcState, &surfState);
  if (!(skinFlags & IMESH_MK_SURF)) {
    bcState = i;
    tcState = j;
  }

  anyTrans = (bcState & CHANGED_TRANS) | (tcState & CHANGED_TRANS) | 
    istoreCountItems(bc->store, GEN_STORE_TRANS, 1) |
    istoreCountItems(tc->store, GEN_STORE_TRANS, 1);
  /*printf("bco %d surf %d state %d %d tco %d surf %d state %d %d\n", bco,
          bc->surf, bcState, i, tco, tc->surf, tcState, j);
          istoreDump(obj->store);*/

  /* Find direction of each contour.
   * If contours aren't flat in a Z plane, they must already be
   * in the correct orientation.
   */
  direction[0] = imodContZDirection(bc);
  direction[1] = imodContZDirection(tc);
  if (!direction[0])
    direction[0] = 1;
  if (!direction[1])
    direction[1] = 1;

  si[0] = si[1] = 0;
  dofast = 1;

  /* Get connectors if any */
  connects = makeConnectors(bc, tc, &numCon, iobjClose(obj->flags), 
                            direction[0] * direction[1]);
  curCon = 0;
  lc = numCon - 1;

  /* Set starting index for both contours. */
  if (bothOpen) {
    dofast = fastmesh;

    /* If BOTH CONTOURS OPEN, then the starting points are endpoints 
       regardless of connectors */
    /* If the OBJECT TYPE IS OPEN CONTOUR, then ignore the
       computed contour directions; set directions to +, and
       invert the second one if that makes them match up better */
    if (!iobjClose(obj->flags)) {
      direction[1] = direction[0] = 1;

      /* But first see if starts or ends are connected and use that to set
       the polarity.  Also set polarity if there is more than 1 connector */
      if (numCon) {
        startsConnected = ((!connects[0].b1 && 
                            (!connects[0].t1 || connects[0].t2 == tsize - 1))
                           || connects[0].skipFromStart) ? 1 : 0;
        endsConnected = ((connects[lc].b2 == bsize - 1 && 
                          (!connects[lc].t1 || connects[lc].t2 == tsize - 1))
                         || connects[lc].skipToEnd) ? 1 : 0;
      }

      if (startsConnected || endsConnected || numCon > 1) {
        if ((startsConnected && ((connects[0].t2 == tsize - 1) || 
                                 (!connects[0].t1 && connects[0].skipFromStart
                                  && connects[0].skipIndex > connects[0].b2)))
            || (endsConnected && (!connects[lc].t1 || 
                                  (connects[lc].b2 == bsize - 1 &&
                                   connects[lc].skipToEnd &&
                                   connects[lc].skipIndex < connects[lc].t1))
                || (numCon > 1 && connects[1].t1 < connects[0].t1)))
          direction[1] = -1;
    
      } else {

        /* If there is no polarity info, now use endpoint distances */
        dista = imodPointDistance(bc->pts, &tc->pts[0]) + imodPointDistance
          (&bc->pts[bsize - 1], &tc->pts[tsize - 1]);
        distb = imodPointDistance(bc->pts, &tc->pts[tsize-1]) + 
          imodPointDistance(&bc->pts[bsize - 1], &tc->pts[0]);
        if (distb < dista)
          direction[1] = -1;
      }

      /* If either one is marked as inverted before and is NOT inverted this 
         time, invert them both */
      if (((bc->flags & ICONT_CONNECT_INVERT) && 
           (bc->flags & ICONT_CONNECT_BOTTOM)) ||
          ((tc->flags & ICONT_CONNECT_TOP) && 
           direction[1] != ((tc->flags & ICONT_CONNECT_INVERT) ? -1 : 1))) {
        direction[0] *= -1;
        direction[1] *= -1;
      }
      /* printf("direction %d %d\n", direction[0], direction[1]); */

      /* Then set the invert flag for whichever is now inverted */
      setOrClearFlags(&bc->flags, ICONT_CONNECT_INVERT, 1 - direction[0]);
      setOrClearFlags(&tc->flags, ICONT_CONNECT_INVERT, 1 - direction[1]);
    }

    /* Still BOTH OPEN, now either type of object */
    /* Invert the direction of connections if bottom is reversed */
    if (numCon)
      invertConnectors(connects, numCon, direction);

    /* Invert the starting points for reversed directions and (re-)evaluate 
       if starts or ends are connected */
    if (direction[0] < 0)
      si[0] = bsize - 1;
    if (direction[1] < 0)
      si[1] = tsize - 1;

    if (numCon) {
      startsConnected = ((connects[0].b1 == si[0] && connects[0].t1 == si[1]) 
                         || connects[0].skipFromStart) ? 1 : 0;
      endsConnected = ((connects[lc].b2 ==  bsize - 1 - si[0] && 
                        connects[lc].t2 ==  tsize - 1 - si[1]) ||
                       connects[lc].skipToEnd) ? 1 : 0;

      /* Finally, if the starts are connected, use those to set indexes */
      /* WHY IS THIS B2/T2? SEEMS TO WORK FOR Closed Obj, BUT NOT OPEN */
      if (startsConnected) {
        si[0] = iobjClose(obj->flags) ? connects[0].b2 : connects[0].b1;
        si[1] = iobjClose(obj->flags) ? connects[0].t2 : connects[0].t1;
        curCon = 1;
      }
      /* printf("connected %d %d si %d %d\n", startsConnected, endsConnected,
         si[0], si[1]); */
    }

  } else {

    /* NOT BOTH OPEN: if connectors, start at the first connector and set flag,
       but reverse connectors if bottom is inverted */
    if (numCon) {
      invertConnectors(connects, numCon, direction);
      si[0] = connects[0].b2;
      si[1] = connects[0].t2;
      startedAtCon = 1;
      curCon = 1;

      /* If either one is open, start at closest point in other contour */
    } else if (bc->flags & ICONT_OPEN) {
      si[1] = imodContourNearest(tc, bc->pts);

    } else if (tc->flags & ICONT_OPEN) {
      si[0] = imodContourNearest(bc, tc->pts);

    } else {

      /* Both closed contours */
      dofast = fastmesh;
                    
      /* Try to have all mesh start at about the same place
       * so it looks better with fake transparency.
       */
      imodContourGetBBox(bc, &minp, &maxp);
      si[0] = imodContourNearest(bc, &minp);
                    
      /* Now find a similar point in the top contour. */
                    
      imodContourGetBBox(tc, &minp, &maxp);
      si[1] = imodContourNearest(tc,  &minp); 

      /* DNM: deleted attempt to get nearest points; it doesn't work any 
         better than going for corner points: basically, need to do two 
         passes to have a good starting connector  */
    }
  }
     
  /*
   * Build the matrices of areas for up and down triangles
   */
  build_area_matrices(bc, direction[0], tc, direction[1], scale, up, down);

  //siFirst[0] = si[0];
  //siFirst[1] = si[1];

  for (; curCon <= numCon - endsConnected; curCon++) {

    blen = bdim;
    tlen = tdim;

    if (curCon <= 0 || !connects[curCon - 1].skipToNext) {

      /* If there are connectors, need to revise the endpoints */
      if (numCon) {
        
        
        /* index for endpoints is the current connector index, unless at the 
           end.  In that case it is the first connector or the other end of 
           both open contours */
        nextCon = curCon;
        if (curCon >= numCon)
          nextCon = startedAtCon ? 0 : -1;
        if (nextCon >= 0) {
          endb = connects[nextCon].b1;
          endt = connects[nextCon].t1;
        } else {
          endb = direction[0] > 0 ? bsize - 1 : 0;
          endt = direction[1] > 0 ? tsize - 1 : 0;
        }

        /* get the extent to mesh; routine will mesh 0 to dim inclusive.
           If both open contours, length can be zero and zero must not wrap */
        blen = direction[0] > 0 ? endb - si[0] : si[0] - endb;
        if (blen < 0 || (!bothOpen && blen == 0))
          blen += bsize;
        tlen = direction[1] > 0 ? endt - si[1] : si[1] - endt;
        if (tlen < 0 || (!bothOpen && tlen == 0))
          tlen += tsize;

        /*printf("%d %d %d %d %d %d %d %d %d %d\n", bdim, direction[0], si[0], 
          endb, blen, tdim, direction[1], si[1],  endt, tlen);
        fflush(stdout); */
      }
 
      /* flip starting indexes if directions are reversed */
      bsi = si[0];
      if (direction[0] < 0)
        bsi = bsize - 1 - bsi;
      tsi = si[1];
      if (direction[1] < 0)
        tsi = tsize - 1 - tsi;


      /* Get the cost and path matrices for these starting points */
    
      cost_from_area_matrices(up, down, cost, path, bdim, tdim,
                              bsize, bsi, tsi, blen, tlen, -1.0);
       
      /* optimize connections only if there are no connectors */
      if (!numCon) {
        if (dofast) {
        
          if (!(bc->flags & ICONT_OPEN) && 
              !(tc->flags & ICONT_OPEN)) {
          
            /* If both closed contours, go halfway around to find a
               new starting point, and use that instead */
            i = bdim;
            j = tdim;
            for (step = 0; step < (bdim + tdim) / 2; step++) {
              if (path[i + j * (bdim + 1)])
                i--;
              else
                j--;
            }
            bsi = (bsi + i) % bsize;
            tsi = (tsi + j) % tsize;
            cost_from_area_matrices(up, down, cost, path, bdim, tdim,
                                    bsize, bsi, tsi, bdim, tdim, -1.0);
          } 
        } else {
        
          /* Time-consuming: find starting point that gives minimum  area */
     
          mincost = cost[totind];
        
          if (tc->flags & ICONT_OPEN  || bc->flags & ICONT_OPEN) {
            /* For open contours, just do reverse direction */
          
            build_area_matrices(bc, direction[0], tc, -direction[1],
                                scale, up, down);
            cost_from_area_matrices(up, down, cost, path, bdim, tdim,
                                    bsize, bsi, tsi, bdim, tdim, mincost);

            /* If it's better, flip direction; otherwise rebuild areas */
            if (cost[totind] < mincost)
              direction[1] *= -1;
            else
              build_area_matrices(bc, direction[0], tc, direction[1],
                                  scale, up, down);
          } else {
          
            for (i = 0; i < tsize; i++) {
              cost_from_area_matrices(up, down, cost, path, bdim, tdim,
                                      bsize, bsi, i, bdim, tdim, mincost);
                 
              ccost = cost[totind];
            
              if (ccost < mincost) {
                mincost = ccost;
                tsi = i;
                /*             printf ("* "); */
              }
              /*           printf("%d %d %g\n",si[0], i, ccost); */
            }
          }

          /* Redo path from final starting indices and direction */
          cost_from_area_matrices(up, down, cost, path, bdim, tdim,
                                  bsize, bsi, tsi, bdim, tdim, -1.0);
        }
      }

      /* get back the starting indexes in terms of unreversed data */
      if (direction[0] < 0)
        bsi = bsize - 1 - bsi;
      if (direction[1] < 0)
        tsi = tsize - 1 - tsi;

      if (!mesh->vsize) {

        /*
         * first time through, copy contour points to mesh vert array
         */
        siCopy[0] = bsi;
        siCopy[1] = tsi;
        mesh->vsize = bsize + tsize + 2;
        mesh->vert = (Ipoint *)malloc(mesh->vsize * sizeof(Ipoint));
        if (mesh->vert == NULL) {
          free(cost);
          free(up);
          free(down);
          free(mesh);
          free(path);
          return(NULL);
        }
      
        /* copy bottom contour data to mesh array. */
        pt = siCopy[0];
        for (i = 0; i < bsize; i++) {
          mesh->vert[i] = bc->pts[pt];
          pt += direction[0];
          if (pt == bsize) {
            pt = 0;
            iskip = i;
          } else if (pt < 0) {
            pt = bsize - 1;
            iskip = i;
          }
        }
        mesh->vert[bsize] = bc->pts[siCopy[0]];
     
        /* copy top contour data to mesh vert array. */
        for (i = bsize + 1, pt = siCopy[1]; i < mesh->vsize - 1; i++) {
          mesh->vert[i] = tc->pts[pt];
          pt += direction[1];
          if (pt == tsize) {
            pt = 0;
            jskip = i - (bsize + 1);
          } else if (pt < 0) {
            pt = tsize - 1;
            jskip = i - (bsize + 1);
          }
        }
        mesh->vert[mesh->vsize - 1] = tc->pts[siCopy[1]];
      }

      /* The connection loop.  Set starting indexes and offsets  */
      i = blen;
      j = tlen;
      io = (direction[0] * (bsi - siCopy[0]) + bsize) % bsize;
      jobase = (direction[1] * (tsi - siCopy[1]) + tsize) % tsize;
      jo = jobase + bsize + 1;

      /* Do we need to add a triangle at the terminal connector? */
      if (numCon && nextCon >= 0 && !connects[nextCon].gap) {
        if (connects[nextCon].b2 != connects[nextCon].b1) {
          i++;
          path[i + j * (bdim + 1)] = 1;
        } else if (connects[nextCon].t2 != connects[nextCon].t1) {
          j++;
          path[i + j * (bdim + 1)] = 0;
        }
      }
      /* printf("%d %d %d %d\n", bsi, siCopy[0], tsi, siCopy[1]);
         printf("%d %d %d %d\n", i, io, j, jobase);
         fflush(stdout); */

      /* Now make the mesh by following the path. */
      while( (i) || (j) ) {
      
        k = mesh->lsize;
        if (path[i + j * (bdim + 1)]) {
          li = i - 1;
          pt3 = (bsi + direction[0] * i + bsize) % bsize;
          pt2 = (bsi + direction[0] * li + bsize) % bsize;
          pt1 = (tsi + direction[1] * j + tsize) % tsize;
          if (!(((bc->flags & ICONT_OPEN) && li + io == iskip) || 
                istorePointIsGap(bc->store, direction[0] > 0 ? pt2 : pt3) ||
                outsideMeshLimits(&tc->pts[pt1], &bc->pts[pt2], 
                                  &bc->pts[pt3]))){
            /* printf("bot %d %d %d\n", pt1, pt2, pt3); */
            chunkAddTriangle(mesh, j+jo, li+io, i+io, &maxsize, inside);
            state3 = bcState;
            props3 = &bcProps;
            store3 = bc->store;
          }
          i--;

        } else {
          lj = j - 1;
          pt3 = (tsi + direction[1] * j + tsize) % tsize;
          pt1 = (tsi + direction[1] * lj + tsize) % tsize;
          pt2 = (bsi + direction[0] * i + bsize) % bsize;
          if (!(((tc->flags & ICONT_OPEN) && lj + jobase == jskip) ||
                istorePointIsGap(tc->store, direction[1] > 0 ? pt1 : pt3) ||
                outsideMeshLimits(&tc->pts[pt1], &bc->pts[pt2],
                                  &tc->pts[pt3]))){
            /* printf("top %d %d %d\n", pt1, pt2, pt3); */

            chunkAddTriangle(mesh, lj+jo, i+io, j+jo, &maxsize, inside);
            state3 = tcState;
            props3 = &tcProps;
            store3 = tc->store;
          }
          j--;
        }
      
        if (mesh->lsize > k) {
          if (!k)
            k++;
          k2 = inside ? k + 2 : k + 1;
          k3 = inside ? k + 1 : k + 2;
          if ((tcState & stateTest) || tc->store)
            istoreGenPointItems(tc->store, &tcProps, tcState, pt1,
                                &mesh->store, k, stateTest);
          if ((state3 & stateTest) || store3)
            istoreGenPointItems(store3, props3, state3, pt3, &mesh->store,
                                k3, stateTest);
          if ((bcState & stateTest) || bc->store)
            istoreGenPointItems(bc->store, &bcProps, bcState, pt2, 
                                &mesh->store, k2, stateTest);

          /* Handle trans states - if any point has a positive trans, set all
             trans to at least 1 so triangle is recognized as trans */
          if (anyTrans) {
            p1State = istoreListPointProps(tc->store, &tcProps, &ptProps, pt1);
            trans1 = transMax = ptProps.trans;
            p2State = istoreListPointProps(bc->store, &bcProps, &ptProps, pt2);
            trans2 = ptProps.trans;
            transMax = B3DMAX(transMax,ptProps.trans);
            p3State = istoreListPointProps(store3, props3, &ptProps, pt3);
            trans3 = ptProps.trans;
            transMax = B3DMAX(transMax,ptProps.trans);
            if ((tcState | bcState | p1State | p2State | p3State) & 
                CHANGED_TRANS) {
              if (transMax && !trans1)
                trans1 = 1;
              if (transMax && !trans2)
                trans2 = 1;
              if (transMax && !trans3)
                trans3 = 1;
              ptProps.trans = trans1;
              istoreGenerateItems(&mesh->store, &ptProps, CHANGED_TRANS, k, 
                                  CHANGED_TRANS);
              ptProps.trans = trans2;
              istoreGenerateItems(&mesh->store, &ptProps, CHANGED_TRANS, k2, 
                                  CHANGED_TRANS);
              ptProps.trans = trans3;
              istoreGenerateItems(&mesh->store, &ptProps, CHANGED_TRANS, k3, 
                                  CHANGED_TRANS);
            }
          }
        }
      }
    }
    /* Set starting indexes for next round */
    if (curCon < numCon) {
      si[0] = connects[curCon].b2;
      si[1] = connects[curCon].t2;
    }
  }

  /* istoreDump(mesh->store); */
  if (numCon)
    free(connects);
  free(path);
  free(cost);
  free(up);
  free(down);

  if (mesh->lsize)
    chunkMeshAddIndex(mesh, IMOD_MESH_ENDPOLY, &maxsize);
  imodMeshAddIndex(mesh, IMOD_MESH_END);
  return(mesh);
}

/*
 * Build the matrices of areas for up and down triangles
 */
static void build_area_matrices(Icont *bc, int bdir,
                                Icont *tc, int tdir, Ipoint *scale,
                                float *up, float *down)
{
  int k, l, i, j, ni, nj, lbase, bsize, tsize, tdim, bdim;
  Ipoint *bpt, *tpt;

  bsize = bc->psize;
  tsize = tc->psize;
  bpt = bc->pts;
  tpt = tc->pts;
  bdim = bsize;
  tdim = tsize;
  if ((bc->flags & ICONT_OPEN) && (tc->flags & ICONT_OPEN)) {
    bdim--;
    tdim--;
  }

  i = 0;
  if (bdir < 0)
    i = bsize - 1;
  j = 0;
  if (tdir < 0)
    j = tsize - 1;

  for (l = 0; l < tsize; l++) {
    nj = j + tdir;
    if (nj == tsize)
      nj = 0;
    if (nj < 0)
      nj = tsize - 1;
    lbase = l * bsize;
    for (k = 0; k < bsize; k++) {
      ni = i + bdir;
      if (ni == bsize)
        ni = 0;
      if (ni < 0)
        ni = bsize - 1;
      if (k == bdim)
        up[k + lbase] = 0.;
      else
        up[k + lbase] = imodPointAreaScale(&tpt[j], &bpt[i], 
                                           &bpt[ni], scale);
      if (l == tdim)
        down[k + lbase] = 0.;
      else
        down[k + lbase] = imodPointAreaScale(&tpt[j], &tpt[nj], 
                                             &bpt[i], scale);
      i = ni;
    }
    j = nj;
  }
}

/*
 * Compute the minimum area path to every possible connection, thus allowing
 * one to follow a minimum area path from ending to starting connection
 */
static void cost_from_area_matrices(float *up, float *down, float *cost,
                                    char *path, int bdim, int tdim, int bsize,
                                    int sb, int st, int bmax,
                                    int tmax, float curmin)
{
  int i, j, k, l, jl, il, ind, lbase;
  float rowmin, costup, costdown;

  cost[0] = 0;
  j = st;
  if (j == tdim)
    j = 0;
  rowmin = 0.0;
  for (l = 0; l <= tmax; l ++) {
    lbase = l * (bdim + 1);
    i = sb;
    if (i == bdim)
      i = 0;
    for (k = 0; k <= bmax; k++) {
      ind = k + lbase;
      if (!k) {

        /* If in first column, add area of down triangles */
        if (l) {
          cost[ind] = cost[ind - (bdim + 1)] + 
            down[i + jl * bsize];
          path[ind] = 0;
          rowmin = cost[ind];
        }
      } else {
        if (!l) {

          /* If in first row, add area of up triangles */
          cost[ind] = cost[ind - 1] + up[il + j * bsize];
          path[ind] = 1;
        } else {

          /* Otherwise figure out which is smaller and add area
             and save path direction for that */
          costdown = cost[ind - (bdim + 1)] + 
            down[i + jl * bsize];
          costup = cost[ind - 1] + up[il + j * bsize];

          if (costdown < costup) {
            cost[ind] = costdown;
            path[ind] = 0;
          } else {
            cost[ind] = costup;
            path[ind] = 1;
          }
          /* keep track of minimum along the row */
          if (cost[ind] < rowmin)
            rowmin = cost[ind];
        }
      }

      /* adjust row index into area matrices */
      il = i;
      i++;
      if (i == bdim)
        i = 0;
    }

    /* adjust column index into area matrices */
    jl = j;
    j++;
    if (j == tdim)
      j = 0;

    /* If there is a current minimum and row min exceeds it, abort */
    if (curmin >= 0.0 && rowmin > curmin) {
      cost[bmax + tmax * (bdim + 1)] = rowmin;
      return;
    }
  }
}

/*
 * Analyze contour stores for connectors and encode them in an array of
 * structures with the bottom and top indices.
 */
static Connector *makeConnectors(Icont *bc, Icont *tc, int *numCon, 
                                 int closedObj, int dirProduct)
{
  int maxCon, maxTop, i, j, used, index, k, bothOpen, tsize, start, last, mid;
  int nj, connum1, connum2, openDir = 0;
  Istore *stp, *stp2;
  Connector *conn, *connp;
  *numCon = 0;
  maxCon = istoreCountItems(bc->store, GEN_STORE_CONNECT, 0);
  maxTop = istoreCountItems(tc->store, GEN_STORE_CONNECT, 0);
  maxCon = B3DMIN(maxCon, maxTop);
  if (!maxCon)
    return NULL;
  conn = (Connector *)malloc(maxCon * sizeof(Connector));
  if (!conn)
    return NULL;

  bothOpen = ((bc->flags & ICONT_OPEN) && (tc->flags & ICONT_OPEN)) ? 1 : 0; 
  tsize = tc->psize;

  for (i = 0; i < ilistSize(bc->store); i++) {
    stp = istoreItem(bc->store, i);
    if (stp->type == GEN_STORE_CONNECT) {

      /* First see if the connect # is used already */
      used = 0;
      for (j = 0; j < *numCon; j++)
        if (stp->value.i == conn[j].connect)
          used = 1;
      if (used)
        continue;

      /* Next look for connect in top contour */
      used = 0;
      for (j = 0; j < ilistSize(tc->store); j++) {
        stp2 = istoreItem(tc->store, j);
        if (stp2->type == GEN_STORE_CONNECT && stp2->value.i == stp->value.i) {
          used = 1;
          break;
        }
      }
  
      if (!used)
        continue;


      /* Check for direction consistency */
      if (*numCon) {
        start = conn[0].t1;
        last = conn[*numCon - 1].t1;
        mid = stp2->index.i;

        if (closedObj && bothOpen) {
          
          /* For open contours of closed object, direction is set, any one 
             point is OK but the next point must be in the right direction */
          if (dirProduct * (mid - start) < 0)
            continue;
        } else if (closedObj) {

          /* For closed contours, direction is set, any two points are OK but
             the next point must be between the last and the end.  This is 
             assessed by making sure sum of distances from last to next and 
             next to start is the same as distance from last to start */
          if (*numCon > 1 &&
              (dirProduct * (mid - last) + tsize) % tsize + 
              (dirProduct * (start - mid) + tsize) % tsize !=
              (dirProduct * (start - last) + tsize) % tsize)
            continue;
        } else {
          
          /* For open object, direction 1 can flip so any two points are OK, 
             but next point must change in same direction as the last */
          if (*numCon == 2)
            openDir = (last - start > 0) ? 1 : -1;
          if ((mid - last) * openDir < 0)
            continue;
        }
      }


      /* Define the connector */
      connp = &conn[(*numCon)++];
      connp->b1 = stp->index.i;
      connp->t1 = stp2->index.i;
      connp->b2 = stp->index.i;
      connp->t2 = stp2->index.i;
      connp->gap = 0;
      connp->connect = stp->value.i;
      connp->skipToNext = 0;
      connp->skipToEnd = 0;
      connp->skipFromStart = 0;
      connp->skipIndex = 0;

      /* Look for explicit neighboring 3rd point in bottom */
      used = 0;
      for (k = 0; !used && k < ilistSize(bc->store); k++) {
        stp = istoreItem(bc->store, k);
        if (stp->flags & (GEN_STORE_NOINDEX | 3))
          break;
        if (stp->type == GEN_STORE_CONNECT && stp->value.i == connp->connect) {
          if  (stp->index.i == connp->b1 + 1) {
            connp->b2++;
            used = 1;
          }
          
          /* If connector is at zero and this is an end of cont, back off
             the starting index to the end of the cont */
          if (stp->index.i == bc->psize - 1 && connp->b1 == 0 && !bothOpen) {
            connp->b1 = bc->psize - 1;
            used = 1;
          }
        }
      }
             
      /* Look for explicit neighboring 3rd point in top */
      for (k = 0; !used && k < ilistSize(tc->store); k++) {
        stp = istoreItem(tc->store, k);
        if (stp->flags & (GEN_STORE_NOINDEX | 3))
          break;
        if (stp->type == GEN_STORE_CONNECT && stp->value.i == connp->connect) {
          if  (stp->index.i == connp->t1 + 1) {
            connp->t2++;
            used = 1;
          }
          if (stp->index.i == tc->psize - 1 && connp->t1 == 0 && !bothOpen) {
            connp->t1 = tc->psize - 1;
            used = 1;
          }
        }
      }

      /* If a third point was found, check for gap */
      if ((connp->b1 != connp->b2 && istorePointIsGap(bc->store, connp->b1)) ||
          (connp->t1 != connp->t2 && istorePointIsGap(tc->store, connp->t1)))
          connp->gap = 1;

      /*printf("%d %d %d %d %d %d\n", connp->connect, connp->b1, connp->b2, 
        connp->t1, connp->t2, connp->gap);*/
    }
  }

  /* Now that all connectors are gotten, check for gaps before and after 
     each point and introduce an implied third point on other side of the 
     gap as long as the point does not appear in another connector */
  for (j = 0; j < *numCon; j++) {
    connp = &conn[j];
    
    /* Skip if there are already three points and it is NOT a gap */
    if (!connp->gap && (connp->b1 != connp->b2 || connp->t1 != connp->t2))
      continue;
    
    /* Look forward on bottom.  Extend if not already extended and there is
       not another connector with this point */
    index = (connp->b1 + 1) % bc->psize;
    if (connp->b1 == connp->b2 && istorePointIsGap(bc->store, connp->b1) &&
        !(connp->b1 == bc->psize - 1 && bothOpen)) {
      used = 0;
      for (i = 0; i < *numCon; i++) {
        if ((i != j) && (conn[i].b1 == index || conn[i].b2 == index))
          used = 1;
      }
      if (!used && istoreConnectNumber(bc->store, index) >= 0) {
        connp->b2 = index;
        connp->gap = 1;
      }
    }
    
    /* Look backward on bottom */
    index = (bc->psize + connp->b1 - 1) % bc->psize;
    if (connp->b1 == connp->b2 && istorePointIsGap(bc->store, index) && 
        !(index == bc->psize - 1 && bothOpen)) {
      used = 0;
      for (i = 0; i < *numCon; i++) {
        if ((i != j) && (conn[i].b1 == index || conn[i].b2 == index))
          used = 1;
      }
      if (!used && istoreConnectNumber(bc->store, index) >= 0) {
        connp->b1 = index;
        connp->gap = 1;
      } 
    }         
      
    /* Look forward on top */
    index = (connp->t1 + 1) % tc->psize;
    if (connp->t1 == connp->t2 && istorePointIsGap(tc->store, connp->t1) &&
        !(connp->t1 == tc->psize - 1 && bothOpen)) {
      used = 0;
      for (i = 0; i < *numCon; i++) {
        if ((i != j) && (conn[i].t1 == index || conn[i].t2 == index))
          used = 1;
      }
      if (!used && istoreConnectNumber(tc->store, index) >= 0) {
        connp->t2 = index;
        connp->gap = 1;
      }
    }
    
    /* Look backward on top */
    index = (tc->psize + connp->t1 - 1) % tc->psize;
    if (connp->t1 == connp->t2 && istorePointIsGap(tc->store, index) && 
        !(index == tc->psize - 1 && bothOpen)) {
      used = 0;
      for (i = 0; i < *numCon; i++) {
        if ((i != j) && (conn[i].t1 == index || conn[i].t2 == index))
          used = 1;
      }
      if (!used && istoreConnectNumber(tc->store, index) >= 0) {
        connp->t1 = index;
        connp->gap = 1;
      }
    }
  }

  /* Now look for skip segments between connectors or at start/end */
  openDir = closedObj ? dirProduct : openDir;
  for (j = 0; j < *numCon; j++) {
    nj = (j + 1) % *numCon;
    if (!j && (bothOpen || !closedObj)) {
      
      /* Test for skip at start of open contours */
      /* First test that top is at known or possible end and blocker is
         before the bottom point */
      if (conn[j].b1 > 0 && conn[j].t1 == 0 && openDir >= 0 || 
          conn[j].t2 == tc->psize - 1 && openDir <= 0) {
        connum1 = istoreConnectNumber(bc->store, conn[j].b1 - 1);
        if (connum1 >= 0 && connum1 != conn[j].connect) {
          conn[j].skipFromStart = 1;
          conn[j].skipIndex = conn[j].b1 - 1;
        }
      }
      
      /* Then test if bottom is at end and top has block in right direction */
      if (conn[j].b1 == 0 && conn[j].t1 > 0 && conn[j].t2 < tc->psize - 1) {
        if (openDir >= 0) {
          connum1 = istoreConnectNumber(tc->store, conn[j].t1 - 1);
          if (connum1 >= 0 && connum1 != conn[j].connect) {
            conn[j].skipFromStart = 1;
            conn[j].skipIndex = conn[j].t1 - 1;
          }
        }
        if (!conn[j].skipFromStart && openDir <= 0) {
          connum1 = istoreConnectNumber(tc->store, conn[j].t2 + 1);
          if (connum1 >= 0 && connum1 != conn[j].connect) {
            conn[j].skipFromStart = 1;
            conn[j].skipIndex = conn[j].t2 + 1;
          }
        }
      } 
    }

    if (j < *numCon - 1 || (closedObj && !bothOpen)) {

      /* Test for skip between two successive contours */
      /* First look for pair of blockers on bottom */
      index = (conn[j].b2 + 1) % bc->psize;
      connum1 = istoreConnectNumber(bc->store, index);
      index = (bc->psize + conn[nj].b1 - 1) % bc->psize;
      connum2 = istoreConnectNumber(bc->store, index);
      if (connum1 >= 0 && connum2 >= 0 && 
          connum1 != conn[j].connect && connum1 != conn[nj].connect &&
          connum2 != conn[j].connect && connum2 != conn[nj].connect)
        conn[j].skipToNext = 1;
      
      /* Look for pair on top, trickier because of direction dependence */
      if (!conn[j].skipToNext) {
        if (dirProduct > 0)
          index = (conn[j].t2 + 1) % tc->psize;
        else
          index = (tc->psize + conn[j].t1 - 1) % tc->psize;
        connum1 = istoreConnectNumber(tc->store, index);
        if (dirProduct < 0)
          index = (conn[nj].t2 + 1) % tc->psize;
        else
          index = (tc->psize + conn[nj].t1 - 1) % tc->psize;
        connum2 = istoreConnectNumber(tc->store, index);
        if (connum1 >= 0 && connum2 >= 0 && 
            connum1 != conn[j].connect && connum1 != conn[nj].connect &&
            connum2 != conn[j].connect && connum2 != conn[nj].connect)
          conn[j].skipToNext = 1;
      }

    } else if (j || !conn[j].skipFromStart) {

      /* Test for skip to end of open contours */
      /* First test that top is at known or possible end and blocker is
         after the bottom point */
      if (conn[j].b2 < bc->psize - 1 && conn[j].t1 == 0 && openDir <= 0 || 
          conn[j].t2 == tc->psize - 1 && openDir >= 0) {
        connum1 = istoreConnectNumber(bc->store, conn[j].b2 + 1);
        if (connum1 >= 0 && connum1 != conn[j].connect) {
          conn[j].skipToEnd = 1;
          conn[j].skipIndex = conn[j].b2 + 1;
        }
      }
      
      /* Then test if bottom is at end and top has block in right direction */
      if (conn[j].b2 == bc->psize - 1 && conn[j].t1 > 0 && 
          conn[j].t2 < tc->psize - 1) {
        if (openDir >= 0) {
          connum1 = istoreConnectNumber(tc->store, conn[j].t2 + 1);
          if (connum1 >= 0 && connum1 != conn[j].connect) {
            conn[j].skipToEnd = 1;
            conn[j].skipIndex = conn[j].t2 + 1;
          }
        }
        if (!conn[j].skipToEnd && openDir <= 0) {
          connum1 = istoreConnectNumber(tc->store, conn[j].t1 - 1);
          if (connum1 >= 0 && connum1 != conn[j].connect) {
            conn[j].skipToEnd = 1;
            conn[j].skipIndex = conn[j].t1 - 1;
          }
        }
      } 
    }

  }  
  /*for (i = 0; i < *numCon; i++)
    printf("%d %d %d %d %d %d %d %d %d %d\n", conn[i].connect, conn[i].b1, 
    conn[i].b2, conn[i].t1, conn[i].t2, conn[i].gap, 
    conn[i].skipFromStart,
    conn[i].skipToNext, conn[i].skipToEnd, conn[i].skipIndex);
    fflush(stdout); */
  return conn;
}

/* If bottom direction is negative, this reverses the order of the connectors
   in the array and exchanges b1 and b2.  If top direction is negative, it
   exchanges t1 and t2 */
static void invertConnectors(Connector *connects, int numCon, int direction[2])
{
  int i, tmp, zeroSkip;
  Connector connTmp;
  if (direction[0] < 0) {

    /* Swap connectors for bottom inversion */
    for (i = 0; i < numCon / 2; i++) {
      connTmp = connects[i];
      connects[i] = connects[numCon - 1 - i];
      connects[numCon - 1 - i] = connTmp;
    }

    /* Fix up skips at start and end */
    tmp = connects[0].skipToEnd;
    if (connects[numCon - 1].skipFromStart) {
      connects[numCon - 1].skipToEnd = 1;
      connects[numCon - 1].skipFromStart = 0;
    }
    if (tmp) {
      connects[0].skipFromStart = 1;
      connects[0].skipToEnd = 0;
    } 

    /* Swap b1 and b2, and fix skip to next */
    zeroSkip = connects[0].skipToNext;
    connects[0].skipToNext = 0;
    for (i = 0; i < numCon; i++) {
      tmp = connects[i].b1;
      connects[i].b1 = connects[i].b2;
      connects[i].b2 = tmp;
      tmp = (i + 1) % numCon;
      if (tmp) {
        if (connects[tmp].skipToNext) {
          connects[tmp].skipToNext = 0;
          connects[i].skipToNext = 1;
        }
      } else
        connects[i].skipToNext = zeroSkip;
    }
  }

  /* Top inversion: switch t1 and t2 */
  if (direction[1] < 0) {
    for (i = 0; i < numCon; i++) {
      tmp = connects[i].t1;
      connects[i].t1 = connects[i].t2;
      connects[i].t2 = tmp;
    }
  }
  /* for (i = 0; i < numCon; i++)
    printf("ic %d %d %d %d %d %d %d %d %d %d\n", connects[i].connect,
    connects[i].b1, connects[i].b2, connects[i].t1, connects[i].t2,
    connects[i].gap, connects[i].skipFromStart,
    connects[i].skipToNext, connects[i].skipToEnd, connects[i].skipIndex);
    fflush(stdout);  */
}

/* Tests for whether a triangle is entirely outside the limit */
static int outsideMeshLimits(Ipoint *p1, Ipoint *p2, Ipoint *p3)
{
  if ((p1->x < meshMin.x || p1->x > meshMax.x || 
       p1->y < meshMin.y || p1->y > meshMax.y) &&
      (p2->x < meshMin.x || p2->x > meshMax.x || 
       p2->y < meshMin.y || p2->y > meshMax.y) &&
      (p3->x < meshMin.x || p3->x > meshMax.x || 
       p3->y < meshMin.y || p3->y > meshMax.y))
    return 1;
  return 0;
}

/* Adds one triangle to the mesh, exchanging second and third points if inside
   is set */
static void chunkAddTriangle(Imesh *mesh, int i1, int i2, int i3, int *maxsize,
                             int inside)
{
  int o2 = i2;
  int o3 = i3;
  if (inside) {
    o2 = i3;
    o3 = i2;
  }

  if (!mesh->lsize)
    chunkMeshAddIndex(mesh, IMOD_MESH_BGNPOLY, maxsize);
  chunkMeshAddIndex(mesh, i1, maxsize);
  chunkMeshAddIndex(mesh, o2, maxsize);
  chunkMeshAddIndex(mesh, o3, maxsize);
}

#define CHUNKSIZE 8192
/*!
 * Adds [index] to the index list of [mesh], allocating new memory in large 
 * chunks to avoid expensive frequent reallocations.  [maxlist] specifies the 
 * current size of the allocated list and is returned with a new size when it
 * becomes larger.  Returns NULL for error
 */
int chunkMeshAddIndex(Imesh *mesh, int index, int *maxlist)
{
  int *tmp;
     
  if (mesh->lsize >= *maxlist) {
    if ((mesh->lsize > 0) && (mesh->list)) {
      *maxlist += CHUNKSIZE;
      tmp = (int *)realloc(mesh->list, *maxlist * sizeof(int));
    } else {
      *maxlist = CHUNKSIZE;
      tmp = (int *)malloc(*maxlist * sizeof(int));
      mesh->lsize = 0;
    }
    if (tmp == NULL)
      return(-1);
     
    mesh->list = tmp;
  }
  mesh->list[mesh->lsize] = index;
  mesh->lsize++;
  return(0);
}

/* make a cap mesh connecting a contour to a point in the given direction */
Imesh *makeCapMesh(Icont *cont, Ipoint *cm, int meshdir, 
                          DrawProps *props, int state, int stateTest)
{
  int pt, npt;
  Imesh *m = imodMeshNew();
  if (!m) 
    return(m);

  imodMeshAddVert(m, cm);
  
  for (pt = 0; pt < cont->psize; pt++) {
    npt = (pt + 1) % cont->psize;
    imodMeshAddVert(m, &cont->pts[pt]);
    if (outsideMeshLimits(cm,  &cont->pts[pt],  &cont->pts[npt]))
      continue;
    if (state & stateTest) {
      istoreGenerateItems(&m->store, props, state, m->lsize + 1, stateTest);
      istoreGenerateItems(&m->store, props, state, m->lsize + 2, stateTest);
      istoreGenerateItems(&m->store, props, state, m->lsize + 3, stateTest);
    }
    imodMeshAddIndex(m, IMOD_MESH_BGNPOLY);
    imodMeshAddIndex(m, meshdir ? 0 : pt + 1);
    imodMeshAddIndex(m, npt+1);
    imodMeshAddIndex(m, meshdir ? pt + 1 : 0);
    imodMeshAddIndex(m, IMOD_MESH_ENDPOLY);
  }

  imodMeshAddIndex(m, IMOD_MESH_END);
  return(m);
}

/* make a contour for tube mesh. */
int makeTubeCont(Icont *cont, Ipoint *loc, Ipoint *n, Ipoint *scale,
               float tubeDiameter, int slices)
{
  Ipoint spt, tpt, cpt;
  Imat *mat = imodMatNew(3);
  Imat *rmat = imodMatNew(3);
  double astep = 360.0 / (double)slices;
  double a = 0.0, b = 0.0;
  int sl;

  Ipoint rscale;

  /*     printf("mkcont: norm = %g %g %g\n",
         n->x, n->y, n->z);
  */

  rscale.x = 1.0f/scale->x;
  rscale.y = 1.0f/scale->y;
  rscale.z = 1.0f/scale->z;


  b = acos((double)n->z);
  b *= 57.29578;

  /* DNM: modify method of getting rotation matrix to end up in correct
     quadrant, using atan2 */

  imodMatRot(mat, b, b3dY);
  a = atan2((double)n->y, (double)n->x);
  a *= 57.29578;
  imodMatRot(mat, a, b3dZ);


  spt.x = 0.0f;
  spt.y = tubeDiameter * 0.5f;
  spt.z = 0.0f;

  imodMatScale(mat, &rscale);  /* DNM: Move this outside the loop */
  for (sl = 0; sl < slices; sl++) {
    imodMatRot(rmat, astep, b3dZ);
    imodMatTransform(rmat, &spt, &tpt);
    imodMatTransform(mat, &tpt, &cpt);

    cpt.x += loc->x;
    cpt.y += loc->y;
    cpt.z += loc->z;
    imodPointAppend(cont, &cpt);
  }
  imodMatDelete(mat);
  imodMatDelete(rmat);
  return(0);
}

/* Find point at which back-transformed circle reaches its top, and return 0
   for ccw or 1 for cw circle */

static int circle_top_and_direction(Icont *cont, Imat *mat, int *ptop)
{
  int pt, pt2;
  int reverse = 0;
  float ytop, xtop;
  Ipoint tpt;

  /* Find pt at which Y reaches a maximum */
  for (pt = 0; pt <= cont->psize; pt++) {
    pt2 = pt;
    if (pt2 == cont->psize)
      pt2 = 0;
    imodMatTransform(mat, &cont->pts[pt2], &tpt);
    if (pt == 0 || tpt.y > ytop) {
      ytop = tpt.y;
      xtop = tpt.x;
      *ptop = pt2;
    }
    /* if the LAST point was a maximum, determine whether there's
       a reversal based on change in X */
    if (pt == *ptop + 1)
      reverse = tpt.x > xtop ? 1 : 0;
  }
  return reverse;
}
               
/* join two tube contours together. Return NUll for error. */
Imesh *joinTubeCont(Icont *c1, Icont *c2, Ipoint *norm, 
                    DrawProps *props1, int state1, DrawProps *props2,
                    int state2)
{
  int pt, mpt, npt, pt2, mpt2, pt1, idir2, k, last1, next1, maxpt, last2,next2;
  Imat *mat = imodMatNew(3);
  Imesh *mesh = imodMeshNew();
  double a, b;
  int reverse1 = 0, reverse2 = 0;
  int stateTest = CHANGED_COLOR | CHANGED_FCOLOR | CHANGED_TRANS | 
    CHANGED_VALUE1;
  int genItems = ((state1 | state2) & stateTest) ? 1 : 0;
  if (!mesh || !mat)
    return(NULL);

  mpt = c1->psize;
  mpt2 = c2->psize;
  maxpt = B3DMAX(mpt, mpt2);

  /* Get matrix that rotates this central normal to Z axis */
  b = acos((double)norm->z);
  b *= 57.29578;
  a = atan2((double)norm->y, (double)norm->x);
  a *= 57.29578;
  imodMatRot(mat, -a, b3dZ);
  imodMatRot(mat, -b, b3dY);

  /* Start at top point of each back-transformed circle */

  reverse1 = circle_top_and_direction(c1, mat, &pt1);
  reverse2 = circle_top_and_direction(c2, mat, &pt2);

  /* and go backwards for second contour if reverse flag doesn't match */

  idir2 = (reverse1 == reverse2) ? 1 : -1;

  /* If either trans change flag is set, set both and make sure trans state
     is the same (both 0 or both non-zero) */
  if ((state1 | state2) & CHANGED_TRANS) {
    if (props1->trans && !props2->trans)
      props2->trans = 1;
    if (props2->trans && !props1->trans)
      props1->trans = 1;
    state1 |= CHANGED_TRANS;
    state2 |= CHANGED_TRANS;
  }

  mesh->vsize = mpt + mpt2;
  mesh->lsize = 3 * (mpt + mpt2 + 1);
  mesh->vert = (Ipoint *)malloc(mesh->vsize * sizeof(Ipoint));
  mesh->list = (int *)malloc(mesh->lsize * sizeof(int));
  if (!mesh->vert || !mesh->list) {
    if (mesh->vert)
      free(mesh->vert);
    if (mesh->list)
      free(mesh->list);
    free(mesh);
    imodMatDelete(mat);
    return(NULL);
  }

  /* Load the vertices first, in the right order for each circle */
  for (pt = 0 ; pt < mpt; pt++) {
    mesh->vert[pt] = c1->pts[pt1];
    pt1++;
    if (pt1 == mpt)
      pt1 = 0;
  }  

  for (; pt < mpt + mpt2; pt++) {
    mesh->vert[pt] = c2->pts[pt2];
    pt2 += idir2;
    if (pt2 == mpt2)
      pt2 = 0;
    if (pt2 < 0)
      pt2 = mpt2 - 1;
  }

  last1 = 0;
  last2 = 0;
  k = 0;
  mesh->list[k++] = IMOD_MESH_BGNPOLY;
  for (pt = 0 ; pt < maxpt; pt++) {

    /* Get next point in c1, and the point it matches in c2 */
    npt = (pt+1) % maxpt;
    next1 = ((int)floor(((double)mpt * npt )/ maxpt + 0.5)) % mpt;
    next2 = ((int)floor(((double)mpt2 * npt )/ maxpt + 0.5)) % mpt2;

    /* add triangle with base in c1, and triangle with base in c2 only if the
       matches are different */
    if (last1 != next1) {
      mesh->list[k++] = mpt + last2;
      mesh->list[k++] = last1;
      mesh->list[k++] = next1;
      if (genItems) {
        istoreGenerateItems(&mesh->store, props2, state2, k - 3, stateTest);
        istoreGenerateItems(&mesh->store, props1, state1, k - 2, stateTest);
        istoreGenerateItems(&mesh->store, props1, state1, k - 1, stateTest);
      }
    }

    if (next2 != last2) {
      mesh->list[k++] = mpt + last2;
      mesh->list[k++] = next1;
      mesh->list[k++] = mpt + next2;
      if (genItems) {
        istoreGenerateItems(&mesh->store, props2, state2, k - 3, stateTest);
        istoreGenerateItems(&mesh->store, props1, state1, k - 2, stateTest);
        istoreGenerateItems(&mesh->store, props2, state2, k - 1, stateTest);
      }
    }

    last1 = next1;
    last2 = next2;
  }
  mesh->list[k++] = IMOD_MESH_ENDPOLY;
  mesh->list[k++] = IMOD_MESH_END;

  imodMatDelete(mat);
  /* istoreDump(mesh->store); */
  return(mesh);
}



/*
$Log$
Revision 1.6  2009/03/10 02:52:54  mast
Analyze for blocking connectors to skip regions of mesh

Revision 1.5  2008/12/02 21:20:07  mast
Store value in tube meshes

Revision 1.4  2008/11/15 21:53:08  mast
Invert order of open contours to maintain consistency from one to the next

Revision 1.3  2008/09/19 15:28:47  mast
Fixed tube contour joining to work for different size contours

Revision 1.2  2006/11/02 07:16:21  mast
Documentation

Revision 1.1  2006/09/12 14:58:19  mast
Split up and made into new library

Revision 3.20  2006/09/01 15:20:41  mast
Encoded stored values in mesh as well

Revision 3.19  2006/05/08 16:53:35  mast
Fixed forcing option to connect nearest pairs of contours, and added
ability to use connection numbers to force or prevent contour connections

Revision 3.18  2006/01/11 05:53:50  mast
Fixed test for gap at end of contour

Revision 3.17  2005/09/22 15:12:46  mast
Handled surface property changes for all caps, transferred contour/surface
properties to first point of contour when joining contours

Revision 3.16  2005/09/12 14:27:07  mast
Made it incorporate surface fine-grained info if run w/o -S option

Revision 3.15  2005/09/11 19:28:01  mast
Incorporated fine-grained state into mesh, added clipping of triangle output,
implemented new vertex-normal index list without redundant vertices

Revision 3.14  2005/05/26 20:11:24  mast
Fixed properly for reaching end of range with -P for open contours

Revision 3.13  2005/05/22 05:26:50  mast
Implemented multiple passes for open contour objects

Revision 3.12  2005/04/04 22:41:33  mast
Fixed problem with argument order to imdContourGetBBox

Revision 3.11  2005/03/20 19:56:05  mast
Eliminating duplicate functions

Revision 3.10  2005/01/30 17:45:44  mast
Changed calls to overlap_fractions to send address of contour pointers

Revision 3.9  2005/01/29 20:28:32  mast
Pulled out routines for making Z tables and doing nested contour
analysis

Revision 3.8  2004/09/27 22:14:52  mast
Try again - get rid of empty polygon at start of mesh

Revision 3.7  2004/09/27 21:58:33  mast
Prevented polygons from being combined on second run of imeshReMeshNormal

Revision 3.6  2004/09/10 21:34:01  mast
Eliminated long variables

Revision 3.5.6.1  2004/09/10 19:27:52  mast
Eliminate long variables

Revision 3.5  2003/08/26 03:47:57  mast
Added ability to cap ends of tubes, made a function to add a cap mesh, made
the number of segments in a tube mesh be variable, and
renamed jcont and mkcont to indicate they are specific to tubes.

Revision 3.4  2003/02/27 16:57:57  mast
change to b3dX,Y,Z

Revision 3.3  2003/02/07 02:36:59  mast
Compute zmin correctly so it will work with negative z's; add some tests
for empty contours in early setup of contour lists

Revision 3.2  2002/04/09 19:07:15  mast
Fixed a bug in scan_points_to_segments found by Lambert Zipj.

Revision 3.1  2002/01/13 17:37:02  mast
Made it work with inside-out donuts (annulus to horseshoe transitions)

*/
