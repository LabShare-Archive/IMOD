/*  surfacesort.c - function for sorting points onto two surfaces
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2010 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define setsurfsortparam SETSURFSORTPARAM
#define surfacesort SURFACESORT
#else
#define setsurfsortparam setsurfsortparam_
#define surfacesort surfacesort_
#endif
#define DTOR 0.017453293


/* DOC_SECTION PARAMETERS */
/* DOC_CODE Algorithm and Parameters for surfaceSort */
/*
  This routine works as follows: ^
  First it fits a plane to all of the points and rotates the points to make that plane
  level.  Analysis proceeds with rotated positions. ^
  For rapid access to neighboring points, it sets up a grid of squares (at sGridSpacing)
  and list of delta values to grid positions in successively wider rings around one
  grid square.  Then it makes lists of points within each grid square. ^
  For each point (not marked as an outlier by values of -1 in the group array) it then 
  searches for the neighboring point at the steepest angle with respect to it, looking 
  at up to sMaxAngleNeigh neighbors. ^
  Duplicate pairs are allowed to form, and are eliminated after sorting. ^
  The points are sorted by steepest angle, and then it determines a set of pairs to use 
  for building clusters, as controlled by parameters sSteepestRatio, sNumMinForAmax, 
  sAngleMax, sNumMinForArelax, and sAngleRelax. ^
  Given this set of pairs, it then makes a list of the separation in Z between each pair 
  and analyzes for MAD-Median outliers with criterion sOutlierCrit.  These outliers are 
  eliminated from cluster formation, as are the ones identified by the caller.
  Pairs are always considered in order from the steepest angle downward.  To build a
  cluster, it starts with the next point that hasn't been added to a cluster yet, putting
  it and its pair on a list of cluster points to check.  Each point on the cluster list
  is checked for whether it occurs in a pair at a steep angle, and if so its mate is 
  then added to the opposite group. ^
  For each cluster, a central position and an average delta Z are computed. ^
  Finally, it searches in progressively bigger rings around the clusters for grid
  squares where there are unassigned points.  When it finds an unassigned point, it
  collects the nearest assigned points in the neighborhood up to sMaxNumFit or up to
  a distance of sMaxFitDist.  If this includes points from both surfaces and there are
  at least sBiplaneMinFit points, it fits a pair of planes to all points; otherwise it
  fits a single plane to just the points from one surface if there are at least 
  sPlaneMinFit points for this; otherwise it simply gets a mean of Z positions on the
  top and bottom if possible.  One way or another this gives an estimate of the top and 
  bottom Z position at the unassigned point, and it assigns the point based on which Z 
  it is closer to.  In fact, this process is done in two rounds, and on the first round
  assignment is deferred if the disparity between the point and estimated surface Z 
  values as a percentage of the Z separation exceeds sMaxRound1Dist. ^
  ^
  The numbers in the comments below are index values for setSurfSortParam */

/* Spacing of squares for sorting points and accessing rings of points */
static float sGridSpacing = 50.; /* 0 */
/* Maximum neighbors to evaluate for finding neighbor with steepest angle */
static int sMaxAngleNeigh = 50;  /* 1 */
/* Use pairs with angle more than this fraction of very steepest angle */
static float sSteepestRatio = 0.5f; /* 2 */
/* If there are fewer than this number of pairs, take pairs down to sAngleMax */
static int sNumMinForAmax = 10;  /* 3 */
/* Angle to go down to if sSteepestRatio doesn't give enough pairs */
static float sAngleMax = 20.;    /* 4 */
/* If fewer than this number of pairs, take pairs down to sAngleRelax */
static int sNumMinForArelax = 3; /* 5 */
/* Angle to go down to if sAngleMax doesn't give enough pairs */
static float sAngleRelax = 5.;   /* 6 */
/* Criterion for MAD-Median outlier elimination based on delta Z of a pair */
static float sOutlierCrit = 3.;  /* 7 */
/* Maximum distance to search for neighboring points in plane fits */
static float sMaxFitDist = 2048.; /* 8 */
/* Maximum number of points in plane fits */
static int sMaxNumFit = 15;      /* 9 */
/* Minimum # of points for fitting parallel planes */
static int sBiplaneMinFit = 5;   /* 10 */
/* Minimum # of points for fitting one plane */
static int sPlaneMinFit = 4;     /* 11 */
/* Maximum % distance of Z value between nearest and other plane for deferring to 2nd
   round: values > 50 disable deferring points */
static float sMaxRound1Dist = 100.; /* 12 */
/* 1 for minimal output, 2 for exhaustive output */
static int sDebugLevel = 0;      /* 13 */
/* END_CODE */
/* END_SECTION */

/*!
 * Sets one parameter for @surfaceSort .  
 */
int setSurfSortParam(int index, float value)
{
  switch (index) {
  case 0: sGridSpacing = value; break;
  case 1: sMaxAngleNeigh = B3DNINT(value); break;
  case 2: sSteepestRatio = value; break;
  case 3: sNumMinForAmax = B3DNINT(value); break;
  case 4: sAngleMax = value; break;
  case 5: sNumMinForArelax = B3DNINT(value); break;
  case 6: sAngleRelax = value; break;
  case 7: sOutlierCrit = value; break;
  case 8: sMaxFitDist = value; break;
  case 9: sMaxNumFit = B3DNINT(value); break;
  case 10: sBiplaneMinFit = B3DNINT(value); break;
  case 11: sPlaneMinFit = B3DNINT(value); break;
  case 12: sMaxRound1Dist = value; break;
  case 13: sDebugLevel = B3DNINT(value); break;
  default:
    return 1;
  }
  return 0;
}

/*!
 * Fortran wrapper for @setSurfSortParam
 */
int setsurfsortparam(int *index, float *value)
{
  return setSurfSortParam(*index, *value);
}

/*!
 * Sorts a set of points in 3D onto two surfaces.  The X, Y, and Z coordinates 
 * of the [numPts] points are packed sequentially into the array [xyz].  If 
 * [markersInGroup] is nonzero, then the [group] array should be passed in with 0 for 
 * points eligible to be included in the pairs of points that are used to initiate a 
 * cluster, or with a -1 for points that should not be used to initiate clusters.  The
 * surface numbers, 1 for the bottom one and 2 for the top one, are returned in
 * [group].  The return value is 1 for a memory allocation error.
 */
int surfaceSort(float *xyz, int numPts, int markersInGroup, int *group)
{
  int i, numGridX, numGridY, numSquares, numRings, ind, ring, dx, dy, sx, sy;
  float afit, bfit, cfit, zp, xmin, xmax, ymin, ymax, diagonal, pdx, pdy;
  double alpha, cosal, sinal, theta, sinth, costh, slope;
  float *xrot, *yrot, *zrot;
  short int *idx, *idy;
  int *ringStart, *numInSquare, *squareInd, *pointLists, *steepNeigh, *sortInd;
  unsigned char *squareDone;
  float *steepAngle, *xfit, *yfit, *zfit, *grpfit, *delZ, *outlie;
  int *cluster, *clusterSX, *clusterSY;
  int isq, ipt, ix, iy, jnd, jsq, jpt, numNeigh, firstSteep, numSteep, ifdup;
  float angle, verySteepest, zbot, ztop, a1, a2, dzfit, con, medianDelZ;
  int j, numDone, jdxy, kdxy, jring, tx, ty, maxRings, grpsum, keepGroup;
  int ntop, nbot, nfit, jx, jy, knd, kpt, numCluster, numInClust, checkInd;
  int oldpt, newpt, numErr, round;
  float xsum, ysum, errsum, errsq, errmax;


  if (numPts < 3) {
    group[0] = 1;
    if (numPts > 1) 
      group[1] = 2;
    return 0;
  }

  /* Copy to rot arrays and fit a plane to all of the points */
  xrot = B3DMALLOC(float, numPts);
  yrot = B3DMALLOC(float, numPts);
  zrot = B3DMALLOC(float, numPts);
  if (!xrot || !yrot || !zrot)
    return 1;
  for (i = 0; i < numPts; i++) {
    xrot[i] = xyz[3 * i];
    yrot[i] = xyz[3 * i + 1];
    zrot[i] = xyz[3 * i + 2];
  }
  lsFit2(xrot, yrot, zrot, numPts, &afit, &bfit, &cfit);

  /* Find rotation angles and cosine and sines */
  alpha = atan((double)bfit);
  cosal = cos(alpha);
  sinal = sin(alpha);
  slope = afit / (cosal - bfit * sinal);
  theta = -atan(slope);
  costh = cos(theta);
  sinth = sin(theta);

  /* Back-rotate by -alpha around X then -theta around Y */
  xmin = ymin = 1.e30;
  xmax = ymax = -1.e30;
  for (i = 0; i < numPts; i++) {
    yrot[i] = xyz[3*i+1] * cosal + xyz[3*i+2] * sinal;
    zp = - xyz[3*i+1] * sinal + xyz[3*i+2] * cosal ;
    xrot[i] = xyz[3*i] * costh - zp * sinth;
    zrot[i] = xyz[3*i] * sinth + zp * costh;
    xmin = B3DMIN(xmin, xrot[i]);
    xmax = B3DMAX(xmax, xrot[i]);
    ymin = B3DMIN(ymin, yrot[i]);
    ymax = B3DMAX(ymax, yrot[i]);
  }

  /* Set up grid and get arrays */
  numGridX = (int)((xmax - xmin) / sGridSpacing + 1.);
  numGridY = (int)((ymax - ymin) / sGridSpacing + 1.);
  numSquares = numGridX * numGridY;
  diagonal = sqrt((numGridX-1.) * (numGridX-1.) + (numGridY-1.)*(numGridY-1.));
  numRings = B3DNINT(diagonal) + 1;
  idx = B3DMALLOC(short int, 4 * numSquares);
  idy = B3DMALLOC(short int, 4 * numSquares);
  ringStart = B3DMALLOC(int, (numRings + 2));
  numInSquare = B3DMALLOC(int, numSquares);
  squareInd = B3DMALLOC(int, numSquares);
  pointLists = B3DMALLOC(int, numPts);
  squareDone = B3DMALLOC(unsigned char, numSquares);
  steepAngle = B3DMALLOC(float, numPts);
  steepNeigh = B3DMALLOC(int, numPts);
  sortInd = B3DMALLOC(int, numPts);
  xfit = B3DMALLOC(float, sMaxNumFit);
  yfit = B3DMALLOC(float, sMaxNumFit);
  zfit = B3DMALLOC(float, sMaxNumFit);
  grpfit = B3DMALLOC(float, sMaxNumFit);

  if (!idx || !idy || !numInSquare || !squareInd || !squareDone || !ringStart
      || !pointLists || !steepAngle || !steepNeigh || !sortInd || !xfit || 
      !yfit || !zfit || !grpfit)
    return 1;
  
  /* Set up rings of delta values */
  ind = 0;
  for (ring = 0; ring < numRings; ring++) {
    ringStart[ring] = ind;
    for (dy = -(numGridY - 1); dy < numGridY; dy++) {
      for (dx = -(numGridX - 1); dx < numGridX; dx++) {
        if (B3DNINT(sqrt((double)(dx * dx + dy * dy))) == ring) {
          idx[ind] = dx;
          idy[ind++] = dy;
        }
      }
    }
  }
  ringStart[numRings] = ind;

  /* Sort the points into grid - first count how many in each square so
     indexes can be set up, then make the indexes, then put points into index
     list */
  for (i = 0; i < numSquares; i++)
    numInSquare[i] = 0;
  for (i = 0; i < numPts; i++) {
    sx = (int)((xrot[i] - xmin) / sGridSpacing);
    sy = (int)((yrot[i] - ymin) / sGridSpacing);
    numInSquare[sx + sy * numGridX]++;
  }
  ind = 0;
  for (i = 0; i < numSquares; i++) {
    squareInd[i] = ind;
    ind += numInSquare[i];
    numInSquare[i] = 0;
    squareDone[i] = 0;
  }
  for (i = 0; i < numPts; i++) {
    sx = (int)((xrot[i] - xmin) / sGridSpacing);
    sy = (int)((yrot[i] - ymin) / sGridSpacing);
    ind = sx + sy * numGridX;
    pointLists[squareInd[ind] + numInSquare[ind]] = i;
    numInSquare[ind]++;
  }

  /* For each point, find the neighbor with the steepest angle 
   Loop on the squares; loop on each point in the square 
   For each point, loop on sequence of neighboring squares and on points in
   each square until reach maximum number of neighors */
  for (ind = 0; ind < numSquares; ind++) {
    sx = ind % numGridX;
    sy = ind / numGridX;
    for (isq = 0; isq < numInSquare[ind]; isq++) {
      ipt = pointLists[squareInd[ind] + isq];
      steepAngle[ipt] = -1.;
      steepNeigh[ipt] = -1;
      sortInd[ipt] = ipt;
      if (markersInGroup && group[ipt] < 0) {
        if (sDebugLevel > 1) 
          printf("Skipping search for %d\n", ipt);
        continue;
      }
      numNeigh = 0;
      for (jdxy = 0; jdxy < ringStart[numRings] && numNeigh < sMaxAngleNeigh;
           jdxy++) {
        ix = sx + idx[jdxy];
        iy = sy + idy[jdxy]; 
        if (ix < 0 || ix >= numGridX || iy < 0 || iy >= numGridY)
          continue;
        jnd = ix + iy * numGridX;
        for (jsq = 0; jsq < numInSquare[jnd] && numNeigh < sMaxAngleNeigh;
             jsq++) {
          jpt = pointLists[squareInd[jnd] + jsq];
          if (ipt == jpt || (markersInGroup && group[jpt] < 0))
            continue;
          pdx = xrot[ipt] - xrot[jpt];
          pdy = yrot[ipt] - yrot[jpt];
          angle = atan2(fabs((double)(zrot[ipt] - zrot[jpt])), 
                        sqrt(pdx * pdx + pdy * pdy)) / DTOR;
          if (angle > steepAngle[ipt]) {
            steepAngle[ipt] = angle;
            steepNeigh[ipt] = jpt;
          }
          numNeigh++;
        }
      }
    }
  }

  /* Zero group values now that markers have been used, if any */
  for (i = 0; i < numPts; i++)
    group[i] = 0;

  /* Sort the points by steepest angle */
  rsSortIndexedFloats(steepAngle, sortInd, numPts);
  if (sDebugLevel > 1) {
    for (i = 0; i < numPts; i++) {
      ipt = sortInd[i];
      printf("pt %d  neigh %d  angle %f\n", ipt, steepNeigh[ipt], 
      steepAngle[ipt]);
    }
  }
  verySteepest = steepAngle[sortInd[numPts-1]];

  /* Find pairs that are sufficiently steep */
  firstSteep = numPts - 1;
  numSteep = 1;
  for (ind = numPts-2; ind >= 0; ind--) {
    angle = steepAngle[sortInd[ind]];

    /* Termination conditions: */
    if ((angle < sSteepestRatio * verySteepest && numSteep >= sNumMinForAmax) ||
        (angle < sAngleMax && numSteep >= sNumMinForArelax) ||
        (angle < sAngleRelax))
      break;

    /* Check for duplicate pair */
    ifdup = 0;
    for (i = ind + 1; i < numPts; i++) {
      if (steepAngle[sortInd[i]] > angle + 1.e-5)
        break;
      if (sortInd[i] == steepNeigh[sortInd[ind]] &&
          sortInd[ind] == steepNeigh[sortInd[i]]) {
        ifdup = 1;
        if (sDebugLevel > 1) 
          printf("Duplicate  %d  %d  %f\n", sortInd[ind], sortInd[i],
                 steepAngle[sortInd[ind]]);
        steepAngle[sortInd[ind]] = -1.;
        break;
      }
    }
    if (ifdup)
      continue;

    /* Add this point as start of steep ones to use */
    numSteep++;
    firstSteep = ind;
  }

  /* Collect the delta Z values to get median and outlier evaluation */
  delZ = B3DMALLOC(float, (numPts - firstSteep));
  outlie = B3DMALLOC(float, (numPts - firstSteep));
  cluster = B3DMALLOC(int, (numPts - firstSteep));
  clusterSX = B3DMALLOC(int, (numPts - firstSteep));
  clusterSY = B3DMALLOC(int, (numPts - firstSteep));
  if (!delZ || !outlie || !cluster || !clusterSX || !clusterSY)
    return 1;
  i = 0;
  for (ind = firstSteep; ind < numPts; ind++)
    if (steepAngle[sortInd[ind]] > 0)
      delZ[i++] = fabs(zrot[sortInd[ind]] - zrot[steepNeigh[sortInd[ind]]]);
  rsMedian(delZ, numSteep, outlie, &medianDelZ);
  if (sDebugLevel) 
    printf("Steep pairs: n = %d  median delz = %f\n", i, medianDelZ);
  if (numSteep > 2) {

    /* If more than 2 points, identify outliers with criterion, and then
       eliminate them too */
    rsMadMedianOutliers(delZ, numSteep, sOutlierCrit, outlie);
    i = 0;
    for (ind = firstSteep; ind < numPts; ind++) {
      if (steepAngle[sortInd[ind]] > 0) {
        if (outlie[i] != 0.) {
          steepAngle[sortInd[ind]] = -1.;
          numSteep--;
        }
        i++;
      }
    }
  }
   
  /* Build clusters from the top of the list down */
  numCluster = 0;
  numDone = 0;
  for (;;) {
    
    /* Look for first remaining pair */
    for (ind = numPts - 1; ind >= firstSteep; ind--)
      if (steepAngle[sortInd[ind]] > 0)
        break;
    if (ind < firstSteep)
      break;

    /* Start a cluster and list of points to check */
    numInClust = 2;
    ipt = sortInd[ind];
    jpt = steepNeigh[ipt];
    cluster[0] = ipt;
    cluster[1] = jpt;
    group[ipt] = (zrot[ipt] < zrot[jpt]) ? 1 : 2;
    group[jpt] = 3 - group[ipt];
    checkInd = 0;
    steepAngle[ipt] = -1.;
    while (checkInd < numInClust) {

      /* To check a point in cluster list, loop on the rest of the best pairs looking
         for one that includes this point */
      for (jnd = ind - 1; jnd >= firstSteep; jnd--) {
        ipt = sortInd[jnd];
        jpt = steepNeigh[ipt];
        if (steepAngle[ipt] > 0 && 
            (ipt == cluster[checkInd] || jpt == cluster[checkInd])) {
          
          /* oldpt is index of one already in cluster, newpt is its pair */
          newpt = ipt;
          oldpt = jpt;
          if (ipt == cluster[checkInd]) {
            oldpt = ipt;
            newpt = jpt;
          }
          
          /* Check consistency with existing entry */
          knd = (zrot[oldpt] < zrot[newpt]) ? 1 : 2;
          if (knd != group[oldpt]) {
            printf("INCONSISTENCY IN INITIAL STEEP PAIRS IN SURFACE SORT.\n");
            
          } else {

            /* See if other point is already in cluster; if so check its 
               surface consistency too */
            ifdup = 0;
            knd = (zrot[oldpt] >= zrot[newpt]) ? 1 : 2;
            for (j = 0; j < numInClust; j++) {
              if (newpt == cluster[j]) {
                ifdup = 1;
                if (knd != group[newpt])
                  printf("INCONSISTENCY IN INITIAL STEEP PAIRS IN SURFACE SORT.\n");
                break;
              }
            }

            /* Add new point to cluster and set its angle -1 to mark its pair as used */
            if (!ifdup) {
              cluster[numInClust++] = newpt;
              group[newpt] = knd;
            }
          }
          steepAngle[ipt] = -1.;
        }
      }
      checkInd++;
    }

    /* A cluster is done.  Get its delta Z overall and central square */
    nbot = 0;
    ntop = 0;
    zbot = 0.;
    ztop = 0.;
    xsum = 0.;
    ysum = 0.;
    for (i = 0; i < numInClust; i++) {
      ipt = cluster[i];
      if (group[ipt] == 2) {
        ntop++;
        ztop += zrot[ipt];
      } else {
        nbot++;
        zbot += zrot[ipt];
      }
      xsum += xrot[ipt];
      ysum += yrot[ipt];
    }
        
    delZ[numCluster] = ztop / ntop - zbot / nbot;
    clusterSX[numCluster] = (int)((xsum / numInClust - xmin) / sGridSpacing);
    clusterSY[numCluster++] = (int)((ysum / numInClust - ymin) / sGridSpacing);
    numDone += numInClust;
    if (sDebugLevel) 
      printf("cluster %d  num  %d  delz  %f  sx, sy %d %d, done %d\n",
             numCluster,numInClust, delZ[numCluster-1],
             clusterSX[numCluster-1], clusterSY[numCluster-1],numDone);
    if (sDebugLevel > 1)
      for (i = 0; i < numInClust; i++)
        printf("%d  %.1f  %.1f  %.1f  %d\n",cluster[i], xrot[cluster[i]], 
               yrot[cluster[i]], zrot[cluster[i]], group[cluster[i]]);
  }          
            
  numErr = 0;
  errmax = -1000.;
  errsum = errsq = 0.;

  /* Loop on rings, in each ring loop on clusters */
  for (round = 0; round < 2; round++) {
    for (ring = 0; ring < numRings && numDone < numPts; ring++) {
      for (ind = 0; ind < numCluster && numDone < numPts; ind++) {
        sx = clusterSX[ind];
        sy = clusterSY[ind];

        /* Loop on the squares in the ring and on the points in the squares */
        for (jdxy = ringStart[ring]; jdxy < ringStart[ring+1] &&
               numDone < numPts; jdxy++) {
          ix = sx + idx[jdxy];
          iy = sy + idy[jdxy]; 
          if (ix < 0 || ix >= numGridX || iy < 0 || iy >= numGridY)
            continue;
          jnd = ix + iy * numGridX;
          
          /* If square done, skip; otherwise set to 1 and reset to 0 when 
             find a point that needs doing */
          if (squareDone[jnd])
            continue;
          squareDone[jnd] = 1;
          for (jsq = 0; jsq < numInSquare[jnd] && numDone < numPts; jsq++) {
            jpt = pointLists[squareInd[jnd] + jsq];
            if (!group[jpt]) {
              squareDone[jnd] = 0;

              /* Now do rings around this point to collect identified 
                 neighbors within a certain range up to a certain count */
              maxRings = B3DNINT(sMaxFitDist / sGridSpacing);
              maxRings = B3DMIN(maxRings, numRings);
              nfit = 0;
              grpsum = 0;
              tx = (int)((xrot[jpt] - xmin) / sGridSpacing);
              ty = (int)((yrot[jpt] - ymin) / sGridSpacing);
              for (jring = 0; (jring < maxRings || nfit < 2) && 
                     nfit < sMaxNumFit; jring++) {
                for (kdxy = ringStart[jring]; kdxy < ringStart[jring+1] 
                       && nfit < sMaxNumFit; kdxy++) {
                  jx = tx + idx[kdxy];
                  jy = ty + idy[kdxy];
                  if (jx < 0 || jx >= numGridX || jy < 0 || jy >= numGridY)
                    continue;
                  knd = jx + jy * numGridX;
                  for (isq = 0; isq < numInSquare[knd] && nfit < sMaxNumFit; 
                       isq++) {
                    kpt = pointLists[squareInd[knd] + isq];
                    if (group[kpt]) {
                      xfit[nfit] = xrot[kpt];
                      yfit[nfit] = yrot[kpt];
                      zfit[nfit] = zrot[kpt];
                      grpfit[nfit++] = group[kpt] - 1;
                      grpsum += group[kpt] - 1;
                    }
                  }
                }
              }
              if (sDebugLevel > 1) 
                printf("For %d  %.1f %.1f  %.1f  nfit %d  ntop %d\n", jpt,
                       xrot[jpt], yrot[jpt], zrot[jpt], nfit, grpsum);

              /* Fit a biplane if there are enough points and 2 surfaces */
              if (nfit >= sBiplaneMinFit && grpsum && grpsum != nfit) {
                lsFit3(xfit, yfit, grpfit, zfit, nfit, &a1, &a2, &dzfit, &con);

                /* Get bottom and top predicted values.  Store current DZ as
                   delz for this area if there are at least 2 on each surface*/
                zbot = a1 * xrot[jpt] + a2 * yrot[jpt] + con;
                ztop = zbot + dzfit;
                if (sDebugLevel > 1) 
                  printf("fit3 %f  %f %f %f\n", a1, a2, dzfit, con);
                if (grpsum > 1 && nfit - grpsum > 1)
                  delZ[ind] = dzfit;
              } else {

                /* Need to keep just one group for single plane fit - so find 
                   out how many points this leaves */
                keepGroup = 1;
                if (grpsum <= nfit / 2)
                  keepGroup = 0;
                if ((keepGroup && grpsum >= sPlaneMinFit) ||
                    (!keepGroup && nfit - grpsum >= sPlaneMinFit)) {
                
                  /* If this leaves enough points for a fit, repack the array 
                     with that group */
                  if (grpsum && grpsum != nfit) {
                    j = 0;
                    for (i = 0; i < nfit; i++) {
                      if (grpfit[i] == keepGroup) {
                        xfit[j] = xfit[i];
                        yfit[j] = yfit[i];
                        zfit[j] = zfit[i];
                        grpfit[j++] = grpfit[i];
                      }
                    }
                    nfit = j;
                    grpsum = j * keepGroup;
                  }

                  /* Do fit and get upper and lower Z using the delz for area*/
                  lsFit2(xfit, yfit, zfit, nfit, &a1, &a2, &con);
                  zbot = ztop = a1 * xrot[jpt] + a2 * yrot[jpt] + con;
                  if (keepGroup)
                    zbot -= delZ[ind];
                  else
                    ztop += delZ[ind];
                  if (sDebugLevel > 1) 
                    printf("fit2  %f  %f  %f  zbot %.1f  ztop  %.1f\n",a1,a2,
                           con, zbot,ztop);
                } else {
                
                  /* Otherwise get mean of bottom and top */ 
                  nbot = 0;
                  ntop = 0;
                  zbot = 0.;
                  ztop = 0.;
                  for (i = 0; i < nfit; i++) {
                    if (grpfit[i]) {
                      ntop++;
                      ztop += zfit[i];
                    } else {
                      nbot++;
                      zbot += zfit[i];
                    }
                  }

                  /* Use both means if they exist; otherwise use delta Z and 
                     one mean to get the two Z values */
                  if (nbot)
                    zbot /= nbot;
                  if (ntop)
                    ztop /= ntop;
                  if (nbot && !ntop)
                    ztop = zbot + delZ[ind];
                  if (!nbot && ntop)
                    zbot = ztop - delZ[ind];
                  if (sDebugLevel > 1) 
                    printf("means  %d  %d  zbot %.1f  ztop  %.1f\n",nbot,ntop,
                           zbot,ztop);
                }
              }

              /* At last, assign point to group based on which Z is closest */
              if (fabs((double)(zrot[jpt] - zbot)) < 
                  fabs((double)(zrot[jpt] - ztop))) {
                group[jpt] = 1;
                xsum = 100. * (zrot[jpt] - zbot) / (ztop - zbot);
              } else {
                group[jpt] = 2;
                xsum = 100. * (ztop - zrot[jpt]) / (ztop - zbot);
              }
              if (round || xsum < sMaxRound1Dist) {
                if (sDebugLevel > 1) 
                  printf("Assign to %d   distance %.1f%%\n", group[jpt], xsum);
                numDone++;
                errsum += xsum;
                errmax = B3DMAX(errmax, xsum);
                errsq += xsum * xsum;
                numErr++;
              } else {
                if (sDebugLevel > 1) 
                  printf("Defer %d because distance is %.1f%%\n", jpt, xsum);
                group[jpt] = 0;
              }
            }
          }
        }
      }
    }
  }
  sumsToAvgSD(errsum, errsq, numErr, &xsum, &ysum);
  if (sDebugLevel) {
    printf("Distance from nearest plane for %d points: mean %.1f%%  SD %.1f%% "
           " max %.1f%%\n", numErr, xsum, ysum, errmax);
    fflush(stdout);
  }
  free(xrot);
  free(yrot);
  free(zrot);
  free(idx);
  free(idy);
  free(ringStart);
  free(numInSquare);
  free(squareInd);
  free(pointLists);
  free(steepNeigh);
  free(sortInd);
  free(squareDone);
  free(steepAngle);
  free(xfit);
  free(yfit);
  free(zfit);
  free(grpfit);
  free(delZ);
  free(outlie);
  free(cluster);
  free(clusterSY);
  free(clusterSX);
  fflush(stdout);
  return 0;
} 

/*!
 * Fortran wrapper for @surfaceSort, where [xyz] is dimensioned to (3,*)
 */
int surfacesort(float *xyz, int *numPts, int *markersInGroup, int *group)
{
  return surfaceSort(xyz, *numPts, *markersInGroup, group);
}
