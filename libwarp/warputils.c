/*  warputils.c - utility functions for warp grids and transforms
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "warpfiles.h"
#include "b3dutil.h"

static int newGridLimits(int *nxGrid, float *xStart, float xInterval, int xdim, int ixmin,
                         int ixmax, float xBigStr, float xBigEnd);
static int adjustSizeAndStart(int nxwarp, int ixgdim, float xmin, float xmax,
                              float xGridIntrv, float *xGridStrt);

/*!
 * Returns a value from one position in a 2D warping grid.  ^
 * [x], [x] is the position in the grid ^
 * [dxGrid], [dyGrid] are the grid arrays, with first dimension [ixgDim] ^
 * [nxGrid], [nyGrid] are the number of grid points in X and Y ^
 * [xGridStart], [yGridStart] are coordinates at which the grid starts ^
 * [xGridIntrv], [yGridIntrv] are the spacing between grid points in X and Y ^
 * [dx] and [dy] are returned with the interpolated values at the given position.
 */
void interpolateGrid(float x, float y, float *dxGrid, float *dyGrid, int ixgDim,
                      int nxGrid, int nyGrid, float xGridStart, float yGridStart,
                      float xGridIntrv, float yGridIntrv, float *dx, float *dy)
{
  float xgrid,ygrid,fx1,fx,fy1,fy,c00,c10,c01,c11;
  int ixg,iyg,ixg1,iyg1;

  xgrid = (x - xGridStart) / xGridIntrv;
  ixg = xgrid;
  ixg = B3DMAX(0,B3DMIN(nxGrid-2,ixg));
  fx1 = B3DMAX(0.,B3DMIN(1.,xgrid-ixg));       /* NO EXTRAPOLATIONS ALLOWED */
  fx = 1.-fx1;
  ixg1 = ixg+1;
  ygrid  =  (y - yGridStart) / yGridIntrv;
  iyg = ygrid;
  iyg = B3DMAX(0,B3DMIN(nyGrid-2,iyg));
  fy1 = B3DMAX(0.,B3DMIN(1.,ygrid-iyg));
  fy = 1.-fy1;
  iyg1 = iyg+1;
  c00 = fx*fy;
  c10 = fx1*fy;
  c01 = fx*fy1;
  c11 = fx1*fy1;

  /* interpolate */
  *dx = c00*dxGrid[ixg + iyg * ixgDim] + c10*dxGrid[ixg1 + iyg * ixgDim] +
    c01*dxGrid[ixg + iyg1 * ixgDim] + c11*dxGrid[ixg1 + iyg1 * ixgDim];
  *dy = c00*dyGrid[ixg + iyg * ixgDim] + c10*dyGrid[ixg1 + iyg * ixgDim] +
    c01*dyGrid[ixg + iyg1 * ixgDim] + c11*dyGrid[ixg1 + iyg1 * ixgDim];
}

/*!
 * Finds the point that a grid maps to [x], [y] by iteration.  Returns the coordinates
 * of that point in [xnew], [ynew] (which can be the same as [x], [y]) and the 
 * interpolated grid values at that point in [dx] and [dy].  Grid parameters are the same 
 * as in @@interpolateGrid@.
 */
void findInversePoint(float x, float y, float *dxGrid, float *dyGrid, int ixgDim,
                      int nxGrid, int nyGrid, float xGridStart, float yGridStart, 
                      float xGridIntrv, float yGridIntrv, float *xnew, float *ynew,
                      float *dx, float *dy)
{
  int maxIter = 10;
  double changeCrit = 0.01;
  int iter;
  float xlast = x, ylast = y;

  for (iter = 0; iter < maxIter; iter++) {
    interpolateGrid(xlast, ylast, dxGrid, dyGrid, ixgDim, nxGrid, nyGrid, xGridStart, 
                     yGridStart, xGridIntrv, yGridIntrv, dx, dy);
    *xnew = x - *dx;
    *ynew = y - *dy;
    if (fabs((double)(*xnew - xlast)) < changeCrit && 
        fabs((double)(*ynew - ylast)) < changeCrit)
      break;
    xlast = *xnew;
    ylast = *ynew;
  }
}

/*!
 * Finds the inverse of a warping consisting of a linear transform in [xform] followed
 * by a grid of inverse warp displacements in [dxGrid], [dyGrid].  The center of transform
 * is specified in [xcen], [ycen].  The linear component of the inverse is returned in
 * [xfInv] and the inverse warp displacements of the inverse are returned in [dxInv], 
 * [dyInv].  [rows] specifies the number of rows of the transform (2 or 3; omitted when
 * calling from Fortran).  Grid parameters are the same as in @@interpolateGrid@.
 */
void invertWarpGrid(float *dxGrid, float *dyGrid, int ixgDim, int nxGrid, int nyGrid,
                    float xGridStart, float yGridStart, float xGridIntrv, 
                    float yGridIntrv, float *xform, float xcen, float ycen, float *dxInv,
                    float *dyInv, float *xfInv, int rows)
{
  int ix, iy;
  float xgrid, ygrid, dx, dy, xnew, ynew;
  xfInvert(xform, xfInv, rows);
  for (iy = 0; iy < nyGrid; iy++) {
    for (ix = 0; ix < nxGrid; ix++) {
      ygrid = yGridStart + iy * yGridIntrv;
      xgrid = xGridStart + ix * xGridIntrv;
      xfApply(xform, xcen, ycen, xgrid, ygrid, &xgrid, &ygrid, rows);
      findInversePoint(xgrid, ygrid, dxGrid, dyGrid, ixgDim, nxGrid, nyGrid, xGridStart,
                       yGridStart, xGridIntrv, yGridIntrv, &xnew, &ynew, &dx, &dy);
      dxInv[ix + iy * ixgDim] = -dx;
      dyInv[ix + iy * ixgDim] = -dy;
    }
  }
}

/*static void printxf(float *xf)
{  fprintf(stderr, "%f %f %f %f %f %f\n", xf[0], xf[2], xf[1], xf[3], xf[4], xf[5]);
}*/

/*!
 * Multiply two warp transforms together.  Each warping is specified by a linear
 * transform applied first, and the inverse of warp displacement vectors.  The warp
 * transform applied first has its linear component in [xform1] and has displacements in 
 * arrays [dxGrid1] and [dyGrid1], with [nxGrid1] by [nyGrid1] elements and an X 
 * dimension of [ixgDim1].  The grid starts at [xStart1], [yStart1] and has intervals in X
 * and Y of [xIntrv1] and [yIntrv1].  The warp transform applied second is specified by 
 * the corresponding arguments ending in 2.  The center of transformation is given by 
 * [xcen], [ycen].  Set [useSecond] nonzero to make an output grid whose parameters match
 * that of the second input grid rather than the first.  The number of rows of the 
 * linear transforms is specified in [rows] (2 or 3, omit when calling from Fortran).
 * The output warp grid is returned in [dxProd], [dyProd] and the linear transform is
 * returned in [xfProd].  The second warp grid may be omitted by setting [nxGrid] to zero,
 * but the first warp grid must exist.  Returns 1 for memory or other errors.
 */
int multiplyWarpings(float *dxGrid1, float *dyGrid1, int ixgDim1, int nxGrid1, 
                      int nyGrid1, float xStart1, float yStart1, float xIntrv1, 
                      float yIntrv1, float *xform1, float xcen, float ycen, 
                      float *dxGrid2, float *dyGrid2, int ixgDim2, int nxGrid2,
                      int nyGrid2, float xStart2, float yStart2, float xIntrv2, 
                      float yIntrv2, float *xform2, float *dxProd,
                      float *dyProd, float *xfProd, int useSecond, int rows)
{

  /* Select the parameters of the output grid */
  int ixgDim = useSecond ? ixgDim2 : ixgDim1;
  int nxGrid = useSecond ? nxGrid2 : nxGrid1;
  float xGridStart = useSecond ? xStart2 : xStart1;
  float xGridIntrv = useSecond ? xIntrv2 : xIntrv1;
  int nyGrid = useSecond ? nyGrid2 : nyGrid1;
  float yGridStart = useSecond ? yStart2 : yStart1;
  float yGridIntrv = useSecond ? yIntrv2 : yIntrv1;
  float xfinv1[9], xfinv2[9], dx, dy, xgrid, ygrid, xnew, ynew;
  float *xpos, *ypos;
  int ix, iy, err, ind;

  if (rows / 2 != 1 || !nxGrid)
    return 1;
  
  /* Need arrays for positions for extracting linear transform.  Make them only big 
     enough for packed data */
  xpos = B3DMALLOC(float, ixgDim * nyGrid);
  ypos = B3DMALLOC(float, ixgDim * nyGrid);
  if (!xpos || !ypos) {
    B3DFREE(xpos);
    B3DFREE(ypos);
    return 1;
  }

  xfInvert(xform2, xfinv2, rows);
  xfInvert(xform1, xfinv1, rows);
  /*printxf(xform1);
  printxf(xfinv1);
  printxf(xform2);
  printxf(xfinv2); */
  for (iy = 0; iy < nyGrid; iy++) {
    ygrid = yGridStart + iy * yGridIntrv;
    for (ix = 0; ix < nxGrid; ix++) {
      ind = ix + nxGrid * iy;
      xgrid = xGridStart + ix * xGridIntrv;
      dx = 0.;
      dy = 0.;

      /* Back-transform through the second grid if it exists */
      if (nxGrid2)
        interpolateGrid(xgrid, ygrid, dxGrid2, dyGrid2, ixgDim2, nxGrid2, nyGrid2,
                         xStart2, yStart2, xIntrv2, yIntrv2, &dx, &dy);

      /* Then by inverse of f2, then through the first grid, then by inverse of f1 */
      xfApply(xfinv2, xcen, ycen, xgrid + dx, ygrid + dy, &xnew, &ynew, rows);
      interpolateGrid(xnew, ynew, dxGrid1, dyGrid1, ixgDim1, nxGrid1, nyGrid1, xStart1,
                       yStart1, xIntrv1, yIntrv1, &dx, &dy);
      xfApply(xfinv1, xcen, ycen, xnew + dx, ynew + dy, &xnew, &ynew, rows);

      /* Store data packed contiguously */
      dxProd[ind] = xnew - xgrid;
      dyProd[ind] = ynew - ygrid;
      xpos[ind] = xgrid;
      ypos[ind] = ygrid;
    }
  }
  err = extractLinearXform(xpos, ypos, dxProd, dyProd, nxGrid * nyGrid, xcen, ycen, 
                           dxProd, dyProd, xfProd, rows);
  if (!err) {

    /* Get the linear transform and unpack into larger array if needed */
    /*printxf(xfProd);*/
    if (ixgDim != nxGrid) {
      for (iy = nyGrid - 1; iy >= 0; iy--) {
        for (ix = nxGrid - 1; ix >= 0; ix--) {
          ind = ix + nxGrid * iy;
          dxProd[ix + ixgDim * iy] = dxProd[ind];
          dyProd[ix + ixgDim * iy] = dyProd[ind];
        }
      }
    }
  }

  B3DFREE(xpos);
  B3DFREE(ypos);
  return err;
}

/*!
 * Extracts the linear transform embedded in a set of positions and vectors, and modifies
 * the vectors to remove the transform.  The positions of [nPoints] points are in [xPos],
 * [yPos] and the X and Y displacements are in [xVector] and [yVector].  The center for 
 * transformations is [xcen], [ycen].  The modified vectors are returned in [newXvec] and
 * [newYvec], which can be the same as [xVector] and [yVector].  The inverse of the
 * embedded transform is returned in [xfinv] with the number of rows specified in [rows].
 * Returns 1 for a memory allocation error, fewer than 3 points, or [rows] not 2 or 3.
 */
int extractLinearXform(float *xPos, float *yPos, float *xVector, float *yVector,
                       int nPoints, float xcen, float ycen, float *newXvec, 
                       float *newYvec, float *xfinv, int rows)
{
  float *ptmp;
  float mat[6], xtmp, ytmp;
  int i;

  if (nPoints < 3 || rows < 2 || rows > 3)
    return 1;

  ptmp = B3DMALLOC(float, nPoints);
  if (!ptmp)
    return 1;

  /* Need to fit points as function of points plus vector; this gives the
   inverse of the embedded transform.  This is the only use of [xy]Vector so new[XY]vec
   can be the same */
  for (i = 0; i < nPoints; i++) {
    newXvec[i] = xPos[i] - xcen + xVector[i];
    newYvec[i] = yPos[i] - ycen + yVector[i];
    ptmp[i] = xPos[i] - xcen;
    /* printf("%d %.2f %.2f %.2f\n", i, newXvec[i], newYvec[i], ptmp[i]); */
  }
  lsFit2(newXvec, newYvec, ptmp, nPoints, &mat[0], &mat[2], &mat[4]);
  for (i = 0; i < nPoints; i++)
    ptmp[i] = yPos[i] - ycen;
  lsFit2(newXvec, newYvec, ptmp, nPoints, &mat[1], &mat[3], &mat[5]);
  /* printf("   %.6f  %.6f  %.6f  %.6f  %.2f  %.2f\n", mat[0], mat[2], mat[1], mat[3], 
     mat[4], mat[5]); */

  /* Apply this transform and subtract the point positions to get the new vectors */
  for (i = 0; i < nPoints; i++) {
    xfApply(mat, 0., 0., newXvec[i], newYvec[i], &xtmp, &ytmp, 2);
    newXvec[i] = xtmp + xcen - xPos[i];
    newYvec[i] = ytmp + ycen - yPos[i];
    /* printf("%d %.2f %.2f %.2f %.2f\n", i, xPos[i], yPos[i], newXvec[i], newYvec[i]);*/
  }

  /* Return the inverse of the embedded transform, let caller use it or take inverse */
  xfCopy(mat, 2, xfinv, rows);
  free(ptmp);
  return 0;
}

#define MAX_THREADS 16
static int *sNumNeigh = NULL;
static int *sIndNeighStart = NULL;
static int *sNeighbors = NULL;

/*!
 * Extrapolates a grid from locations marked as solved to those not marked.  The grid
 * of X and Y displacements is in [dxGrid] and [dyGrid].  [solved] is an corresponding 
 * array with 0 for a location to be fille din, and 1 for a location with a vector.
 * The X dimension of [dxGrid], [dyGrid], and [solved] is given by [xdim].  [solved] must
 * be six times the size of this array (6 * xdim * nyGrid) in order to be used for 
 * temporary storage.  The number of grid elements in X and Y is given by [nxGrid] and 
 * [nyGrid]; the intervals between grid points in X and Y are given by [xInterval] and
 * [yInterval].  If [reuse] is non-zero, it uses stored information about how to 
 * extrapolate from the last grid analyzed; this should only be done if the the last grid
 * had the identical pattern of solved locations.  Some of this information is stored in
 * the [solved] array and the rest is stored in arrays that are always kept, until
 * @extrapolateDone is called.  Returns 1 for a memory allocation error or an attempt to
 * reuse when there is no stored data.
 */
int extrapolateGrid(float *dxGrid, float *dyGrid, char *solved, int xdim, int nxGrid,
                    int nyGrid, float xInterval, float yInterval, int reuse)
{
  int dxCorn[4] = {-1, 1, 1, -1};
  int dyCorn[4] = {-1, -1, 1, 1};
  int dxAlong[4] = {1, 0, -1, 0};
  int dyAlong[4] = {0, 1, 0, -1};
  int ixstep[12] = {1,1,1,0,-1,-1,-1,0,1,1,1,0};
  int iystep[12] = {-1,0,1,1,1,0,-1,-1,-1,0,1,1};
  char *blockType = solved + xdim * nyGrid;
  int *indList = (int *)(blockType + xdim * nyGrid);
  int lind, numAllNeigh, numThreads, thread;
  static int ninList = 0;
  int quantum = 1000;
  int thrMaxNeigh[MAX_THREADS];
  int thrNumAllNeigh[MAX_THREADS];
  int thrListStart[MAX_THREADS + 1];
  int *thrNeighbors[MAX_THREADS];
  float xfOfsX[6], xfOfsY[6];
  float range = 2.;
  int ix, iy, jx, jy, minx, miny, ixcorn, iycorn, ixyind, delta, dir, num, start, end;
  float dist, dmin, angle, dx, dy, wsum, distcrit, dlook, xyint, dxcen, dycen;
  float a11, a12, a21, a22, dx00, dx01, dx10, dx11, dy00, dy01, dy10, dy11;
  int nayx, nayy, is, indDom, jxmin, jxmax, jymin, jymax, i, boundary, jxyind, btype;
  /* double wallStart = wallTime(); */

  if (reuse && !ninList)
    return 0;
  if (reuse && (!sNumNeigh || !sIndNeighStart || !sNeighbors))
    return 1;

  numThreads = numOMPthreads(B3DMIN(ninList, MAX_THREADS));

  if (!reuse) {

    B3DFREE(sNumNeigh);
    B3DFREE(sIndNeighStart);
    B3DFREE(sNeighbors);
    ninList = 0;

    /* Make list of indexes to do */
    for (iy = 0; iy < nyGrid; iy++) {
      for (ix = 0; ix < nxGrid; ix++) {
        ixyind = ix + iy * xdim;
        if (!solved[ixyind])
          indList[ninList++] = ixyind;
        /*if (solved[ixyind])
          printf("Solved: %d %d %.1f %.1f\n", ix, iy, dxGrid[ixyind], dyGrid[ixyind]);*/
      }
    }
    if (!ninList)
      return 0;

    /* First analyze all blocks (indexed by lower left corner) for whether they have all
       4 or 3 corners solved.  Number them by missing point (CCW from lower left) */
    for (iy = 0; iy < nyGrid - 1; iy++) {
      for (ix = 0; ix < nxGrid - 1; ix++) {
        ixyind = ix + iy * xdim;
        blockType[ixyind] = 0;
        if (solved[ixyind]) {
          if (solved[ixyind+1] && solved[ixyind+xdim] && solved[ixyind+xdim+1])
            blockType[ixyind] = 5;
          else if (solved[ixyind+1] && solved[ixyind+xdim])
            blockType[ixyind] = 3;
          else if (solved[ixyind+1] && solved[ixyind+xdim+1])
            blockType[ixyind] = 4;
          else if (solved[ixyind+xdim] && solved[ixyind+xdim+1])
            blockType[ixyind] = 2;
        } else if (solved[ixyind+1] && solved[ixyind+xdim] && solved[ixyind+xdim+1])
          blockType[ixyind] = 1;
      }
    }

    sNumNeigh = B3DMALLOC(int, ninList);
    sIndNeighStart = B3DMALLOC(int, ninList);
    for (thread = 0; thread < numThreads; thread++) {
      thrNumAllNeigh[thread] = 0;
      thrNeighbors[thread] = B3DMALLOC(int, quantum);
      thrMaxNeigh[thread] = quantum;
      if (!sNumNeigh || !sIndNeighStart || !thrNeighbors[thread]) {
        B3DFREE(sNumNeigh);
        B3DFREE(sIndNeighStart);
        for (ix = 0; ix <= thread; ix++)
          B3DFREE(thrNeighbors[thread]);
        return 1;
      }
    }
  }

  for (thread = 0; thread < numThreads; thread++) {
    thrListStart[thread] = (ninList * thread) / numThreads;
    if (reuse)
      thrNeighbors[thread] = sNeighbors;
  }
  thrListStart[numThreads] = ninList;
        
  /* The coordinate offsets from lower left corner to center of points */
  xfOfsX[2] = xfOfsX[3] = xInterval / 3.;
  xfOfsX[1] = xfOfsX[4] = 2. * xInterval / 3.;
  xfOfsX[5] = xInterval / 2.;
  xfOfsY[3] = xfOfsY[4] = yInterval / 3.;
  xfOfsY[1] = xfOfsY[2] = 2. * yInterval / 3.;
  xfOfsY[5] = yInterval / 2.;
  xyint = B3DMIN(xInterval, yInterval);


  /* This is successful enough: speedup ~2.2 @ 4 cores, 3.7 @ 8 cores, 5.1 @ 12 */

#pragma omp parallel for                                          \
  shared(indList, xdim, dxGrid, dyGrid, nxGrid, nyGrid, xInterval, yInterval, xyint, \
         xfOfsX, xfOfsY, blockType, ninList, range, dxCorn, dyCorn, dxAlong, dyAlong, \
         ixstep, iystep, thrListStart, thrNeighbors, thrNumAllNeigh, thrMaxNeigh, \
         sNumNeigh, sIndNeighStart, quantum)                              \
  private(ix, iy, jx, jy, minx, miny, ixcorn, iycorn, ixyind, delta, dir, num, start, \
          end, dist, dmin, angle, dx, dy, wsum, distcrit, dlook, dxcen, dycen, \
          a11, a12, a21, a22, dx00, dx01, dx10, dx11, dy00, dy01, dy10, dy11, \
          nayx, nayy, is, indDom, jxmin, jxmax, jymin, jymax, i, boundary, jxyind, \
          btype, lind, thread)
  for (thread = 0; thread < numThreads; thread++) {
    for (lind = thrListStart[thread]; lind < thrListStart[thread+1]; lind++) {
      ixyind = indList[lind];
      ix = ixyind % xdim;
      iy = ixyind / xdim;

      if (!reuse) {
        sNumNeigh[lind] = 0;
        sIndNeighStart[lind] = thrNumAllNeigh[thread];

        dmin = 1.e30;

        /* find closest block with transform */
        /* Search progressively larger squares until half-side > minimum distance */
        for (delta = 1; delta < B3DMAX(nxGrid, nyGrid); delta++) {
          if ((delta - 0.7) * (delta - 0.7) * xyint * xyint  > dmin)
            break;

          /* printf ("%d %d search delta %d\n", ix, iy, delta);*/
          /* Look at the 4 edges of the square, 4 directions;  Start in corner */
          for (dir = 0; dir < 4; dir++) {
            ixcorn = ix + dxCorn[dir] * delta;
            iycorn = iy + dyCorn[dir] * delta;

            /* If whole edge is out, skip */
            if ((dxAlong[dir] && (iycorn < 0 || iycorn >= nyGrid - 1)) ||
                (dyAlong[dir] && (ixcorn < 0 || ixcorn >= nxGrid - 1)))
              continue;
            num = 2 * delta;

            /* Adjust the other dimension's limits */
            if (dxAlong[dir]) {
              start = B3DMAX(0, B3DMIN(nxGrid - 2, ixcorn));
              end = ixcorn + (num - 1) * dxAlong[dir];
              end = B3DMAX(0, B3DMIN(nxGrid - 2, end));
              ixcorn = start;
            } else {
              start = B3DMAX(0, B3DMIN(nyGrid - 2, iycorn));
              end = iycorn + (num - 1) * dyAlong[dir];
              end = B3DMAX(0, B3DMIN(nyGrid - 2, end));
              iycorn = start;
            }

            /* Test each position along the edge */
            num = B3DMAX(start - end, end - start) + 1;
            /* printf("dir %d, test num %d, corner %d %d  end %d %d  dmin %.1f\n", dir,
               num, ixcorn, iycorn, ixcorn + (num - 1) * dxAlong[dir], 
               iycorn + (num - 1) * dyAlong[dir], dmin);*/
            for (i = 0; i < num; i++) {
              btype = blockType[ixcorn + iycorn * xdim];
              if (btype) {
                dx = xInterval * (ixcorn - ix) + xfOfsX[btype];
                dy = yInterval * (iycorn - iy) + xfOfsY[btype];
                dist = dx * dx + dy * dy;
                if (dist < dmin) {
                  dmin = dist;
                  minx = ixcorn;
                  miny = iycorn;
                }
              }
              ixcorn += dxAlong[dir];
              iycorn += dyAlong[dir];
            }
          }
        }

        /* Get actual distance to look, range of indexes to search, and
           the criterion which is square of maximum distance */
        dist = range * (float)sqrt((double)dmin);
        dlook = dist / B3DMAX(xInterval, 1.) + 1.;
        jxmin = B3DMAX(0, B3DNINT(ix - dlook - 1));
        jxmax = B3DMIN(nxGrid - 2, B3DNINT(ix + dlook));
        dlook = dist / B3DMAX(yInterval, 1.) + 1.;
        jymin = B3DMAX(0, B3DNINT(iy - dlook - 1));
        jymax = B3DMIN(nyGrid - 2, B3DNINT(iy + dlook));
        distcrit = dist * dist;
        /* printf("nearest %d %d  dist %.1f  jxmm %d %d  jymm %d %d\n", minx, miny,
           sqrt(dist), jxmin, jxmax, jymin, jymax);*/
               
        /* Loop in the neighborhood, find boundary points within range */
        for (jy = jymin; jy <= jymax; jy++) {
          for (jx = jxmin; jx <= jxmax; jx++) {
            btype = blockType[jx + jy * xdim];
            if (btype) {
              dxcen = (jx - ix) * xInterval + xfOfsX[btype];
              dycen = (jy - iy) * yInterval + xfOfsY[btype];
              dist = dxcen * dxcen + dycen * dycen;
              if (dist <= distcrit) {
                
                /* Find dominant direction to the point */
                angle = atan2(dycen, dxcen) / 0.017453293 + 157.5;
                if (angle < 0.) 
                  angle += 360.;
                indDom = angle / 45.;
                indDom = B3DMAX(0, B3DMIN(7, indDom));
                
                /* Check that this point is a boundary, i.e. does not
                   have a neighbor in any one of the 5 directions toward
                   or at right angles to the dominant direction */
                boundary = 0;
                for (is = indDom; is <= indDom + 4 && !boundary; is++) {
                  nayx = jx + ixstep[is];
                  nayy = jy + iystep[is];
                  if (nayx >= 0 && nayx < nxGrid-1 && nayy >= 0 && nayy < nyGrid-1 &&
                      !blockType[nayx + nayy * xdim]) 
                    boundary = 1;
                }
                
                /* For boundary or min point, add to weighted sum */
                if (boundary || ((jx == minx && jy == miny) && thrNeighbors[thread])) {
                  sNumNeigh[lind]++;
                  if (thrNumAllNeigh[thread] >= thrMaxNeigh[thread]) {
                    thrMaxNeigh[thread] += quantum;
                    B3DREALLOC(thrNeighbors[thread], int, thrMaxNeigh[thread]);
                    if (!thrNeighbors[thread])
                      sNumNeigh[lind] = 0;
                  }
                  if (thrNeighbors[thread])
                    thrNeighbors[thread][thrNumAllNeigh[thread]++] = jx + jy * xdim;
                }
              }
            }
          }
        }
      }

      /* Now go through the neighbor list */
      dxGrid[ixyind] = 0.;
      dyGrid[ixyind] = 0.;
      wsum = 0.;
      for (nayx = 0; nayx < sNumNeigh[lind]; nayx++) {
        jxyind = thrNeighbors[thread][sIndNeighStart[lind] + nayx];
        jx = jxyind % xdim;
        jy = jxyind / xdim;
        btype = blockType[jxyind];
        dxcen = (jx - ix) * xInterval + xfOfsX[btype];
        dycen = (jy - iy) * yInterval + xfOfsY[btype];
        dist = dxcen * dxcen + dycen * dycen;
        
        /* Compute the vector transform centered on lower left of block */
        dx00 = dxGrid[jxyind];
        dx10 = dxGrid[jxyind + 1];
        dx01 = dxGrid[jxyind + xdim];
        dx11 = dxGrid[jxyind + xdim + 1];
        dy00 = dyGrid[jxyind];
        dy10 = dyGrid[jxyind + 1];
        dy01 = dyGrid[jxyind + xdim];
        dy11 = dyGrid[jxyind + xdim + 1];
        /* if (!ix && iy == 18)
          printf("%.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f\n", dx00, dx01, dx10, dx11, 
          dy00, dy01, dy10, dy11); */
        switch (btype) {

          /* These are either an exact transform (1-4) or best linear (5) */
        case 1:
          a11 = dx11 - dx01;
          a12 = dx11 - dx10;
          dx = dx10 + dx01 - dx11;
          a21 = dy11 - dy01;
          a22 = dy11 - dy10;
          dy = dy10 + dy01 - dy11;
          break;
        case 2:
          a11 = dx11 - dx01;
          a12 = dx01 - dx00;
          dx = dx00;
          a21 = dy11 - dy01;
          a22 = dy01 - dy00;
          dy = dy00;
          break;
        case 3:
          a11 = dx10 - dx00;
          a12 = dx01 - dx00;
          dx = dx00;
          a21 = dy10 - dy00;
          a22 = dy01 - dy00;
          dy = dy00;
          break;
        case 4:
          a11 = dx10 - dx00;
          a12 = dx11 - dx10;
          dx = dx00;
          a21 = dy10 - dy00;
          a22 = dy11 - dy10;
          dy = dy00;
          break;
        case 5:
          a11 = (dx10 - dx00 + dx11 - dx01) / 2.;
          a12 = (dx01 - dx00 + dx11 - dx10) / 2.;
          dx = (3. * dx00 + dx01 + dx10 - dx11) / 4.;
          a21 = (dy10 - dy00 + dy11 - dy01) / 2.;
          a22 = (dy01 - dy00 + dy11 - dy10) / 2.;
          dy = (3. * dy00 + dy01 + dy10 - dy11) / 4.;
          break;
        }
        /* if (!ix && iy == 18)
           printf("Adding in %d %d  btype %d  w %f\n", jx, jy, btype, 1./dist); */

        /* Convert this to a coordinate transform centered on the point being
           filled in, in which case dx, dy are the vector there. */
        a11 = 1. + a11 / xInterval;
        a12 = a12 / yInterval;
        a21 = a21 / xInterval;
        a22 = 1. + a22 / yInterval;
        /*  if (!ix && iy == 18)
            printf("  %.4f  %.4f  %.4f  %.4f  %.2f  %.2f", a11,a12,a21,a22, dx,dy); */
        dx = dx + dxcen - a11 * dxcen - a12 * dycen;
        dy = dy + dycen - a21 * dxcen - a22 * dycen;
        /* if (!ix && iy == 18)
           printf("  ->  %.2f  %.2f\n", dx,dy); */

        dxGrid[ixyind] += dx / dist;
        dyGrid[ixyind] += dy / dist;
        wsum += 1. / dist;
      }
      dxGrid[ixyind] /= wsum;
      dyGrid[ixyind] /= wsum;
      /* printf("Filled in %.1f %.1f  wsum %f\n", dxGrid[ixyind], dyGrid[ixyind], wsum);*/
    }
  }

  /* Now copy thread neighbor lists to one array and adjust start indices */
  if (!reuse) {
    if (numThreads > 1) {
      numAllNeigh = 0;
      nayx = 0;
      for (thread = 0; thread < numThreads; thread++) {
        numAllNeigh += thrNumAllNeigh[thread];
        if (!thrNeighbors[thread])
          nayx = 1;
      }

      sNeighbors = NULL;
      if (!nayx)
        sNeighbors = B3DMALLOC(int, numAllNeigh);
      if (sNeighbors) {
        numAllNeigh = 0;
        for (thread = 0; thread < numThreads; thread++) {
          for (ix = 0; ix < thrNumAllNeigh[thread]; ix++)
            sNeighbors[ix + numAllNeigh] = thrNeighbors[thread][ix];
          for (lind = thrListStart[thread]; lind < thrListStart[thread+1]; lind++)
            sIndNeighStart[lind] += numAllNeigh;
          numAllNeigh += thrNumAllNeigh[thread];
          B3DFREE(thrNeighbors[thread]);
        }
      }
      for (thread = 0; thread < numThreads; thread++)
        B3DFREE(thrNeighbors[thread]);
    } else

      /* But if only one thread, just need to assign the neighbors pointer */
      sNeighbors = thrNeighbors[0];
  }

  /* printf("Extrapolate time %.4f\n", 1000. * (wallTime() - wallStart)); */
  return sNeighbors == NULL ? 1 : 0 ;
}

/*!
 * Frees the arrays used by @extrapolateGrid
 */
void extrapolateDone()
{
  B3DFREE(sNumNeigh);
  B3DFREE(sIndNeighStart);
  B3DFREE(sNeighbors);
}


/*!
 * Expand a grid to a larger area by shifting it and then extrapolating it with
 * @@extrapolateGrid@.  ^
 * [dxGrid], [dyGrid] - grid arrays with displacements in X and Y ^
 * [xdim], [ydim] - X and Y dimensions of arrays ^
 * [nxGrid], [nyGrid] - call with original number of grid points in X and Y, returned 
 * with new number ^
 * [xStart], [yStart] - Starting X and Y coordinates of grid, returned with new values ^
 * [xInterval], [yInterval] - Spacing between points in X and Y ^
 * [xBigStr], [yBigStr] - Starting X and Y coordinates of larger area ^
 * [xBigEnd], [yBigEnd] - Ending X and Y coordinates of larger area ^
 * [ixmin], [ixmax] - limiting coordinates in X; grid will not go outside this ^
 * [iymin], [iymax] - limiting coordinates in X; grid will not go outside this ^
 * The new area covered by the grid will be at least as large as the larger area, unless 
 * that would make a grid point be out of bounds. Returns 1 for memory error.
 */
int expandAndExtrapGrid(float *dxGrid, float *dyGrid, int xdim, int ydim, int *nxGrid,
                        int *nyGrid, float *xStart, float *yStart, float xInterval, 
                        float yInterval, float xBigStr, float yBigStr, float xBigEnd,
                        float yBigEnd, int ixmin, int ixmax, int iymin, int iymax)
{
  char *solved;
  int nxOrig = *nxGrid;
  int nyOrig = *nyGrid;
  int ix, iy, addx, addy, jin, jout;

  /* Determine new limits in X and Y */
  addx = newGridLimits(nxGrid, xStart, xInterval, xdim, ixmin, ixmax, xBigStr, xBigEnd);
  addy = newGridLimits(nyGrid, yStart, yInterval, ydim, iymin, iymax, yBigStr, yBigEnd);
  if (*nxGrid == nxOrig && *nyGrid == nyOrig)
    return 0;

  /* Shift the grid and mark the solved array */
  solved = B3DMALLOC(char, 6 * xdim * *nyGrid);
  if (!solved)
    return 1;
  for (iy = *nyGrid - 1; iy >= 0; iy--) {
    for (ix = *nxGrid - 1; ix >= 0; ix--) {
      jout = ix + iy * xdim;
      if (ix < addx || ix >= nxOrig + addx || iy < addy || iy >= nyOrig + addy) {
        solved[jout] = 0;
        dxGrid[jout] = 0.;
        dyGrid[jout] = 0.;
      } else {
        solved[jout] = 1;
        jin = ix - addx + (iy - addy) * xdim;
        dxGrid[jout] = dxGrid[jin];
        dyGrid[jout] = dyGrid[jin];
      }
    }
  }
  jout = extrapolateGrid(dxGrid, dyGrid, solved, xdim, *nxGrid, *nyGrid, xInterval,
                         yInterval, 0);
  extrapolateDone();
  free(solved);
  return jout;
}

/* Determine new limits for expanding a grid into */
static int newGridLimits(int *nxGrid, float *xStart, float xInterval, int xdim, int ixmin,
                         int ixmax, float xBigStr, float xBigEnd)
{
  int nxgin = *nxGrid;
  int addlo, addhi, extra, sublo, subhi;
  
  /* Add grid points to get outside the big start and end, but limit it to stay between
     ixmin and ixmax */
  /* fprintf(stderr, "new lim: %d %f %f %d %d %f %f\n", *nxGrid, *xStart, xInterval, xdim,
     nx, xBigStr, xBigEnd); */
  addlo = (int)ceil((*xStart - xBigStr) / xInterval);
  while (*xStart - addlo * xInterval <= ixmin)
    addlo--;
  addlo = B3DMAX(0, addlo);
  addhi = (int)ceil((xBigEnd - (*xStart + (nxgin - 1) * xInterval)) / xInterval);
  addhi = B3DMAX(0, addhi);
  while (*xStart + (addhi + nxgin - 1) * xInterval >= ixmax) 
    addhi--;
  addhi = B3DMAX(0, addhi);

  /* Then if this is too many points for the array, trim equally from both sides */
  extra = nxgin + addlo + addhi - xdim;
  if (extra > 0) {
    sublo = B3DMIN(extra / 2, addlo);
    subhi = B3DMIN(extra - sublo, addhi);
    
    /* If that can't work without throwing away existing data, forget it */
    if (sublo + subhi < extra)
      return 0;
    addlo -= sublo;
    addhi -= extra - sublo;
  }
  *xStart -= addlo * xInterval;
  *nxGrid += addlo + addhi;
  /* fprintf(stderr, "New: %d %d %d\n", *nxGrid, addlo, addhi); */
  return addlo;
}

#define ERR_RETURN(a) {                 \
    strncpy(errString, a, lenString-1); \
    return -2;                          \
  }

/*!
 * Attempts to open a transform file in [filename] as a warping file and performs basic 
 * checks on it.  Set [needDist] to 1 if the file must be a distortion field file with
 * an inverse transform grid, or [needInv] to 1 if a warping file must contain inverse 
 * transforms.  The return value is a warping file index, -1 for a linear transform file,
 * or -2 for an error.  In case of an error, [errString] is filled with an error message;
 * [lenString] is the size of [errString] (omitted when calling from Fortran).  For a 
 * warping file, returns the image size in [nx], [ny]. number of sections in [nz], 
 * binning of images in [ibinning], pixel size in [pixelSize], and flags in [iflags].
 */
int readCheckWarpFile(char *filename, int needDist, int needInv, int *nx, int *ny, 
                      int *nz, int *ibinning, float *pixelSize, int *iflags, 
                      char *errString, int lenString)
{
  int iversion, ierr;
  ierr = readWarpFile(filename, nx, ny, nz, ibinning, pixelSize, &iversion, iflags);
  errString[lenString-1] = 0x00;
  if (needDist) {
    if (ierr < 0)
      ERR_RETURN("OPENING OR READING DISTORTION FILE");
    if (*iflags % 2 == 0)
      ERR_RETURN("DISTORTION CORRECTION CAN BE DONE ONLY WITH INVERSE TRANSFORMS");
    if ((*iflags / 2) % 2 != 0)
      ERR_RETURN("DISTORTION CORRECTION CAN BE DONE ONLY WITH A WARPING GRID, "
              "NOT CONTROL POINTS");
  } else if (ierr < 0 && (iversion != 0 || ierr != -3)) {
    if (ierr > -3)
      ERR_RETURN("OPENING OR READING TRANSFORM FILE");
    ERR_RETURN("INAPPROPRIATE VALUE OR MEMORY ERROR PROCESSING TRANSFORM FILE"
               " AS A WARPING FILE (IT DOES NOT APPEAR TO BE A LINEAR TRANSFORM FILE)");
  }
  if (ierr < 0)
    ierr = -1;
  else if (needInv && *iflags % 2 == 0)
    ERR_RETURN("THIS PROGRAM WILL WORK ONLY WITH INVERSE WARP DISPLACEMENTS");
  return ierr;
}

/*!
 * Determines the maximum grid size needed in X and Y for all of the transforms in the
 * current warping file.  Set the needed minimum and maximum 
 * X and Y coordinates to be composed in [xmin], [xmax], [ymin], and [ymax].  The number 
 * of control points, or 4 for a grid, is returned in [nControl], which must be allocated 
 * before calling.  The maximum grid sizes in X amd Y are returned in [maxNxg] and
 * [maxNyg].  Returns -2 for an error and places an error message in [errString];
 * [lenString] is the size of [errString] (omitted when calling from Fortran).
 */
int findMaxGridSize(float xmin, float xmax, float ymin, float ymax, int *nControl,
                    int *maxNxg, int *maxNyg, char *errString, int lenString)
{
  int nxwarp, nywarp, nzwarp, controlPts, iz, iy, nxGrid, nyGrid;
  float yIntMin, xIntMin, xGridStrt, yGridStrt, xGridIntrv, yGridIntrv;
  if (getWarpFileSize(&nxwarp, &nywarp, &nzwarp, &controlPts))
    ERR_RETURN("GETTING MAX GRID SIZE - THERE IS NO CURRENT WARP FILE");
  
  xIntMin = 1.e20;
  yIntMin = 1.e20;
  *maxNxg = 0;
  *maxNyg = 0;
  for (iz = 0; iz < nzwarp; iz++) {
    nControl[iz] = 4;
    if (controlPts) {
      if (getNumWarpPoints(iz, &nControl[iz]))
        ERR_RETURN("GETTING NUMBER OF CONTROL POINTS");
    }
    if (nControl[iz] >= 3) {
      if (controlPts) {
        if (gridSizeFromSpacing(iz, -1., -1., 1)) 
          ERR_RETURN("SETTING GRID SIZE FROM SPACING OF CONTROL POINTS");
      }
      if (getGridParameters(iz, &nxGrid, &nyGrid, &xGridStrt, &yGridStrt, &xGridIntrv,
                            &yGridIntrv)) 
        ERR_RETURN("GETTING GRID PARAMETERS");
      xIntMin = B3DMIN(xIntMin, xGridIntrv);
      yIntMin = B3DMIN(yIntMin, yGridIntrv);
    }
  }
       
  /* Allow the grid to be expanded to fit the actual image, at the minimum interval.
     Allow an extra position by adding 2 */
  if (xIntMin < 1.e19) {
    iz = B3DNINT(B3DMAX(nxwarp, xmax) - B3DMIN(0.,xmin));
    iy = B3DNINT(B3DMAX(nywarp, ymax) - B3DMIN(0.,ymin));
    *maxNxg = (int)ceil(iz / xIntMin) + 2;
    *maxNyg = (int)ceil(iy / yIntMin) + 2;
  }
  return 0;
}

/*!
 * Gets a warping grid for section [iz] of the current warping file, expanded to fill
 * the needed area and adjusted for size changes and offsets.  ^
 * [xnbig, ynbig] - Size of the needed area, in warp file coordinates ^
 * [xOffset], [yOffset] - Offset of needed area from the image used to get the warping ^
 * [adjustStart] - Set to 1 to have starting coordinates adjusted for difference in size
 * and for the offset, and to have the grid vectors adjusted for the offset too ^
 * [warpScale] - Scaling between input image pixel size and warp file pixel size ^
 * [iBinning] - Binning of pixels for output.  Vectors, starts, and intervals will be all
 * be multiplied by [warpScale] / [iBinning] ^
 * [nxGrid], [nyGrid] - Number of positions in returned grid in X and Y ^
 * [xGridStrt], [yGridStrt] - Returned starting coordinate of grid ^
 * [xGridIntrv], [yGridIntrv] - Returned interval between grid points ^
 * [fieldDx], [fieldDy] - Grid of displacements in X and Y ^
 * [ixgDim], [iygDim] - Dimensions of grid arrays in X and Y ^
 * Returns -2 for an error and places an error message in [errString], whose size should
 * be supplied in [lenString] (but not when calling from Fortran).
 */
int getSizeAdjustedGrid(int iz, float xnbig, float ynbig, float xOffset, float yOffset,
                        int adjustStart, float warpScale, int iBinning, int *nxGrid,
                        int *nyGrid, float *xGridStrt, float *yGridStrt, 
                        float *xGridIntrv, float *yGridIntrv, float *fieldDx,
                        float *fieldDy, int ixgdim, int iygdim, char *errString,
                        int lenString)
{
  int nxwarp, nywarp, nzwarp, controlPts, ierr, i, iy;
  float binRatio, xAdd, yAdd, xmin, xmax, ymin, ymax;
  if (getWarpFileSize(&nxwarp, &nywarp, &nzwarp, &controlPts))
    ERR_RETURN("GETTING SIZE-ADJUSTED GRID - THERE IS NO CURRENT WARP FILE");

  /* For control points, see if we need to expand the sampled grid.  For stability, we
   always provide the full grid over the aligned area and expand by extrapolation */
  if (controlPts) {
    ierr = gridSizeFromSpacing(iz, -1., -1., 1);
    ierr = getGridParameters(iz, nxGrid, nyGrid, xGridStrt, yGridStrt,
                             xGridIntrv, yGridIntrv);
    /* fprintf(stderr, "iz %d nxyg %d %d st %f %f int %f %f\n", iz, *nxGrid, *nyGrid,
     *xGridStrt, *yGridStrt, *xGridIntrv, *xGridIntrv); */

    /* The area actually needed */
    xmin = nxwarp / 2. - xnbig / 2. + xOffset;
    xmax = xmin + xnbig;
    ymin = nywarp / 2. - ynbig / 2. + yOffset;
    ymax = ymin + ynbig;
    if (xmax > nxwarp || xmin < 0 || ymax > nywarp || ymin < 0) {

      /* If the area is larger, get new size and starting points */
      *nxGrid = adjustSizeAndStart(nxwarp, ixgdim, xmin, xmax, *xGridIntrv, xGridStrt);
      *nyGrid = adjustSizeAndStart(nywarp, iygdim, ymin, ymax, *yGridIntrv, yGridStrt);
      ierr = setGridSizeToMake(iz, *nxGrid, *nyGrid, *xGridStrt, *yGridStrt,
                               *xGridIntrv, *yGridIntrv);
    }
  }
  if (getWarpGrid(iz, nxGrid, nyGrid, xGridStrt, yGridStrt, xGridIntrv,
                  yGridIntrv, fieldDx, fieldDy, ixgdim))
    ERR_RETURN("GETTING WARP GRID OR DISTORTION FIELD");

  /* If images are not full field, adjust grid start by half the difference between 
     image and field size, still in warp file pixels. 
     Also subtract the offset and set up to add it to the vectors */
  xAdd = 0.;
  yAdd = 0.;
  if (adjustStart) {
    *xGridStrt += (xnbig - nxwarp) / 2. - xOffset;
    *yGridStrt += (ynbig - nywarp) / 2. - yOffset;
    xAdd = xOffset;
    yAdd = yOffset;
  }
             
  /* Then expand a grid to fill the space */
  if (! controlPts) {
    if (expandAndExtrapGrid(fieldDx, fieldDy, ixgdim, iygdim, nxGrid, nyGrid, xGridStrt,
                            yGridStrt, *xGridIntrv, *yGridIntrv, 0., 0., xnbig, ynbig,
                            B3DNINT(xOffset), B3DNINT(xnbig + xOffset), B3DNINT(yOffset),
                            B3DNINT(ynbig + yOffset)))
      ERR_RETURN("EXTRAPOLATING WARPING/DISTORTION GRID TO FULL AREA");
  }
  
  /* Next adjust grid start and interval and field itself for the
     overall binning or change of scale */
  binRatio = warpScale / iBinning;
  *xGridStrt *= binRatio;
  *yGridStrt *= binRatio;
  *xGridIntrv *= binRatio;
  *yGridIntrv *= binRatio;

  /* scale field */
  for (iy = 0; iy < *nyGrid; iy++) {
    for (i = 0; i < *nxGrid; i++) {
      fieldDx[i + iy * ixgdim] = (fieldDx[i + iy * ixgdim] + xAdd) * binRatio;
      fieldDy[i + iy * ixgdim] = (fieldDy[i + iy * ixgdim] + yAdd) * binRatio;
    }
  }
  /*fprintf(stderr,"%d %d %f %f %f %f\n", *nxGrid, *nyGrid, *xGridStrt, *yGridStrt,
          *xGridIntrv, *yGridIntrv);
  for (i = 0; i < 6; i++)
    fprintf(stderr," %.2f %.2f", fieldDx[i], fieldDy[i]);
    puts(" "); */
  return 0;
}

/* Routine to adjust size and starting point of one dimension for a control grid */
static int adjustSizeAndStart(int nxwarp, int ixgdim, float xmin, float xmax,
                              float xGridIntrv, float *xGridStrt)
{
  int nxgr;
  float gstr, gend;

  /* compute starts and ends that encompass the needed area and get new grid size */
  gstr = B3DMIN(0, xmin) + xGridIntrv / 10.;
  gend = B3DMAX(nxwarp, xmax) - xGridIntrv / 10.;
  nxgr = B3DMIN(ixgdim, (int)ceil((gend - gstr) / xGridIntrv) + 1);

  /* Get ideal new starting point and then shift to nearest point that keeps points
     on the same grid - this stabilizes the grid against changes in amount of expansion */
  gstr = (gend + gstr) / 2. - xGridIntrv * (nxgr - 1) / 2.;
  *xGridStrt -= B3DNINT((*xGridStrt - gstr) / xGridIntrv) * xGridIntrv;
  return nxgr;
}
