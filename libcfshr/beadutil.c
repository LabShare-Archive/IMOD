/*
 * beadutil.c - Utility functions for bead finding
 *
 * Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 */

#include <math.h>
#include "b3dutil.h"
#include "imodconfig.h"

#ifdef F77FUNCAP
#define makemodelbead MAKEMODELBEAD
#else
#define makemodelbead makemodelbead_
#endif

/*!
 * Makes a model bead of radius [beadSize] in [array] with dimensions [boxSize]
 * by [boxSize].
 */
void makeModelBead(int boxSize, float beadSize, float *array)
{
  int icen, ix, iy, ixd, iyd, ndiv = 10;
  float fcen, radsq, distsq, height, delx, dely; 
  
  icen = boxSize / 2;
  fcen = (boxSize - 1. / ndiv) / 2.;
  radsq = beadSize * beadSize / 4.;
  for (iy = 0; iy <= icen; iy++) {
    for (ix = 0; ix <= icen; ix++) {
      height = 0.;
      for (iyd = 0; iyd < ndiv; iyd++) {
        for (ixd = 0; ixd < ndiv; ixd++) {
          dely = iy + ((float)iyd) / ndiv - fcen;
          delx = ix + ((float)ixd) / ndiv - fcen;
          distsq = delx * delx + dely * dely;
          if (distsq < radsq)
            height += (float)sqrt(radsq - distsq);
        }
      }
      height /= -ndiv * ndiv;
      array[ix + iy * boxSize] = height;
      array[boxSize - 1 - ix + (boxSize - 1 - iy) * boxSize] = height;
      array[ix + (boxSize - 1 - iy) * boxSize] = height;
      array[boxSize - 1 - ix + iy * boxSize] = height;
    }
  }
}

/*! Fortran wrapper for @@makeModelbead@. */
void makemodelbead(int *boxSize, float *beadSize, float *array)
{
  makeModelBead(*boxSize, *beadSize, array);
}


/*!
 * Finds integral above background of a bead located at [xcen], [ycen] in 
 * [array], an image [nx] by [ny] with X dimension [nxdim].  [rCenter], 
 * [rInner], and [rOuter] are radii of the center and the inner and outer radii
 * of the background annulus.  Returns the center and annular means in 
 * [cenmean] and [annmean], and if [annPct] is supplied with a fractional 
 * percentile, returns the percentile value in [median], using [temp] as a 
 * temporary array.  [temp] is not used if [annPct] is 0.  The return value is
 * center minus annular mean.
 */
double beadIntegral(float *array, int nxdim, int nx, int ny, float rCenter,
                    float rInner, float rOuter, float xcen, float ycen,
                    float *cenmean, float *annmean, float *temp, float annPct, 
                    float *median)
{
  float xpcen = xcen - 0.5f;
  float ypcen = ycen - 0.5f;
  float rcensq = rCenter * rCenter;
  float rinsq = rInner * rInner;
  float routsq = rOuter * rOuter;

  int ncen = 0, nann = 0;
  double censum = 0., annsum = 0.;
  int ixcen = B3DNINT(xpcen);
  int iycen = B3DNINT(ypcen);
  int iradout = (int)(rOuter + 1.5);
  int ix, iy, idx;
  double dxsq;
  float dx, dy, radsq;

  for (iy = iycen - iradout; iy <= iycen + iradout; iy++) {
    if (iy < 0 || iy >= ny)
      continue;
    dy = iy - ypcen;
    dxsq = routsq - dy * dy;
    idx = (int)(sqrt(B3DMAX(0.,dxsq)) + 1.5);
    for (ix = ixcen - idx; ix <= ixcen + idx; ix++) {
      if (ix < 0 || ix >= nx)
        continue;
      dx = ix - xpcen;
      radsq = dy * dy + dx * dx;
      if (radsq <= rcensq) {
        ncen++;
        censum += array[ix + iy * nx];
      } else if (radsq <= routsq && radsq >= rinsq) {
        if (annPct > 0.)
          temp[nann] = array[ix + iy * nx];
        nann++;
        annsum += array[ix + iy * nx];
      }
    }
    
  }
  //printf("cen %.2f in %.2f out %.2f ncen %d censum %f nann %d annsum %f\n",
  //       rCenter, rInner, rOuter, ncen, censum, nann, annsum);
  if (!nann || !ncen)
    return 0.;
  *annmean = annsum / nann;
  *cenmean = censum / ncen;
  if (annPct > 0.)
    *median = percentileFloat((int)(annPct * nann + 1.), temp, nann);
  return (censum / ncen - *annmean);
}
