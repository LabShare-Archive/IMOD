/*
 * insidecontour.c - Tests for point inside set of vertices
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 * Log at end of file
 */

#include "cfsemshare.h"
#include "imodconfig.h"

#ifdef F77FUNCAP
#define insidecontour INSIDECONTOUR
#else
#define insidecontour insidecontour_
#endif

/* Based on algorithm in "Computational Geometry in C", Joseph O'Rourke,
   1998, with modifications to speed up search for ray crossings.  First
   written in fortran then translated to C 
*/

/*!
 * Returns 1 if the point [x], [y] is inside or on the polygon (contour) whose
 * vertices are in [ptX], [ptY], where [np] is the number of points, otherwise
 * returns 0.
 */
int InsideContour(float *ptX, float *ptY, int np, float x, float y)
{
  int rstrad, lstrad;

  int nrcross=0;
  int nlcross=0;
  float xp, yp, xc, yc, xcross;
  int j, jl;

  if (np <= 0)
    return 0;
  yp = ptY[np - 1];
  j = 0;
  while(j < np) {
    if(yp < y) {
          
      /* if last point below y, search for first that is not below */
           
      while(j < np && ptY[j] < y) 
        j++;

    } else if (yp > y) {

      /* or if last point above y, search for first that is not 
         above */

      while(j < np && ptY[j] > y)
        j++;
    }

    if (j < np) {
      jl=j-1;
      if(jl < 0)
        jl=np - 1;
      xp=ptX[jl];
      yp=ptY[jl];
      xc=ptX[j];
      yc=ptY[j];
        
      /*  return if point is a vertex */

      if (x == xc && y == yc)
        return 1;
        
      /* does edge straddle the ray to the right or the left? */

      rstrad=(yc > y ? 1 : 0) != (yp > y ? 1 : 0);
      lstrad=(yc < y ? 1 : 0) != (yp < y ? 1 : 0);
      if(lstrad || rstrad) {
          
        /* if so, compute the crossing of the ray, add up crossings */

        xcross=xp+(y-yp)*(xc-xp)/(yc-yp);
        if(rstrad && (xcross > x))
          nrcross++;
        if(lstrad && (xcross < x))
          nlcross++;
      }
      yp=yc;
    }
    j++;
  }
  
  /*  if left and right crossings don't match, it's on an edge
      otherwise, inside iff crossings are odd */
  if(nrcross % 2 !=  nlcross % 2)
    return 1;
  return((nrcross % 2) > 0 ? 1 : 0);
}

/*!
 * Fortran wrapper to @InsideContour .  But it is more convenient to call
 * ^   logical function inside(ptX, ptY, np, x, y)
 */
int insidecontour(float *ptX, float *ptY, int *np, float *x, float *y)
{
  return (InsideContour(ptX, ptY, *np, *x, *y));
}

/*

$Log$
Revision 1.3  2010/06/21 16:27:49  mast
fix log

Revision 1.2  2007/10/12 04:14:51  mast
Documentation fix

Revision 1.1  2007/10/01 15:26:09  mast
*** empty log message ***

*/
