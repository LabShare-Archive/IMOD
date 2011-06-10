/* 
 * Header file for wrapper to Clarkson hull Delaunay triangulation
 * $Id$ 
 */

#ifndef HULLWARP_H
#define HULLWARP_H

struct hullio {
  double *pointlist;
  int numberofpoints;
  int *trianglelist;
  int *neighborlist;
  int numberoftriangles;
  double heightBaseCrit;
  double areaFractionCrit;
  int minNumForPruning;
  int verbose;
};

int hull_triangulate(struct hullio *hio);

#endif

