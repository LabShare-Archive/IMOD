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
  int verbose;
};

int hull_triangulate(struct hullio *hio);

#endif

