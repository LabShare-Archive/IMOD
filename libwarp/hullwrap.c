/*  hullwarp.c - wrapper to Clarkson hull Delaunay triangulation
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include "hullwrap.h"
#include "hull.h"

static double *sPointArray;
static int sNumPoints;
static int sNextPoint;
static int sNumTri;
static int sListInd;
static struct hullio *sHio;

FILE *DFILE;

static int site_numm(site p);
static site get_next_site(void);
static void *save_triangle(simplex *s, void *p);
static void *mark_triangle(simplex *s, void *p);

/* The external call to do the triangulation */
int hull_triangulate(struct hullio *hio)
{
  simplex *root;

  DFILE = stderr;
  sPointArray = hio->pointlist;
  sNumPoints = hio->numberofpoints;
  sNextPoint = 0;
  sHio = hio;
  if (hio->verbose)
    fprintf(stderr, "Number of points %d\n", sNumPoints);

  root = build_convex_hull(get_next_site, site_numm, 2, 1);

  sNumTri = 0;
  visit_hull(root, mark_triangle);

  if (hio->verbose)
    fprintf(stderr, "%d triangles\n", sNumTri);

  /* Allocate output arrays */
  hio->numberoftriangles = sNumTri;
  hio->trianglelist = NULL;
  hio->neighborlist = NULL;
  if (!sNumTri) {
    free_hull_storage();
    return 0;
  }

  hio->trianglelist = (int *)malloc(3 * sNumTri * sizeof(int));
  hio->neighborlist = (int *)malloc(3 * sNumTri * sizeof(int));
  if (!hio->trianglelist || !hio->neighborlist) {
    free_hull_storage();
    return 1;
  }
  
  /* Build the lists */
  sListInd = 0;
  visit_hull(root, save_triangle);

  free_hull_storage();
  return 0;
}

/* Return the point number given its address */
static int site_numm(site p) 
{
  int j;
  if (p == hull_infinity)
    return -1;
  if (!p)
    return -2;
  j = (p - sPointArray) / 2;
  if (j < 0 || j >= sNumPoints)
    return -3;
  if (sHio->verbose > 1)
    fprintf(stderr, "site_numm returning %d for %p\n", j, (void *)p);
  return j;
}

/* Return the next point in the array */
static site get_next_site(void)
{
  if (sNextPoint >= sNumPoints)
    return NULL;
  if (sHio->verbose > 1)
    fprintf(stderr, "get_next_site returning %d  %f %f\n", sNextPoint,
            sPointArray[2 * sNextPoint], sPointArray[2 * sNextPoint+1]);
  return &sPointArray[2 * sNextPoint++];
}

/* Count the triangles, skip ones that connect hull to infinity, and set mark value
 * to the triangle number */
static void *mark_triangle(simplex *s, void *p)
{
  int i;
  if (!s)
    return NULL;
  s->mark = -1;
  for (i = 0; i < 3; i++) 
    if (site_numm(s->neigh[i].vert) < 0)
      return NULL;
  s->mark = sNumTri++;
  return NULL;
}

/* Save each triangle's vertices and neighbors to arrays in counterclockwise order */
static void *save_triangle(simplex *s, void *p)
{
  int i, ist = 0, iend = 2, idir = 1;
  double x0, x1, x2, y0, y1, y2;
  if (!s || s->mark < 0)
    return NULL;

  /* Test for counterclockwise triangle */
  x0 = s->neigh[0].vert[0];
  x1 = s->neigh[1].vert[0];
  x2 = s->neigh[2].vert[0];
  y0 = s->neigh[0].vert[1];
  y1 = s->neigh[1].vert[1];
  y2 = s->neigh[2].vert[1];
  if  ((x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0) < 0.) {
    idir = -1;
    ist = 2;
    iend = 0;
  }

  /* Put the vertices and neighbors into arrays in the right order */
  for (i = ist; idir * (iend - i) >= 0; i += idir) {
    sHio->trianglelist[sListInd] = site_numm(s->neigh[i].vert);
    sHio->neighborlist[sListInd++] = s->neigh[i].simp->mark;
  }
  return NULL;
}

