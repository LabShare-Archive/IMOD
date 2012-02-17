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
#include <math.h>
#include "hullwrap.h"
#include "hull.h"

static double *sPointArray;
static int sNumPoints;
static int sNextPoint;
static int sNumTri;
static int sListInd;
static int sNumPrune;
static int sPruneCrit;
static double sTotalArea;
static struct hullio *sHio;

FILE *DFILE;

static int site_numm(site psite);
static site get_next_site(void);
static void *saveTriangle(simplex *s, void *funcp);
static void *markOutsideGetArea(simplex *s, void *funcp);
static void *markForPruning(simplex *s, void *funcp);
static void *setTriangleNumber(simplex *s, void *funcp);

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
  sNumPrune = 0;
  sTotalArea = 0.;
  sPruneCrit = -1;
  visit_hull(root, markOutsideGetArea);
         
  /* Prune triangles at edge if there are enough and criteria are set */
  if (sNumTri >= hio->minNumForPruning && hio->areaFractionCrit > 0. && 
      hio->heightBaseCrit > 0.) {
    visit_hull(root, markForPruning);
    sPruneCrit = 0;
  }

  sNumTri = 0;
  visit_hull(root, setTriangleNumber);
  if (hio->verbose)
    fprintf(stderr, "%d triangles,  %d pruned at edge, total areax2 %.0f\n", 
            sNumTri, sNumPrune, sTotalArea);

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
  visit_hull(root, saveTriangle);

  free_hull_storage();
  return 0;
}

/* Return the point number given its address */
static int site_numm(site psite) 
{
  int j;
  if (psite == hull_infinity)
    return -1;
  if (!psite)
    return -2;
  j = (psite - sPointArray) / 2;
  if (j < 0 || j >= sNumPoints)
    return -3;
  if (sHio->verbose > 1)
    fprintf(stderr, "site_numm returning %d for %p\n", j, (void *)psite);
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



/* Mark triangles that connect hull to infinity, and compute a total area 
   Sadly, it did not work to save an area as a structure member of simplex.  
   After storing it here, it was zero on next visit */
static void *markOutsideGetArea(simplex *s, void *funcp)
{
  int i;
  double dx10, dx20, dy10, dy20;
  if (!s)
    return NULL;
  s->mark = -2;
  for (i = 0; i < 3; i++)
    if (site_numm(s->neigh[i].vert) < 0)
      return NULL;

  s->mark = 0;
  dx10 = s->neigh[1].vert[0] - s->neigh[0].vert[0];
  dx20 = s->neigh[2].vert[0] - s->neigh[0].vert[0];
  dy10 = s->neigh[1].vert[1] - s->neigh[0].vert[1];
  dy20 = s->neigh[2].vert[1] - s->neigh[0].vert[1];
  sTotalArea += fabs(dx10 * dy20 - dx20 * dy10);
  sNumTri++;
  return NULL;
}

/* Check triangles at the hull edge for adequate height-base ratio or fraction of
   total area */
static void *markForPruning(simplex *s, void *funcp) {
  int i, ip1, ip2;
  simplex *sn;
  double dx10, dx20, dy10, dy20, areax2, dxb, dyb, basesq, base;
  if (!s || !s->mark)
    return NULL;
  for (i = 0; i < 3; i++) {
    if (site_numm(s->neigh[i].vert) < 0) {
      sn = s->neigh[i].simp;
      if (sn) {
        ip1 = (i+1) % 3;
        ip2 = (i+2) % 3;
        dxb = s->neigh[ip1].vert[0] - s->neigh[ip2].vert[0];
        dyb = s->neigh[ip1].vert[1] - s->neigh[ip2].vert[1];
        basesq = dxb * dxb + dyb * dyb;
        dx10 = sn->neigh[1].vert[0] - sn->neigh[0].vert[0];
        dx20 = sn->neigh[2].vert[0] - sn->neigh[0].vert[0];
        dy10 = sn->neigh[1].vert[1] - sn->neigh[0].vert[1];
        dy20 = sn->neigh[2].vert[1] - sn->neigh[0].vert[1];
        areax2 = fabs(dx10 * dy20 - dx20 * dy10);
        if (sHio->verbose) {
          base = sqrt(basesq);
          fprintf(stderr, "edge %.0f,%.0f to %.0f,%.0f, base %.1f, area %.0f, hgt %.1f"
                  "  h/b %.3f",
                  s->neigh[ip1].vert[0],s->neigh[ip1].vert[1], s->neigh[ip2].vert[0],
                  s->neigh[ip2].vert[1], base, areax2, areax2 / base, areax2 / basesq);
        }
        if (areax2 / basesq < sHio->heightBaseCrit && 
            areax2 / sTotalArea < sHio->areaFractionCrit) {
          sn->mark = -1;
          sNumPrune++;
          if (sHio->verbose)
            fprintf(stderr, " pruned");
        }
        if (sHio->verbose)
          puts(" ");
      }
      return NULL;
    }
  }
  return NULL;
}

/* Count the retained triangles, and set mark value to the triangle number */
static void *setTriangleNumber(simplex *s, void *funcp)
{
  if (!s || s->mark < sPruneCrit)
    return NULL;
  s->mark = sNumTri++;
  return NULL;
}

/* Save each triangle's vertices and neighbors to arrays in counterclockwise order */
static void *saveTriangle(simplex *s, void *funcp)
{
  int i, ist = 0, iend = 2, idir = 1;
  double dx10, dx20, dy10, dy20;
  if (!s || s->mark < sPruneCrit)
    return NULL;

  /* Test for counterclockwise triangle */
  dx10 = s->neigh[1].vert[0] - s->neigh[0].vert[0];
  dx20 = s->neigh[2].vert[0] - s->neigh[0].vert[0];
  dy10 = s->neigh[1].vert[1] - s->neigh[0].vert[1];
  dy20 = s->neigh[2].vert[1] - s->neigh[0].vert[1];
  if  (dx10 * dy20 - dx20 *dy10 < 0.) {
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

