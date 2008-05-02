/* 
 *  remesh.c -- remakes mesh with efficient storage and adds normals
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

Log at end of file
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mkmesh.h"

static int ptcompare(const void *v1, const void *v2);


static int newPolyNorm = 1;
void imeshSetNewPolyNorm(int value)
{
  newPolyNorm = value;
}

#define XDIV 10

/*!
 * Remakes meshes into one mesh per surface and time to store data more 
 * efficiently, and calculates normals for all vertices.  The array of meshes
 * is in [meshes], and [size] specifies the number of meshes.  Only meshes
 * matching the resolution given by [resol] will be processed; others will be
 * copied to the output.  The normals are scaled by the values in [scale].
 * THIS FUNCTION DELETES THE INPUT MESH BEFORE RETURNING
 * Returns NULL for error.
 */
/*
 * DNM: modified to take both POLY meshes and POLYNORM meshes
 * Note: the division by X for sorting of points was implemented before qsort
 * was used.  It made a huge difference then but only a small one with qsort.
 */
Imesh *imeshReMeshNormal(Imesh *meshes, int *size, Ipoint *scale, int resol)
{
  Imesh *nm;
  Imesh *mesh, *remesh = NULL;
  int m,l,pt;
  Ipoint npt;
  int surf, surfsize = 0;
  int time, timesize = 0;
  int meshsize = 0;
  unsigned int msize = *size;
  int linc, l1, l2, vBase, nAdd, lastPolyNorm;
  char *gotit;
  int ivt, indvert;
  int zmin, zmax, intz, dup;
  int *numatz, *cumatz;
  Ipoint **vecatz;
  Ipoint *iptz;
  float vecx, vecy;
  float xmin, xmax, delx;
  int indx, tablesize, maxlist;
  int ptmp[3];
  int outCode;
    
  if (!msize)
    return NULL;

  for (m = 0; m < msize; m++) {
    /* count only the meshes where the resolution value matches */
    if (imeshResol(meshes[m].flag) == resol) {
      if (meshes[m].surf > surfsize)
        surfsize = meshes[m].surf;
      if (meshes[m].time > timesize)
        timesize = meshes[m].time;
      if (ilistSize(meshes[m].store))
        newPolyNorm = 1;
    } else {
      /* otherwise, move the mesh to the output mesh */
      nm = &meshes[m];
      remesh = imodel_mesh_add(nm, remesh, &meshsize);
      nm->vert = NULL;
      nm->list = NULL;
      nm->vsize = 0;
      nm->lsize = 0;
      nm->store = NULL;
    }
  }
  surfsize++;
  timesize++;
  outCode = newPolyNorm ? IMOD_MESH_BGNPOLYNORM2 : IMOD_MESH_BGNPOLYNORM;
        
  skin_report_time("Starting ReMesh");
    
  /*    lsize = 0;
        l1 = 0;
        l2 = 0;
        for (m = 0; m < msize; m++) {
        lsize += meshes[m].vsize * 12 + meshes[m].lsize * 4 + 32;
        l1 += meshes[m].vsize;
        l2 += meshes[m].lsize;
        }
        printf("Incoming mesh vertices = %d, indexes = %d, memory = %d\n",
        l1, l2, lsize); */

  for (surf = 0; surf < surfsize; surf++) {
    /*if (surfsize > 1) {
      printf("\rnormals surface %d", surf);
      fflush(stdout);
      }*/
    for (time = 0; time < timesize; time++) {
      nm = imodMeshNew();
      if (!nm) {
        *size = 0;
        return(NULL);
      }

      /* DNM 6/20/01: in the course of adding time support, had it transfer
         surface number to the mesh also */
      maxlist = 0;
      nm->surf = surf;
      nm->time = time;
        
      /* Find min and max Z and X values */
      zmax = -10000000;
      zmin = 10000000;
      xmax = -1.e30;
      xmin = 1.e30;
      for (m = 0; m < *size; m++) {
        if (meshes[m].surf == surf && meshes[m].time == time) {
          mesh = &(meshes[m]);
          for (l = 0; l < mesh->lsize; l++) {
            linc = 1;
            vBase = 0;
            if (mesh->list[l] == IMOD_MESH_BGNPOLY || 
                imodMeshPolyNormFactors(mesh->list[l], &linc, &vBase, &nAdd)) {
              l++;
              while (mesh->list[l] != IMOD_MESH_ENDPOLY) {
                for (ivt = 0; ivt < 3; ivt++) {
                  intz = (int)floor((double)
                                    mesh->vert[mesh->list[l + vBase]].z);
                  if (intz > zmax)
                    zmax = intz;
                  if (intz < zmin)
                    zmin = intz;
                  vecx = mesh->vert[mesh->list[l + vBase]].x;
                  if (vecx > xmax)
                    xmax = vecx;
                  if (vecx < xmin)
                    xmin = vecx;
                  l += linc;                                 
                }
              }
            }
          }
        }
      }

      if (zmax < zmin)
        continue;

      tablesize = XDIV * (zmax + 1 - zmin);
      delx = (xmax + 1. - xmin) / XDIV;
      /* Count the number of each Z and fractional X value */
      numatz = (int *) malloc(sizeof(int) * tablesize);
      cumatz = (int *) malloc(sizeof(int) * (tablesize + 1));
      vecatz = (Ipoint **) malloc(sizeof(Ipoint*) * tablesize);

      if (!cumatz || !numatz || !vecatz) {
        *size = 0;
        return(NULL);
      }
      for (l = 0; l < tablesize; l++)
        numatz[l] = 0;
      for (m = 0; m < *size; m++) {
        if (meshes[m].surf == surf && meshes[m].time == time) {
          mesh = &(meshes[m]);
          for (l = 0; l < mesh->lsize; l++) {
            linc = 1;
            vBase = 0;
            if (mesh->list[l] == IMOD_MESH_BGNPOLY || 
                imodMeshPolyNormFactors(mesh->list[l], &linc, &vBase, &nAdd)) {
              l++;
              while (mesh->list[l] != IMOD_MESH_ENDPOLY) {
                for (ivt = 0; ivt < 3; ivt++) {
                  intz = (int)floor((double)
                                    mesh->vert[mesh->list[l + vBase]].z);
                  vecx = mesh->vert[mesh->list[l + vBase]].x;
                  indx = (vecx - xmin) / delx;
                  numatz[indx + XDIV *(intz - zmin)]++;
                  l += linc;
                }
              }
            }
          }
        }
      }

      /* allocate arrays for the points, rezero the counters */
      for (l = 0; l < tablesize; l++) {
        if (numatz[l]) {
          vecatz[l] = (Ipoint *)malloc(sizeof(Ipoint) * numatz[l]);
          if (!vecatz[l]) {
            *size = 0;
            return(NULL);
          }
          numatz[l] = 0;
        }
      }



      /* Put points into arrays, eliminating duplicates as we go */
      for (m = 0; m < *size; m++) {
        mesh = &(meshes[m]);
        if (mesh->surf == surf && mesh->time == time && mesh->vsize > 0) {
          gotit = (char *)malloc(mesh->vsize);
          if (!gotit) {
            *size = 0;
            return(NULL);
          }
          for (l = 0; l < mesh->vsize; l++)
            gotit[l] = 0;

          for (l = 0; l < mesh->lsize; l++) {
            linc = 1;
            vBase = 0;
            if (mesh->list[l] == IMOD_MESH_BGNPOLY || 
                imodMeshPolyNormFactors(mesh->list[l], &linc, &vBase, &nAdd)) {
              l++;
              while (mesh->list[l] != IMOD_MESH_ENDPOLY) {
                for (ivt = 0; ivt < 3; ivt++) {
                  indvert = mesh->list[l + vBase];
                  if (!gotit[indvert]) {
                    gotit[indvert] = 1;
                    intz = (int)floor((double)mesh->vert[indvert].z);
                    vecx = mesh->vert[indvert].x;
                    vecy = mesh->vert[indvert].y;
                    indx = (vecx - xmin) / delx;
                    intz = indx + XDIV * (intz - zmin);
                    pt = 0;
                    dup = 0;
                    iptz = vecatz[intz];
                    /* Take out this search and use
                       qsort below */
                    /*  while (pt < numatz[intz] && !dup) {
                        if (iptz[pt].x  == vecx &&
                        iptz[pt].y  == vecy)
                        dup = 1;
                        pt++;
                        }
                        if (!dup) { */
                    iptz[numatz[intz]] = mesh->vert[indvert];
                    numatz[intz]++;
                    /* } */
                  }
                  l += linc;
                }
              }
            }
          }
          free (gotit);
        }
      }

      /* Sort the vertices by X/Y, eliminate duplicates */
      for (l = 0; l < tablesize; l++)
        if (numatz[l]) {
          iptz = vecatz[l];
          qsort(iptz, numatz[l], sizeof(Ipoint), ptcompare);
          l2 = 1;
          for (l1 = 1; l1 < numatz[l]; l1++) {
            if (ptcompare(&iptz[l2 - 1], &iptz[l1]))
              iptz[l2++] = iptz[l1];
          }
          numatz[l] = l2;
        }

      cumatz[0] = 0;
      for (l = 0; l < tablesize; l++)
        cumatz[l + 1] = cumatz[l] + 2 * numatz[l];
        
      /* copy points to mesh data, zero out normal space also */
      nm->vert = (Ipoint *)malloc( sizeof(Ipoint) * cumatz[tablesize]);
      nm->vsize = cumatz[tablesize];
      iptz = nm->vert;
      for (l = 0; l < tablesize; l++) {
        for (pt = 0; pt < numatz[l]; pt++) {
          *iptz++ = vecatz[l][pt];
          iptz->x = 0.;
          iptz->y = 0.;
          iptz->z = 0.;
          iptz++;
        }
      }

      /*        printf ("vertices & normals %d\n",nm->vsize); */
      /*        printf("Finding Indices\n");  */
      skin_report_time("Built tables");

      /* add normals to mesh */
      for (m = 0; m < msize; m++) {
        if (meshes[m].surf != surf || meshes[m].time != time)
          continue;
        mesh = &(meshes[m]);
        if (!mesh->lsize)
          continue;
        chunkMeshAddIndex(nm, outCode, &maxlist);
        lastPolyNorm = -1;
        for (l = 0; l < mesh->lsize; l++) {
          linc = 1;
          vBase = 0;
          if (mesh->list[l] == IMOD_MESH_BGNPOLY || 
              imodMeshPolyNormFactors(mesh->list[l], &linc, &vBase, &nAdd)) {

            /* Preserve polynorms polygons: start a new polygon if this is a 
               polynorm or if the last one was */
            if ((lastPolyNorm >= 0 && 
                 imodMeshPolyNormFactors(mesh->list[l], &linc, &vBase, &nAdd))
                || lastPolyNorm > 0) {
              chunkMeshAddIndex(nm, IMOD_MESH_ENDPOLY, &maxlist);
              chunkMeshAddIndex(nm, outCode, &maxlist);
            }
            lastPolyNorm = imodMeshPolyNormFactors(mesh->list[l], &linc, 
                                                   &vBase, &nAdd);
            l++;
            while (mesh->list[l] != IMOD_MESH_ENDPOLY) {
              for (ivt = 0; ivt < 3; ivt++) {
                indvert = mesh->list[l + vBase];
                intz = (int)floor((double)mesh->vert[indvert].z);
                vecx = mesh->vert[indvert].x;
                /*  vecy = mesh->vert[mesh->list[l]].y; */
                indx = (vecx - xmin) / delx;
                intz = indx + XDIV * (intz - zmin);
                /* take this out; use bsearch instead */
                /* for (pt = cumatz[intz]; 
                   pt < cumatz[intz] + 2 * numatz[intz];
                   pt += 2) {
                   if (nm->vert[pt].x == vecx &&
                   nm->vert[pt].y == vecy)
                   break;
                   } */
                                
                iptz = (Ipoint *)bsearch(&mesh->vert[indvert],
                                         &nm->vert[cumatz[intz]], numatz[intz],
                                         2 * sizeof(Ipoint), ptcompare);
                if (iptz)
                  pt = iptz - nm->vert;
                else {
                  b3dError(stderr, "Failed to find vertex in reduced list\n");
                  pt = cumatz[intz];
                }
                ptmp[ivt]=pt;
                l += linc;
              }

              /* Save only if it is truly a triangle */
              if (ptmp[1] != ptmp[2] && ptmp[0] != ptmp[2] &&
                  ptmp[0] != ptmp[1]) {
                for (ivt = 0; ivt < 3; ivt++) {
                  if (!newPolyNorm)
                    chunkMeshAddIndex(nm, ptmp[ivt] + 1, &maxlist);
                  chunkMeshAddIndex(nm, ptmp[ivt], &maxlist);
                  istoreCopyContSurfItems(mesh->store, &nm->store, l + ivt - 3,
                                          nm->lsize - 1, 0);
                }
              }
            }
          }
        }
        chunkMeshAddIndex(nm, IMOD_MESH_ENDPOLY, &maxlist);
      }
        
      imodMeshAddIndex(nm, IMOD_MESH_END);

      /*        printf ("indices %d\n",nm->lsize); */
      /*        printf("Calculating Normals\n"); */
      skin_report_time("Found Indexes");
        
      /* calculate normals for all points. */
      mesh = nm;
      for (l = 0; l < mesh->lsize; l++) {
        if (imodMeshPolyNormFactors(mesh->list[l], &linc, &vBase, &nAdd)) {
          l++;
          while (mesh->list[l] != IMOD_MESH_ENDPOLY) {
            imeshNormal(&npt, 
                         &(mesh->vert[mesh->list[l + vBase + 2 * linc]]),
                         &(mesh->vert[mesh->list[l + vBase]]), 
                         &(mesh->vert[mesh->list[l + vBase + linc]]), 
                         scale);
            for (ivt = 0; ivt < 3; ivt++) {
              indvert = mesh->list[l] + nAdd;
              mesh->vert[indvert].x += npt.x;
              mesh->vert[indvert].y += npt.y;
              mesh->vert[indvert].z += npt.z;
              l += linc;
            }
          }
        }
      }
        
      for (l = 0; l < tablesize; l++)
        if (numatz[l])
          free(vecatz[l]);
      free(numatz);
      free(vecatz);
      free(cumatz);

        
      if (nm) {
        nm->flag |= (resol << IMESH_FLAG_RES_SHIFT);
        if ((nm->vsize) && (nm->lsize))
          remesh = imodel_mesh_add(nm, remesh, &meshsize);
            
        free(nm);
      }
    }
  }
  /*if (surfsize > 1)
    puts(" "); */
  *size = meshsize;

  imodMeshesDelete(meshes, msize);
  skin_report_time("Calculated Normals");

  return(remesh);
}

/*!
 * Computes a normalized normal in [n] from the three points [pi], [p2], and
 * [p3].  Scales the points by the scaling in [sp] first if [sp] is not NULL.
 */
void imeshNormal(Ipoint *n, Ipoint *p1, Ipoint *p2, Ipoint *p3,
                         Ipoint *sp)
{
  double dist, sdist;
  Ipoint v1, v2;
     
  v1.x = p3->x - p2->x;
  v1.y = p3->y - p2->y;
  v1.z = p3->z - p2->z;

  v2.x = p1->x - p2->x;
  v2.y = p1->y - p2->y;
  v2.z = p1->z - p2->z;
  if (sp) {
    v1.x *= sp->x;
    v1.y *= sp->y;
    v1.z *= sp->z;
    v2.x *= sp->x;
    v2.y *= sp->y;
    v2.z *= sp->z;
  }

  n->x = (v1.y * v2.z) - (v1.z * v2.y);
  n->y = (v1.z * v2.x) - (v1.x * v2.z);
  n->z = (v1.x * v2.y) - (v1.y * v2.x);

  /* now normalize n ; x^2 + y^2 + z^2 = 1 */
     
  sdist = (n->x * n->x) + (n->y * n->y) + (n->z * n->z);
  dist = sqrt(sdist);
  if (dist == 0.0) {
    n->x = 0;
    n->y = 0;
    n->z = -1;
  }
  else{
    dist = 1/dist;
    n->x *= dist;
    n->y *= dist;
    n->z *= dist;
  }
  return;     
}


static int ptcompare(const void *v1, const void *v2)
{
  Ipoint *pt1 = (Ipoint *)v1;
  Ipoint *pt2 = (Ipoint *)v2;
  if (pt1->x < pt2->x)
    return -1;
  if (pt1->x > pt2->x)
    return 1;
  if (pt1->y == pt2->y)
    return 0;
  if (pt1->y > pt2->y)
    return 1;
  return -1;
}


/* 
mkmesh.c got the big log from before the split
$Log$
Revision 1.2  2006/11/02 07:16:44  mast
Add documentation

Revision 1.1  2006/09/12 14:58:19  mast
Split up and made into new library


*/
