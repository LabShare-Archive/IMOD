/*
 * taperatfill.c - Taper a slice at fill areas on edges
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 * Log at end of file
 */

#include <stdlib.h>
#include <math.h>
#include "b3dutil.h"
#include "mrcslice.h"
#include "imodconfig.h"

#ifdef F77FUNCAP
#define taperatfill TAPERATFILL
#else
#define taperatfill taperatfill_
#endif

/*!
 * Analyzes for fill areas at the edge of the slice [sl], finds the borders 
 * between actual image and fill areas, and tapers the image intensities down 
 * to the fill value over [ntaper] pixels.  If [inside] is 0 the image 
 * intensities at the edge are extended into the fill area, while if [inside] 
 * is 1 then pixels inside the image are attenuated.  Returns -1 for memory
 * allocation errors.
 */
int sliceTaperAtFill(Islice *sl, int ntaper, int inside)
{
  struct pixdist {
    short int dx, dy;
    int edgeind;
  };
  struct edgelist {
    int x, y;
  };
  #define MAX_TAPER 256
  Ival val, val2, val3, val4;
  float fracs[MAX_TAPER], fillpart[MAX_TAPER];
  float fillval, lastval;
  int len, longest;
  int i, j, k, ix, iy, xnext, ynext, xp, yp, longix, longiy, lastix, lastiy;
  int dir, tapdir, col, row, ncol, ind, xstart, ystart, found, dirstart;
  int dist, distmin, distmax, minedge, lastout;
  struct pixdist *plist;
  struct edgelist *elist;
  int pllimit;
  int plsize = 0;
  int elsize = 0;
  int ellimit;
  int tapersq;
  unsigned char *bitmap;
  int xsize = sl->xsize;
  int ysize = sl->ysize;
  int bmsize = (xsize * ysize + 7) / 8;
  int dxout[4] = {0, 1, 0, -1};
  int dyout[4] = {-1, 0, 1, 0};
  int dxnext[4] = {1, 0, -1, 0};
  int dynext[4] = {0, 1, 0, -1};
  int plist_chunk, elist_chunk;

  ntaper = B3DMIN(MAX_TAPER, ntaper);
  tapersq = (ntaper + 1) * (ntaper + 1);
  inside = inside != 0 ? 1 : 0;
  elist_chunk =  (sl->xsize + sl->ysize) / 5 + 1;
  plist_chunk = ntaper * elist_chunk;
  pllimit = plist_chunk;
  ellimit = elist_chunk;

  /* find longest string of repeated values along the edge */
  longest = 0;
  for (iy = 0; iy < ysize; iy += ysize - 1) {
    len = 0;
    sliceGetVal(sl, 0, iy, val);
    lastval = val[0];
    lastix = 0;
    lastiy = iy;
    for (ix = 1; ix < xsize; ix++) {
      sliceGetVal(sl, ix, iy, val);
      if (val[0] == lastval) {
        /* same as last, add to count, see if this is a new
           best count*/
        len++;
        if (len > longest) {
          longest = len;
          fillval = lastval;
          longix = lastix;
          longiy = lastiy;
          dir = iy ? 2 : 0;
        }
      } else {
        /* different, reset count and set new lastval */
        len = 0;
        lastval = val[0];
        lastix = ix;
      }
    }
  }         
  for (ix = 0; ix < xsize; ix += xsize - 1) {
    len = 0;
    sliceGetVal(sl, ix, 0, val);
    lastval = val[0];
    lastix = ix;
    lastiy = 0;
    for (iy = 1; iy < ysize; iy++) {
      sliceGetVal(sl, ix, iy, val);
      if (val[0] == lastval) {
        /* same as last, add to count, see if this is a new
           best count */
        len++;
        if (len > longest) {
          longest = len;
          fillval = lastval;
          longix = lastix;
          longiy = lastiy;
          dir = ix ? 1 : 3;
        }
      } else {
        /* different, reset count and set new lastval */
        len = 0;
        lastval = val[0];
        lastiy = iy;
      }
    }
  }         

  /* If length below a criterion (what?) , return without error */
  if (longest < 10)
    return 0;

  /* set the slice mean so that sliceGetValue will return this outside the
     edges */
  sl->mean = fillval;

  /* Start at the middle of that long interval and walk in until non-fill
     pixel is found */
  ix = longix + dxnext[dir % 2] * longest / 2;
  iy = longiy + dynext[dir % 2] * longest / 2;
  found = 0;
  /*printf("\nlongix %d longiy %d dir %d longest %d ix %d iy %d\n", longix,
    longiy, dir, longest, ix, iy);*/
  while (!found) {
    ix -= dxout[dir];
    iy -= dyout[dir];
    if (ix < 0 || ix >= xsize || iy < 0 || iy >= ysize)
      break;
    sliceGetVal(sl, ix, iy, val);
    if (val[0] != fillval)
      found = 1;
  }
  
  if (!found)
    return 0;

  /* Get initial chunk of pixel list and bitmap */
  plist = (struct pixdist *)malloc(plist_chunk * sizeof(struct pixdist));
  elist = (struct edgelist *)malloc(elist_chunk * sizeof(struct pixdist));
  if (plist && elist)
    bitmap = (unsigned char *)malloc(bmsize);
  if (!plist || !bitmap || !elist) {
    if (plist)
      free(plist);
    if (elist)
      free(elist);
    return (-1);
  }

  /* clear bitmap */
  for (i = 0; i < bmsize; i++)
    bitmap[i] = 0;

  dirstart = dir;
  xstart = ix;
  ystart = iy;
  tapdir = 1 - 2 * inside;
  elist[0].x = ix;
  elist[0].y = iy;
  elsize = 1;
  lastout = 1;
  /* printf("xstart %d ystart  %d\n", xstart, ystart);*/

  do {
    ncol = 1;
    xnext = ix + dxout[dir] + dxnext[dir];
    ynext = iy + dyout[dir] + dynext[dir];
    sliceGetVal(sl, xnext, ynext, val);
    ind = 1;
    if (val[0] != fillval) {
      /* case 1: inside corner */
      ix = xnext;
      iy = ynext;
      dir = (dir + 3) % 4;
      if (inside)
        ncol = ntaper + 1;
    } else {
      xnext = ix + dxnext[dir];
      ynext = iy + dynext[dir];
      sliceGetVal(sl, xnext, ynext, val);
      if (val[0] != fillval) {
        /* case 2: straight edge to next pixel */
        ix = xnext;
        iy = ynext;
      } else {
        /* case 3: outside corner, pixel stays the same */
        dir = (dir + 1) % 4;
        if (!inside)
          ncol = ntaper + 1;
        ind = lastout;
      }
    }
    /* printf ("%d %d %d %d %d\n", xnext, ynext, ix, iy, dir); */

    /* If outside pixel is outside the data, nothing to add to lists */
    xp = ix + dxout[dir];
    yp = iy + dyout[dir];
    lastout = (xp < 0 || xp >= xsize || yp < 0 || yp >= ysize) ? 1 : 0;
    if (lastout)
      continue;

    /* Add a new point to edge list */
    if (ind) {
      if (elsize >= ellimit) {
        ellimit += elist_chunk;
        elist = (struct edgelist *) realloc
          (elist, ellimit * sizeof(struct edgelist));
        if (!elist) {
          free(plist);
          free(bitmap);
          return (-1);
        }
      }
      elist[elsize].x = ix;
      elist[elsize++].y = iy;
    }
      
    /* Loop on all the pixels to mark */
    for (col = 0; col < ncol; col++) {
      for (row = 1 - inside; row <= ntaper - inside; row++) {
        xp = ix + tapdir * row * dxout[dir] - col * dxnext[dir];
        yp = iy + tapdir * row * dyout[dir] - col * dynext[dir];

        /* skip if pixel outside area */
        if (xp < 0 || xp >= xsize || yp < 0 || yp >= ysize)
          continue;

        /* skip marking outside pixels for inside taper or
           inside pixels for outside taper.  Notice not good assumption that
           anything with fill value is "outside" */
        sliceGetVal(sl, xp, yp, val);
        if ((inside && val[0] == fillval) || 
            (!inside && val[0] != fillval))
          continue;

        /* If pixel is new, mark in bitmap and make a new entry on the list */
        /* Eliminate keeping track of distance and looking pixel up 10/14/08 */
        ind = xsize * yp + xp;
        if (!(bitmap[ind / 8] & (1 << (ind % 8)))) {

          /* But first insist that the pixel have an outside neighbor if out */
          if (!inside) {
            sliceGetVal(sl, xp + 1, yp, val);
            sliceGetVal(sl, xp, yp + 1, val2);
            sliceGetVal(sl, xp - 1, yp, val3);
            sliceGetVal(sl, xp, yp - 1, val4);
            if (val[0] != fillval && val2[0] != fillval && val3[0] != fillval
                && val4[0] != fillval)
              continue;
          }

          bitmap[ind / 8] |= (1 << (ind % 8));
          if (plsize >= pllimit) {
            pllimit += plist_chunk;
            plist = (struct pixdist *) realloc
              (plist, pllimit * sizeof(struct pixdist));
            if (!plist) {
              free (bitmap);
              free(elist);
              return (-1);
            }
          }
          plist[plsize].edgeind = elsize - 1;
          plist[plsize].dx = xp - ix;
          plist[plsize++].dy = yp - iy;
        }
      }
    }

  } while (ix != xstart || iy != ystart || dir != dirstart);

  /* make tables of fractions and amounts to add of fillval */
  for (i = 1; i <= ntaper; i++) {
    dist = inside ? i : ntaper + 1 - i;
    fracs[i] = (float)dist / (ntaper + 1);
    fillpart[i] = (1. - fracs[i]) * fillval;
  }

  /* Process the pixels on the list */
  for (i = 0; i < plsize; i++) {

    /* Go in both directions from starting index looking for closest distance*/
    distmin = plist[i].dx * plist[i].dx + plist[i].dy * plist[i].dy;
    minedge = plist[i].edgeind;
    distmax = 1.5 * B3DMAX(distmin, tapersq);
    ix = elist[minedge].x + plist[i].dx;
    iy = elist[minedge].y + plist[i].dy;
    /* printf("%d,%d  start dist %d to %d,%d", ix,iy,distmin,elist[minedge].x,
       elist[minedge].y); */
    for (dir = -1; dir <= 1; dir += 2) {
      k = plist[i].edgeind;
      for (j = 0; j < elsize; j++) {
        k = (k + dir + elsize) % elsize;
        xp = ix - elist[k].x;
        yp = iy - elist[k].y;
        dist = xp * xp + yp * yp;
        if (dist < distmin) {
          distmin = dist;
          minedge = k;
        }
        if (dist > distmax)
          break;
      }
    }

    ind = sqrt((double)distmin) + inside;
    /* printf("   end dist %d to %d,%d", distmin,elist[minedge].x,
       elist[minedge].y); */
    
    if (ind > ntaper) {
      /* printf("\n"); */
      continue;
    }
    if (inside) {
      xp = ix;
      yp = iy;
    } else {
      xp = elist[minedge].x;
      yp = elist[minedge].y;
    }
    sliceGetVal(sl, xp, yp, val);
    /*  val[0] = fracs[plist[i].dist] * val[0] + fillpart[plist[i].dist]; */
    val[0] = fracs[ind] * val[0] + fillpart[ind];
    slicePutVal(sl, ix, iy, val);
    /* printf("  put val %f\n", val[0]); */
  }

  free(plist);
  free(bitmap);
  return 0;
}

/*! 
 * Fortran wrapper to @sliceTaperAtFill for tapering a real*4 image in [array]
 * of size [nx] by [ny].  Returns -1 for memory error.
 */
int taperatfill(float *array, int *nx, int *ny, int *ntaper, int *inside)
{
  Islice slice;
  sliceInit(&slice, *nx, *ny, SLICE_MODE_FLOAT, array);
  return sliceTaperAtFill(&slice, *ntaper, *inside);
}

/*

$Log$
Revision 1.4  2008/11/12 05:09:36  mast
Add nath include for sqrt

Revision 1.3  2008/10/17 14:19:32  mast
change include to use min/max macros

Revision 1.2  2008/10/16 22:49:13  mast
Kept separate list of edge points to eliminate full search in point list for
a point each time it's found; speeded up a lot for big images.

Revision 1.1  2008/06/24 04:43:51  mast
Moved to libcfshr


*/
